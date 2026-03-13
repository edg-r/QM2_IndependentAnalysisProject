library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(ggplot2)
library(stargazer)

options(scipen = 999)

caption_theme <- theme(
  plot.caption = element_text(hjust = 0, size = 9, lineheight = 1.1),
  plot.caption.position = "plot",
  plot.margin = margin(12, 18, 18, 12)
)

china_red <- "#DE2910"
china_red_dark <- "#A61E0A"
china_red_light <- "#F28B82"
china_red_deep <- "#7A1608"
democracy_blue <- "#1D4E89"
democracy_blue_deep <- "#0B2545"
democracy_blue_soft <- "#8FB7E8"
regime_gradient <- c(
  "Liberal democracy" = democracy_blue_deep,
  "Electoral democracy" = democracy_blue_soft,
  "Electoral autocracy" = china_red,
  "Closed autocracy" = china_red_deep
)

to_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", ".", "NA", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  names(sort(table(x), decreasing = TRUE))[1]
}

describe_vars <- function(data, vars) {
  bind_rows(lapply(vars, function(v) {
    x <- data[[v]]
    data.frame(
      variable = v,
      n = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  }))
}

compute_vif <- function(data, vars) {
  out <- lapply(vars, function(v) {
    rhs <- setdiff(vars, v)
    fml <- as.formula(paste(v, "~", paste(rhs, collapse = " + ")))
    r2 <- summary(lm(fml, data = data))$r.squared
    data.frame(variable = v, vif = 1 / (1 - r2))
  })
  bind_rows(out)
}

vcov_hc1 <- function(model) {
  X <- model.matrix(model)
  u <- resid(model)
  n <- nrow(X)
  k <- ncol(X)
  bread <- solve(crossprod(X))
  meat <- crossprod(X, diag(u^2, nrow = n) %*% X)
  (n / (n - k)) * bread %*% meat %*% bread
}

tidy_with_vcov <- function(model, vcov_mat, model_name) {
  est <- coef(model)
  se <- sqrt(diag(vcov_mat))
  stat <- est / se
  p_value <- 2 * pt(abs(stat), df = df.residual(model), lower.tail = FALSE)
  data.frame(
    term = names(est),
    estimate = unname(est),
    std.error = unname(se),
    statistic = unname(stat),
    p.value = unname(p_value),
    model = model_name,
    row.names = NULL
  )
}

breusch_pagan_test <- function(model) {
  e2 <- resid(model)^2
  aux <- lm(e2 ~ model.matrix(model)[, -1, drop = FALSE])
  stat <- length(e2) * summary(aux)$r.squared
  df <- ncol(model.matrix(model)) - 1
  p_value <- pchisq(stat, df = df, lower.tail = FALSE)
  data.frame(
    statistic = unname(stat),
    df = df,
    p_value = p_value
  )
}

cooks_distance_table <- function(model, data, model_name, top_n = 10) {
  cooks <- cooks.distance(model)
  out <- data.frame(
    observation = seq_along(cooks),
    entity = data$entity,
    cooks_distance = cooks,
    above_cutoff = cooks > (4 / length(cooks)),
    model = model_name
  )
  out[order(out$cooks_distance, decreasing = TRUE), ][seq_len(min(top_n, nrow(out))), ]
}

project_dir <- "."
aid_path <- file.path(project_dir, "chinese-aid-data-2000-2021.xlsx")
owid_path <- file.path(project_dir, "our-world-in-data-2013-2023.xlsx")
output_root_dir <- file.path(project_dir, "output")
output_dir <- file.path(output_root_dir, "country_cross_section")

if (!dir.exists(output_root_dir)) {
  dir.create(output_root_dir, recursive = TRUE)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

aid_raw <- read_excel(aid_path, sheet = "aid-data", col_types = "text")

aid_project_level <- aid_raw %>%
  transmute(
    entity = str_trim(entity),
    year = to_numeric(year),
    recommended_for_aggregates = str_trim(`Recommended For Aggregates`),
    adjusted_amount_usd2021 = to_numeric(`Adjusted Amount (Constant USD 2021)`),
    amount_usd2021 = to_numeric(`Amount (Constant USD 2021)`),
    flow_type_simplified = str_trim(`Flow Type Simplified`),
    intent = str_trim(Intent)
  ) %>%
  filter(!is.na(entity), !is.na(year), year >= 2013, year <= 2021) %>%
  filter(recommended_for_aggregates == "Yes")

aid_panel <- aid_project_level %>%
  group_by(entity, year) %>%
  summarize(
    china_aid_usd2021 = sum(adjusted_amount_usd2021, na.rm = TRUE),
    china_aid_usd2021_unadjusted = sum(amount_usd2021, na.rm = TRUE),
    china_project_count = n(),
    china_loan_share = safe_mean(flow_type_simplified == "Loan"),
    china_development_intent_share = safe_mean(intent == "Development"),
    .groups = "drop"
  ) %>%
  mutate(
    china_aid_usd2021 = ifelse(is.nan(china_aid_usd2021), NA_real_, china_aid_usd2021),
    china_aid_usd2021_unadjusted = ifelse(
      is.nan(china_aid_usd2021_unadjusted),
      NA_real_,
      china_aid_usd2021_unadjusted
    )
  )

sector_breakdown_tbl <- aid_project_level %>%
  mutate(
    sector_code = "All projects",
    sector_name = "All projects"
  ) %>%
  group_by(sector_code, sector_name) %>%
  summarize(
    total_aid_usd2021 = sum(adjusted_amount_usd2021, na.rm = TRUE),
    project_count = n(),
    funded_project_count = sum(!is.na(adjusted_amount_usd2021)),
    recipient_count = n_distinct(entity),
    avg_funded_project_size_usd2021 = ifelse(
      funded_project_count > 0,
      total_aid_usd2021 / funded_project_count,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(share_of_total_aid = 1)

write.csv(
  sector_breakdown_tbl,
  file.path(output_dir, "selection_model_country_sector_breakdown.csv"),
  row.names = FALSE
)

sector_top_recipients_tbl <- aid_project_level %>%
  group_by(entity) %>%
  summarize(
    total_aid_usd2021 = sum(adjusted_amount_usd2021, na.rm = TRUE),
    funded_project_count = sum(!is.na(adjusted_amount_usd2021)),
    .groups = "drop"
  ) %>%
  arrange(desc(total_aid_usd2021)) %>%
  mutate(rank_within_sector = row_number()) %>%
  slice_head(n = 10) %>%
  mutate(
    sector_code = "All projects",
    sector_name = "All projects"
  ) %>%
  select(sector_code, sector_name, entity, total_aid_usd2021, funded_project_count, rank_within_sector)

write.csv(
  sector_top_recipients_tbl,
  file.path(output_dir, "selection_model_country_sector_top_recipients.csv"),
  row.names = FALSE
)

owid_panel <- read_excel(owid_path, sheet = "our-world-in-data", col_types = "text") %>%
  transmute(
    entity = str_trim(entity),
    year = to_numeric(year),
    political_regime = to_numeric(`political-regime`),
    democracy_index = to_numeric(`democracy-index-eiu`),
    cpi = to_numeric(`ti-corruption-perception-index`),
    extreme_poverty = to_numeric(`share-of-population-in-extreme-poverty`),
    gini = to_numeric(`economic-inequality-gini-index`),
    gdp_pc = to_numeric(`gdp-per-capita-worldbank`)
  ) %>%
  filter(!is.na(entity), !is.na(year), year >= 2013, year <= 2021)

analysis_panel <- owid_panel %>%
  left_join(aid_panel, by = c("entity", "year")) %>%
  mutate(
    china_project_count = replace_na(china_project_count, 0L),
    china_aid_usd2021 = replace_na(china_aid_usd2021, 0),
    china_aid_usd2021_unadjusted = replace_na(china_aid_usd2021_unadjusted, 0),
    aid_any = as.integer(china_project_count > 0),
    log_china_aid = log1p(china_aid_usd2021),
    log_gdp_pc = ifelse(!is.na(gdp_pc) & gdp_pc > 0, log(gdp_pc), NA_real_),
    autocracy_score = 3 - political_regime,
    regime_label = case_when(
      political_regime == 0 ~ "Closed autocracy",
      political_regime == 1 ~ "Electoral autocracy",
      political_regime == 2 ~ "Electoral democracy",
      political_regime == 3 ~ "Liberal democracy",
      TRUE ~ NA_character_
    ),
    regime_family = case_when(
      political_regime %in% c(0, 1) ~ "Authoritarian",
      political_regime %in% c(2, 3) ~ "Democratic",
      TRUE ~ NA_character_
    )
  )

country_cross_section <- analysis_panel %>%
  group_by(entity) %>%
  summarize(
    years_in_sample = n(),
    years_with_aid = sum(aid_any, na.rm = TRUE),
    china_aid_usd2021 = mean(china_aid_usd2021, na.rm = TRUE),
    china_aid_usd2021_total = sum(china_aid_usd2021, na.rm = TRUE),
    china_project_count = mean(china_project_count, na.rm = TRUE),
    china_loan_share = safe_mean(china_loan_share),
    china_development_intent_share = safe_mean(china_development_intent_share),
    political_regime = safe_mean(political_regime),
    autocracy_score = safe_mean(autocracy_score),
    democracy_index = safe_mean(democracy_index),
    cpi = safe_mean(cpi),
    extreme_poverty = safe_mean(extreme_poverty),
    gini = safe_mean(gini),
    gdp_pc = safe_mean(gdp_pc),
    modal_regime = safe_mode(regime_label),
    modal_regime_family = safe_mode(regime_family),
    .groups = "drop"
  ) %>%
  mutate(
    log_china_aid = log1p(china_aid_usd2021),
    log_china_aid_total = log1p(china_aid_usd2021_total),
    log_gdp_pc = ifelse(!is.na(gdp_pc) & gdp_pc > 0, log(gdp_pc), NA_real_),
    aid_any = as.integer(years_with_aid > 0),
    regime_label = case_when(
      round(political_regime) == 0 ~ "Closed autocracy",
      round(political_regime) == 1 ~ "Electoral autocracy",
      round(political_regime) == 2 ~ "Electoral democracy",
      round(political_regime) == 3 ~ "Liberal democracy",
      TRUE ~ modal_regime
    ),
    regime_family = case_when(
      round(political_regime) %in% c(0, 1) ~ "Authoritarian",
      round(political_regime) %in% c(2, 3) ~ "Democratic",
      TRUE ~ modal_regime_family
    )
  )

write.csv(
  country_cross_section,
  file.path(output_dir, "selection_model_country_cross_section.csv"),
  row.names = FALSE
)

desc_vars <- c(
  "china_aid_usd2021",
  "china_aid_usd2021_total",
  "log_china_aid",
  "log_china_aid_total",
  "china_project_count",
  "autocracy_score",
  "political_regime",
  "log_gdp_pc",
  "cpi",
  "extreme_poverty",
  "gini"
)

desc_tbl <- describe_vars(country_cross_section, desc_vars)
write.csv(
  desc_tbl,
  file.path(output_dir, "selection_model_country_descriptive_stats.csv"),
  row.names = FALSE
)

desc_by_regime_tbl <- country_cross_section %>%
  filter(!is.na(regime_family)) %>%
  group_by(regime_family) %>%
  summarize(
    observations = n(),
    mean_aid_usd2021 = mean(china_aid_usd2021, na.rm = TRUE),
    median_aid_usd2021 = median(china_aid_usd2021, na.rm = TRUE),
    mean_project_count = mean(china_project_count, na.rm = TRUE),
    mean_autocracy_score = mean(autocracy_score, na.rm = TRUE),
    mean_log_gdp_pc = mean(log_gdp_pc, na.rm = TRUE),
    mean_cpi = mean(cpi, na.rm = TRUE),
    mean_extreme_poverty = mean(extreme_poverty, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  desc_by_regime_tbl,
  file.path(output_dir, "selection_model_country_descriptive_stats_by_regime.csv"),
  row.names = FALSE
)

cs_m1_data <- country_cross_section %>%
  filter(!is.na(log_china_aid), !is.na(autocracy_score))

cs_m1 <- lm(log_china_aid ~ autocracy_score, data = cs_m1_data)

cs_m2_data <- country_cross_section %>%
  filter(
    !is.na(log_china_aid),
    !is.na(autocracy_score),
    !is.na(log_gdp_pc),
    !is.na(cpi),
    !is.na(extreme_poverty),
    !is.na(gini)
  )

cs_m2 <- lm(
  log_china_aid ~ autocracy_score + log_gdp_pc + cpi + extreme_poverty + gini,
  data = cs_m2_data
)

cs_m2_positive_aid_data <- country_cross_section %>%
  filter(
    china_aid_usd2021 > 0,
    !is.na(log_china_aid),
    !is.na(autocracy_score),
    !is.na(log_gdp_pc),
    !is.na(cpi),
    !is.na(extreme_poverty),
    !is.na(gini)
  )

cs_m2_positive_aid <- lm(
  log_china_aid ~ autocracy_score + log_gdp_pc + cpi + extreme_poverty + gini,
  data = cs_m2_positive_aid_data
)

main_controls <- c("autocracy_score", "log_gdp_pc", "cpi", "extreme_poverty", "gini")
vif_tbl <- compute_vif(cs_m2_data, main_controls)
write.csv(
  vif_tbl,
  file.path(output_dir, "selection_model_country_vif.csv"),
  row.names = FALSE
)

vcov_cs_m1_hc1 <- vcov_hc1(cs_m1)
vcov_cs_m2_hc1 <- vcov_hc1(cs_m2)
vcov_cs_m2_positive_aid_hc1 <- vcov_hc1(cs_m2_positive_aid)

cross_section_robust_coef_tbl <- bind_rows(
  tidy_with_vcov(cs_m1, vcov_cs_m1_hc1, "CS_M1_bivariate_HC1"),
  tidy_with_vcov(cs_m2, vcov_cs_m2_hc1, "CS_M2_controls_HC1"),
  tidy_with_vcov(cs_m2_positive_aid, vcov_cs_m2_positive_aid_hc1, "CS_M2_controls_positive_aid_HC1")
)

write.csv(
  cross_section_robust_coef_tbl,
  file.path(output_dir, "selection_model_country_coefficients_robust.csv"),
  row.names = FALSE
)

cross_section_coef_tbl <- bind_rows(
  tidy(cs_m1, conf.int = TRUE) %>% mutate(model = "CS_M1_bivariate"),
  tidy(cs_m2, conf.int = TRUE) %>% mutate(model = "CS_M2_controls"),
  tidy(cs_m2_positive_aid, conf.int = TRUE) %>% mutate(model = "CS_M2_controls_positive_aid")
)

write.csv(
  cross_section_coef_tbl,
  file.path(output_dir, "selection_model_country_coefficients.csv"),
  row.names = FALSE
)

cross_section_fit_tbl <- bind_rows(
  glance(cs_m1) %>% mutate(model = "CS_M1_bivariate"),
  glance(cs_m2) %>% mutate(model = "CS_M2_controls"),
  glance(cs_m2_positive_aid) %>% mutate(model = "CS_M2_controls_positive_aid")
)

write.csv(
  cross_section_fit_tbl,
  file.path(output_dir, "selection_model_country_fitstats.csv"),
  row.names = FALSE
)

bp_tbl <- bind_rows(
  breusch_pagan_test(cs_m1) %>% mutate(model = "CS_M1_bivariate"),
  breusch_pagan_test(cs_m2) %>% mutate(model = "CS_M2_controls"),
  breusch_pagan_test(cs_m2_positive_aid) %>% mutate(model = "CS_M2_controls_positive_aid")
) %>%
  select(model, statistic, df, p_value)

write.csv(
  bp_tbl,
  file.path(output_dir, "selection_model_country_breusch_pagan.csv"),
  row.names = FALSE
)

cooks_tbl <- bind_rows(
  cooks_distance_table(cs_m1, cs_m1_data, "CS_M1_bivariate"),
  cooks_distance_table(cs_m2, cs_m2_data, "CS_M2_controls"),
  cooks_distance_table(
    cs_m2_positive_aid,
    cs_m2_positive_aid_data,
    "CS_M2_controls_positive_aid"
  )
)

write.csv(
  cooks_tbl,
  file.path(output_dir, "selection_model_country_cooks_distance_top10.csv"),
  row.names = FALSE
)

invisible(capture.output(
  stargazer(
    cs_m1, cs_m2, cs_m2_positive_aid,
    type = "text",
    title = "Chinese Aid and Regime Type (Country-Level Cross-Section)",
    dep.var.labels = "Log(1 + mean annual Chinese aid in constant USD 2021)",
    column.labels = c("Bivariate", "Controls", "Controls, positive-aid only"),
    covariate.labels = c(
      "Autocracy score",
      "Log GDP per capita",
      "Corruption Perceptions Index",
      "Extreme poverty share",
      "Gini index"
    ),
    omit.stat = c("f", "ser"),
    add.lines = list(
      c("Unit of analysis", "Country", "Country", "Country"),
      c("Years collapsed", "2013-2021 mean", "2013-2021 mean", "2013-2021 mean"),
      c("Sample", "All countries", "All countries", "Aid recipients only"),
      c("Standard errors", "HC1", "HC1", "HC1")
    ),
    se = list(
      sqrt(diag(vcov_cs_m1_hc1)),
      sqrt(diag(vcov_cs_m2_hc1)),
      sqrt(diag(vcov_cs_m2_positive_aid_hc1))
    ),
    out = file.path(output_dir, "selection_model_country_regression_table.txt")
  )
))

invisible(capture.output(
  stargazer(
    cs_m1, cs_m2, cs_m2_positive_aid,
    type = "html",
    title = "Chinese Aid and Regime Type (Country-Level Cross-Section)",
    dep.var.labels = "Log(1 + mean annual Chinese aid in constant USD 2021)",
    column.labels = c("Bivariate", "Controls", "Controls, positive-aid only"),
    covariate.labels = c(
      "Autocracy score",
      "Log GDP per capita",
      "Corruption Perceptions Index",
      "Extreme poverty share",
      "Gini index"
    ),
    omit.stat = c("f", "ser"),
    add.lines = list(
      c("Unit of analysis", "Country", "Country", "Country"),
      c("Years collapsed", "2013-2021 mean", "2013-2021 mean", "2013-2021 mean"),
      c("Sample", "All countries", "All countries", "Aid recipients only"),
      c("Standard errors", "HC1", "HC1", "HC1")
    ),
    se = list(
      sqrt(diag(vcov_cs_m1_hc1)),
      sqrt(diag(vcov_cs_m2_hc1)),
      sqrt(diag(vcov_cs_m2_positive_aid_hc1))
    ),
    out = file.path(output_dir, "selection_model_country_regression_table.html")
  )
))

invisible(capture.output(
  stargazer(
    desc_tbl,
    type = "text",
    summary = FALSE,
    title = "Country-Level Descriptive Statistics",
    out = file.path(output_dir, "selection_model_country_descriptive_stats.txt")
  )
))

invisible(capture.output(
  stargazer(
    desc_tbl,
    type = "html",
    summary = FALSE,
    title = "Country-Level Descriptive Statistics",
    out = file.path(output_dir, "selection_model_country_descriptive_stats.html")
  )
))

regime_barplot <- country_cross_section %>%
  filter(!is.na(regime_label)) %>%
  group_by(regime_label) %>%
  summarize(
    avg_log_aid = mean(log_china_aid, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    regime_label = factor(
      regime_label,
      levels = c(
        "Liberal democracy",
        "Electoral democracy",
        "Electoral autocracy",
        "Closed autocracy"
      )
    )
  ) %>%
  ggplot(aes(x = regime_label, y = avg_log_aid, fill = regime_label)) +
  geom_col() +
  labs(
    title = "Average Chinese Aid by Regime Type (Country Means, 2013-2021)",
    x = "Regime type",
    y = "Average log(1 + mean annual Chinese aid in constant USD 2021)",
    caption = str_wrap(
      paste(
        "Descriptive figure. Each bar reports the mean of log(1 + mean annual Chinese aid",
        "in constant 2021 USD) across country-level observations created by averaging",
        "country-year values from 2013 to 2021."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = regime_gradient, guide = "none") +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_country_regime_barplot.png"),
  plot = regime_barplot,
  width = 8,
  height = 5,
  dpi = 300
)

sector_barplot <- sector_top_recipients_tbl %>%
  mutate(
    entity = factor(entity, levels = rev(entity)),
    aid_billions_usd2021 = total_aid_usd2021 / 1000000000
  ) %>%
  ggplot(aes(x = entity, y = aid_billions_usd2021)) +
  geom_col(fill = china_red_deep) +
  coord_flip() +
  labs(
    title = "Top Country Recipients of Chinese Aid (2013-2021 Total)",
    x = "Country",
    y = "Adjusted aid, billions of constant USD 2021",
    caption = str_wrap(
      paste(
        "Cross-sectional analogue to the sector chart. Bars show the top 10 countries by",
        "total Chinese aid over 2013-2021 using AidData records marked Recommended For",
        "Aggregates = Yes."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_country_sector_barplot.png"),
  plot = sector_barplot,
  width = 9,
  height = 6,
  dpi = 300
)

aid_scatter <- country_cross_section %>%
  filter(!is.na(autocracy_score), !is.na(log_china_aid)) %>%
  ggplot(aes(x = autocracy_score, y = log_china_aid)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.35, color = democracy_blue) +
  geom_smooth(method = "lm", se = TRUE, color = china_red, linewidth = 1) +
  scale_x_continuous(
    breaks = 0:3,
    labels = c(
      "Liberal democracy",
      "Electoral democracy",
      "Electoral autocracy",
      "Closed autocracy"
    )
  ) +
  labs(
    title = "Bivariate Relationship Between Autocracy and Chinese Aid",
    x = "Regime type",
    y = "Log(1 + mean annual Chinese aid in constant USD 2021)",
    caption = str_wrap(
      paste(
        "Relational figure. Points are countries collapsed to one observation each using",
        "2013-2021 means. The line is an OLS fit with 95% confidence interval."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_country_aid_scatter.png"),
  plot = aid_scatter,
  width = 8,
  height = 5,
  dpi = 300
)

regime_family_pie <- country_cross_section %>%
  filter(!is.na(regime_family)) %>%
  count(regime_family) %>%
  mutate(
    share = n / sum(n),
    label = paste0(regime_family, " (", round(share * 100, 1), "%)")
  ) %>%
  ggplot(aes(x = "", y = n, fill = regime_family)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3.8) +
  coord_polar(theta = "y") +
  labs(
    title = "Share of Authoritarian vs Democratic Countries",
    fill = "Regime family",
    caption = str_wrap(
      paste(
        "Descriptive figure. Shares are calculated over country-level observations after",
        "collapsing 2013-2021 to one row per country."
      ),
      width = 90
    )
  ) +
  theme_void(base_size = 12) +
  theme(legend.position = "right") +
  caption_theme +
  scale_fill_manual(values = c("Authoritarian" = china_red, "Democratic" = democracy_blue))

ggsave(
  filename = file.path(output_dir, "selection_model_country_regime_family_pie.png"),
  plot = regime_family_pie,
  width = 7,
  height = 5,
  dpi = 300
)

latest_year <- max(analysis_panel$year, na.rm = TRUE)

latest_country_regime_pie <- analysis_panel %>%
  filter(year == latest_year, !is.na(regime_family)) %>%
  distinct(entity, regime_family) %>%
  count(regime_family) %>%
  mutate(
    share = n / sum(n),
    label = paste0(regime_family, " countries (", round(share * 100, 1), "%)")
  ) %>%
  ggplot(aes(x = "", y = n, fill = regime_family)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3.8) +
  coord_polar(theta = "y") +
  labs(
    title = paste("Country Distribution by Regime Family in", latest_year),
    fill = "Regime family",
    caption = str_wrap(
      paste(
        "Descriptive figure. Shares are calculated over distinct countries in the latest",
        "sample year to mirror the panel script's latest-year country composition chart."
      ),
      width = 90
    )
  ) +
  theme_void(base_size = 12) +
  theme(legend.position = "right") +
  caption_theme +
  scale_fill_manual(values = c("Authoritarian" = china_red_dark, "Democratic" = democracy_blue))

ggsave(
  filename = file.path(output_dir, "selection_model_country_latest_regime_family_pie.png"),
  plot = latest_country_regime_pie,
  width = 7,
  height = 5,
  dpi = 300
)

diagnostic_data_m2 <- data.frame(
  fitted = fitted(cs_m2),
  residual = resid(cs_m2),
  std_residual = rstandard(cs_m2),
  cooks_distance = cooks.distance(cs_m2)
)

residual_fitted_plot <- diagnostic_data_m2 %>%
  ggplot(aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.35, color = china_red_dark) +
  geom_hline(yintercept = 0, linetype = "dashed", color = china_red) +
  geom_smooth(se = FALSE, color = china_red_light, linewidth = 1) +
  labs(
    title = "Residuals vs Fitted Values (Country M2)",
    x = "Fitted values",
    y = "Residuals",
    caption = str_wrap(
      paste(
        "Relational diagnostic figure. Residuals from the controlled country-level model",
        "are plotted against fitted values to assess functional form and heteroskedasticity."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_country_residuals_vs_fitted_m2.png"),
  plot = residual_fitted_plot,
  width = 8,
  height = 5,
  dpi = 300
)

qq_data_m2 <- data.frame(
  sample = sort(diagnostic_data_m2$std_residual),
  theoretical = qnorm(ppoints(length(diagnostic_data_m2$std_residual)))
)

qq_plot_m2 <- qq_data_m2 %>%
  ggplot(aes(x = theoretical, y = sample)) +
  geom_point(alpha = 0.35, color = china_red_dark) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = china_red) +
  labs(
    title = "Normal Q-Q Plot of Standardized Residuals (Country M2)",
    x = "Theoretical quantiles",
    y = "Sample quantiles",
    caption = str_wrap(
      paste(
        "Relational diagnostic figure. Standardized residuals from the country-level",
        "controlled model are compared with normal quantiles."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_country_qqplot_m2.png"),
  plot = qq_plot_m2,
  width = 8,
  height = 5,
  dpi = 300
)

cooks_plot_m2 <- cooks_tbl %>%
  filter(model == "CS_M2_controls") %>%
  ggplot(aes(x = reorder(entity, cooks_distance), y = cooks_distance)) +
  geom_col(fill = china_red_deep) +
  geom_hline(yintercept = 4 / nrow(cs_m2_data), linetype = "dashed", color = china_red_light) +
  coord_flip() +
  labs(
    title = "Top 10 Cook's Distance Observations (Country M2)",
    x = "Country",
    y = "Cook's distance",
    caption = str_wrap(
      paste(
        "Relational diagnostic figure. Bars show the 10 most influential country-level",
        "observations in the controlled cross-sectional model."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_country_cooks_distance_m2.png"),
  plot = cooks_plot_m2,
  width = 8,
  height = 6,
  dpi = 300
)

assumption_summary_tbl <- bp_tbl %>%
  mutate(
    heteroskedasticity_flag = ifelse(p_value < 0.05, "Evidence of heteroskedasticity", "No BP evidence at 5%"),
    standard_error_treatment = "HC1 robust SE"
  )

write.csv(
  assumption_summary_tbl,
  file.path(output_dir, "selection_model_country_assumption_summary.csv"),
  row.names = FALSE
)

panel_robust <- read.csv(file.path(output_root_dir, "selection_model_coefficients_robust.csv"))
panel_fit <- read.csv(file.path(output_root_dir, "selection_model_fitstats.csv"))

comparison_tbl <- bind_rows(
  panel_robust %>%
    filter(
      model %in% c("M1_bivariate_HC1", "M2_controls_HC1", "M3_country_year_FE_cluster_entity"),
      term == "autocracy_score"
    ) %>%
    transmute(
      sample = c(
        "Panel bivariate",
        "Panel controls",
        "Panel country + year FE"
      ),
      coefficient = estimate,
      std_error = std.error,
      p_value = p.value
    ),
  cross_section_robust_coef_tbl %>%
    filter(
      model %in% c(
        "CS_M1_bivariate_HC1",
        "CS_M2_controls_HC1",
        "CS_M2_controls_positive_aid_HC1"
      ),
      term == "autocracy_score"
    ) %>%
    transmute(
      sample = c("Country bivariate", "Country controls", "Country controls, positive-aid only"),
      coefficient = estimate,
      std_error = std.error,
      p_value = p.value
    )
)

comparison_tbl$r_squared <- c(
  panel_fit$r.squared[match(
    c("M1_bivariate", "M2_controls", "M3_country_year_FE"),
    panel_fit$model
  )],
  cross_section_fit_tbl$r.squared[match(
    c("CS_M1_bivariate", "CS_M2_controls", "CS_M2_controls_positive_aid"),
    cross_section_fit_tbl$model
  )]
)

write.csv(
  comparison_tbl,
  file.path(output_dir, "selection_model_panel_vs_country_comparison.csv"),
  row.names = FALSE
)

cat("Wrote outputs:\n")
cat("- selection_model_country_cross_section.csv\n")
cat("- selection_model_country_descriptive_stats.csv\n")
cat("- selection_model_country_descriptive_stats.txt\n")
cat("- selection_model_country_descriptive_stats.html\n")
cat("- selection_model_country_descriptive_stats_by_regime.csv\n")
cat("- selection_model_country_coefficients.csv\n")
cat("- selection_model_country_coefficients_robust.csv\n")
cat("- selection_model_country_fitstats.csv\n")
cat("- selection_model_country_vif.csv\n")
cat("- selection_model_country_breusch_pagan.csv\n")
cat("- selection_model_country_cooks_distance_top10.csv\n")
cat("- selection_model_country_assumption_summary.csv\n")
cat("- selection_model_country_sector_breakdown.csv\n")
cat("- selection_model_country_sector_top_recipients.csv\n")
cat("- selection_model_country_regression_table.txt\n")
cat("- selection_model_country_regression_table.html\n")
cat("- selection_model_country_aid_scatter.png\n")
cat("- selection_model_country_regime_barplot.png\n")
cat("- selection_model_country_sector_barplot.png\n")
cat("- selection_model_country_regime_family_pie.png\n")
cat("- selection_model_country_latest_regime_family_pie.png\n")
cat("- selection_model_country_residuals_vs_fitted_m2.png\n")
cat("- selection_model_country_qqplot_m2.png\n")
cat("- selection_model_country_cooks_distance_m2.png\n")
cat("- selection_model_panel_vs_country_comparison.csv\n")
