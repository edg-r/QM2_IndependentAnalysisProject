# Selection model: Do countries with more authoritarian regimes receive more
# Chinese aid? This script builds a country-year panel and runs progressive
# regressions for your QM2 memo.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(ggplot2)
library(stargazer)
library(grid)

options(scipen = 999)

# Convert spreadsheet strings to numeric while treating "." as missing.
to_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", ".", "NA", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

# Mean helper that returns NA when a whole group is missing.
safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

# Descriptive-statistics helper for key variables.
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

caption_theme <- theme(
  plot.caption = element_text(hjust = 0, size = 9, lineheight = 1.1),
  plot.caption.position = "plot",
  plot.margin = margin(12, 18, 18, 12)
)

# Chinese-red palette for memo figures.
china_red <- "#DE2910"
china_red_dark <- "#A61E0A"
china_red_light <- "#F28B82"
china_red_deep <- "#7A1608"
china_red_soft <- "#F7D6D1"
democracy_blue <- "#1D4E89"
democracy_blue_deep <- "#0B2545"
democracy_blue_soft <- "#8FB7E8"
regime_gradient <- c(
  "Liberal democracy" = democracy_blue_deep,
  "Electoral democracy" = democracy_blue_soft,
  "Electoral autocracy" = china_red,
  "Closed autocracy" = china_red_deep
)

# Simple VIF calculator for the non-FE control model (M2).
compute_vif <- function(data, vars) {
  out <- lapply(vars, function(v) {
    rhs <- setdiff(vars, v)
    fml <- as.formula(paste(v, "~", paste(rhs, collapse = " + ")))
    r2 <- summary(lm(fml, data = data))$r.squared
    data.frame(variable = v, vif = 1 / (1 - r2))
  })
  bind_rows(out)
}

# Breusch-Pagan test implemented from the auxiliary regression n * R^2.
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

# HC1 heteroskedasticity-robust covariance matrix.
vcov_hc1 <- function(model) {
  X <- model.matrix(model)
  u <- resid(model)
  n <- nrow(X)
  k <- ncol(X)
  bread <- solve(crossprod(X))
  meat <- crossprod(X, diag(u^2, nrow = n) %*% X)
  (n / (n - k)) * bread %*% meat %*% bread
}

# One-way cluster-robust covariance by grouping variable.
vcov_cluster <- function(model, cluster) {
  X <- model.matrix(model)
  u <- resid(model)
  n <- nrow(X)
  k <- ncol(X)
  g <- length(unique(cluster))
  bread <- solve(crossprod(X))
  cluster_scores <- lapply(split(seq_len(n), cluster), function(idx) {
    Xi <- X[idx, , drop = FALSE]
    ui <- u[idx]
    crossprod(Xi, ui)
  })
  meat <- Reduce(`+`, lapply(cluster_scores, function(s) s %*% t(s)))
  scale_factor <- (g / (g - 1)) * ((n - 1) / (n - k))
  scale_factor * bread %*% meat %*% bread
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

cooks_distance_table <- function(model, data, model_name, top_n = 10) {
  cooks <- cooks.distance(model)
  cutoff <- 4 / length(cooks)
  out <- data.frame(
    observation = seq_along(cooks),
    entity = data$entity,
    year = data$year,
    cooks_distance = cooks,
    above_cutoff = cooks > cutoff,
    model = model_name
  )
  out[order(out$cooks_distance, decreasing = TRUE), ][seq_len(min(top_n, nrow(out))), ]
}

project_dir <- "."
aid_path <- file.path(project_dir, "chinese-aid-data-2000-2021.xlsx")
owid_path <- file.path(project_dir, "our-world-in-data-2013-2023.xlsx")
output_dir <- file.path(project_dir, "output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------
# 1) Load and prep AidData
# -----------------------------
aid_raw <- read_excel(aid_path, sheet = "aid-data", col_types = "text")

aid_project_level <- aid_raw %>%
  transmute(
    entity = str_trim(entity),
    year = to_numeric(year),
    recommended_for_aggregates = str_trim(`Recommended For Aggregates`),
    adjusted_amount_usd2021 = to_numeric(`Adjusted Amount (Constant USD 2021)`),
    amount_usd2021 = to_numeric(`Amount (Constant USD 2021)`),
    flow_type_simplified = str_trim(`Flow Type Simplified`),
    intent = str_trim(Intent),
    sector_code = str_trim(`Sector Code`),
    sector_name = str_trim(`Sector Name`)
  ) %>%
  filter(!is.na(entity), !is.na(year), year >= 2013, year <= 2021) %>%
  # AidData guidance: use records recommended for aggregation to avoid
  # double-counting and to drop cancelled/suspended/pledge-only records.
  filter(recommended_for_aggregates == "Yes") %>%
  mutate(
    sector_code = ifelse(is.na(sector_code) | sector_code == "", "998", sector_code),
    sector_name = ifelse(
      is.na(sector_name) | sector_name == "",
      "UNALLOCATED/UNSPECIFIED",
      sector_name
    )
  )

aid_panel <- aid_project_level %>%
  group_by(entity, year) %>%
  summarize(
    # Main outcome: total yearly Chinese aid by country in constant USD.
    china_aid_usd2021 = sum(adjusted_amount_usd2021, na.rm = TRUE),
    china_aid_usd2021_unadjusted = sum(amount_usd2021, na.rm = TRUE),
    # Additional outcomes useful for robustness.
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
  mutate(
    share_of_total_aid = total_aid_usd2021 / sum(total_aid_usd2021, na.rm = TRUE)
  ) %>%
  arrange(desc(total_aid_usd2021))

write.csv(
  sector_breakdown_tbl,
  file.path(output_dir, "selection_model_sector_breakdown.csv"),
  row.names = FALSE
)

sector_top_recipients_tbl <- aid_project_level %>%
  group_by(sector_code, sector_name, entity) %>%
  summarize(
    total_aid_usd2021 = sum(adjusted_amount_usd2021, na.rm = TRUE),
    funded_project_count = sum(!is.na(adjusted_amount_usd2021)),
    .groups = "drop"
  ) %>%
  group_by(sector_code, sector_name) %>%
  arrange(desc(total_aid_usd2021), .by_group = TRUE) %>%
  mutate(rank_within_sector = row_number()) %>%
  filter(rank_within_sector <= 5) %>%
  ungroup()

write.csv(
  sector_top_recipients_tbl,
  file.path(output_dir, "selection_model_sector_top_recipients.csv"),
  row.names = FALSE
)

# -----------------------------
# 2) Load and prep OWID controls
# -----------------------------
owid_raw <- read_excel(owid_path, sheet = "our-world-in-data", col_types = "text")

owid_panel <- owid_raw %>%
  transmute(
    entity = str_trim(entity),
    year = to_numeric(year),
    entity_year = str_trim(`entity-year`),
    political_regime = to_numeric(`political-regime`),  # 0=closed autocracy ... 3=liberal democracy
    democracy_index = to_numeric(`democracy-index-eiu`),
    cpi = to_numeric(`ti-corruption-perception-index`), # higher = less corruption
    extreme_poverty = to_numeric(`share-of-population-in-extreme-poverty`),
    gini = to_numeric(`economic-inequality-gini-index`),
    gdp_pc = to_numeric(`gdp-per-capita-worldbank`)
  ) %>%
  filter(!is.na(entity), !is.na(year), year >= 2013, year <= 2021)

# -----------------------------
# 3) Merge to country-year panel
# -----------------------------
analysis_panel <- owid_panel %>%
  left_join(aid_panel, by = c("entity", "year")) %>%
  mutate(
    # If a country-year has no AidData record, treat aid as zero (not missing).
    china_project_count = replace_na(china_project_count, 0L),
    china_aid_usd2021 = replace_na(china_aid_usd2021, 0),
    china_aid_usd2021_unadjusted = replace_na(china_aid_usd2021_unadjusted, 0),
    aid_any = as.integer(china_project_count > 0),
    log_china_aid = log1p(china_aid_usd2021),
    log_gdp_pc = ifelse(!is.na(gdp_pc) & gdp_pc > 0, log(gdp_pc), NA_real_),
    # Recode so higher value = more authoritarian; coefficient sign is easier.
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

write.csv(analysis_panel, file.path(output_dir, "selection_model_panel.csv"), row.names = FALSE)

# -----------------------------
# 4) Descriptive statistics
# -----------------------------
desc_vars <- c(
  "china_aid_usd2021",
  "log_china_aid",
  "china_project_count",
  "autocracy_score",
  "political_regime",
  "log_gdp_pc",
  "cpi",
  "extreme_poverty",
  "gini"
)

desc_tbl <- describe_vars(analysis_panel, desc_vars)
write.csv(desc_tbl, file.path(output_dir, "selection_model_descriptive_stats.csv"), row.names = FALSE)

desc_by_regime_tbl <- analysis_panel %>%
  filter(!is.na(regime_family)) %>%
  group_by(regime_family) %>%
  summarize(
    observations = n(),
    countries = n_distinct(entity),
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
  file.path(output_dir, "selection_model_descriptive_stats_by_regime.csv"),
  row.names = FALSE
)

# -----------------------------
# 5) Regressions (progressive specs)
# -----------------------------
run_optional_logit <- FALSE

# Model 1: Bivariate
# Required by assignment: start with bivariate relationship.
m1_data <- analysis_panel %>%
  filter(!is.na(log_china_aid), !is.na(autocracy_score))

m1 <- lm(log_china_aid ~ autocracy_score, data = m1_data)

# Model 2: Add controls
# Adds core confounders from OWID.
m2_data <- analysis_panel %>%
  filter(
    !is.na(log_china_aid),
    !is.na(autocracy_score),
    !is.na(log_gdp_pc),
    !is.na(cpi),
    !is.na(extreme_poverty),
    !is.na(gini)
  )

m2 <- lm(
  log_china_aid ~ autocracy_score + log_gdp_pc + cpi + extreme_poverty + gini,
  data = m2_data
)

# Model 3: Country and year fixed effects (with lm dummies)
# Main selection-model specification: compares within-country changes over time
# while controlling for global year shocks.
m3 <- lm(
  log_china_aid ~ autocracy_score + log_gdp_pc + cpi + extreme_poverty + gini +
    factor(entity) + factor(year),
  data = m2_data
)

# -----------------------------
# 6) Diagnostics and outputs
# -----------------------------
# Multicollinearity check for non-FE covariates.
main_controls <- c("autocracy_score", "log_gdp_pc", "cpi", "extreme_poverty", "gini")
vif_tbl <- compute_vif(m2_data, main_controls)
write.csv(vif_tbl, file.path(output_dir, "selection_model_vif.csv"), row.names = FALSE)

bp_tbl <- bind_rows(
  breusch_pagan_test(m1) %>% mutate(model = "M1_bivariate"),
  breusch_pagan_test(m2) %>% mutate(model = "M2_controls"),
  breusch_pagan_test(m3) %>% mutate(model = "M3_country_year_FE")
) %>%
  select(model, statistic, df, p_value)

write.csv(bp_tbl, file.path(output_dir, "selection_model_breusch_pagan.csv"), row.names = FALSE)

vcov_m1_hc1 <- vcov_hc1(m1)
vcov_m2_hc1 <- vcov_hc1(m2)
vcov_m3_cluster_entity <- vcov_cluster(m3, m2_data$entity)

robust_coef_tbl <- bind_rows(
  tidy_with_vcov(m1, vcov_m1_hc1, "M1_bivariate_HC1"),
  tidy_with_vcov(m2, vcov_m2_hc1, "M2_controls_HC1"),
  tidy_with_vcov(m3, vcov_m3_cluster_entity, "M3_country_year_FE_cluster_entity")
)

write.csv(
  robust_coef_tbl,
  file.path(output_dir, "selection_model_coefficients_robust.csv"),
  row.names = FALSE
)

cooks_tbl <- bind_rows(
  cooks_distance_table(m1, m1_data, "M1_bivariate"),
  cooks_distance_table(m2, m2_data, "M2_controls"),
  cooks_distance_table(m3, m2_data, "M3_country_year_FE")
)

write.csv(cooks_tbl, file.path(output_dir, "selection_model_cooks_distance_top10.csv"), row.names = FALSE)

coef_tbl <- bind_rows(
  tidy(m1, conf.int = TRUE) %>% mutate(model = "M1_bivariate"),
  tidy(m2, conf.int = TRUE) %>% mutate(model = "M2_controls"),
  tidy(m3, conf.int = TRUE) %>% mutate(model = "M3_country_year_FE")
)

if (run_optional_logit) {
  m4 <- glm(
    aid_any ~ autocracy_score + log_gdp_pc + cpi + extreme_poverty + gini +
      factor(entity) + factor(year),
    data = m2_data,
    family = binomial(link = "logit")
  )

  coef_tbl <- bind_rows(
    coef_tbl,
    tidy(m4, conf.int = TRUE) %>% mutate(model = "M4_logit_any_aid")
  )
}

write.csv(coef_tbl, file.path(output_dir, "selection_model_coefficients.csv"), row.names = FALSE)

fit_tbl <- bind_rows(
  glance(m1) %>% mutate(model = "M1_bivariate"),
  glance(m2) %>% mutate(model = "M2_controls"),
  glance(m3) %>% mutate(model = "M3_country_year_FE")
)

if (run_optional_logit) {
  fit_tbl <- bind_rows(
    fit_tbl,
    glance(m4) %>% mutate(model = "M4_logit_any_aid")
  )
}

write.csv(fit_tbl, file.path(output_dir, "selection_model_fitstats.csv"), row.names = FALSE)

# Stargazer regression tables for memo-ready output.
invisible(capture.output(
  stargazer(
    m1, m2, m3,
    type = "text",
    title = "Chinese Aid and Regime Type (Robust Standard Errors)",
    dep.var.labels = "Log(1 + Chinese aid in constant USD 2021)",
    column.labels = c("Bivariate", "Controls", "Country + Year FE"),
    covariate.labels = c(
      "Autocracy score",
      "Log GDP per capita",
      "Corruption Perceptions Index",
      "Extreme poverty share",
      "Gini index"
    ),
    omit = c("factor\\(entity\\)", "factor\\(year\\)"),
    omit.stat = c("f", "ser"),
    add.lines = list(
      c("Country fixed effects", "No", "No", "Yes"),
      c("Year fixed effects", "No", "No", "Yes"),
      c("Standard errors", "HC1", "HC1", "Clustered by country")
    ),
    se = list(
      sqrt(diag(vcov_m1_hc1)),
      sqrt(diag(vcov_m2_hc1)),
      sqrt(diag(vcov_m3_cluster_entity))
    ),
    out = file.path(output_dir, "selection_model_regression_table.txt")
  )
))

invisible(capture.output(
  stargazer(
    m1, m2, m3,
    type = "html",
    title = "Chinese Aid and Regime Type (Robust Standard Errors)",
    dep.var.labels = "Log(1 + Chinese aid in constant USD 2021)",
    column.labels = c("Bivariate", "Controls", "Country + Year FE"),
    covariate.labels = c(
      "Autocracy score",
      "Log GDP per capita",
      "Corruption Perceptions Index",
      "Extreme poverty share",
      "Gini index"
    ),
    omit = c("factor\\(entity\\)", "factor\\(year\\)"),
    omit.stat = c("f", "ser"),
    add.lines = list(
      c("Country fixed effects", "No", "No", "Yes"),
      c("Year fixed effects", "No", "No", "Yes"),
      c("Standard errors", "HC1", "HC1", "Clustered by country")
    ),
    se = list(
      sqrt(diag(vcov_m1_hc1)),
      sqrt(diag(vcov_m2_hc1)),
      sqrt(diag(vcov_m3_cluster_entity))
    ),
    out = file.path(output_dir, "selection_model_regression_table.html")
  )
))

invisible(capture.output(
  stargazer(
    desc_tbl,
    type = "text",
    summary = FALSE,
    title = "Descriptive Statistics",
    out = file.path(output_dir, "selection_model_descriptive_stats.txt")
  )
))

invisible(capture.output(
  stargazer(
    desc_tbl,
    type = "html",
    summary = FALSE,
    title = "Descriptive Statistics",
    out = file.path(output_dir, "selection_model_descriptive_stats.html")
  )
))

# Quick descriptive plot for memo figure draft.
regime_barplot <- analysis_panel %>%
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
    title = "Average Chinese Aid by Regime Type (2013-2021)",
    x = "Regime type",
    y = "Average log(1 + Chinese aid in constant USD 2021)",
    caption = str_wrap(
      paste(
        "Descriptive figure. Bars show the mean of log(1 + annual Chinese aid) by",
        "OWID political-regime category. Source variable: political-regime",
        "(0 = closed autocracy, 1 = electoral autocracy, 2 = electoral democracy,",
        "3 = liberal democracy)."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = regime_gradient, guide = "none") +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_regime_barplot.png"),
  plot = regime_barplot,
  width = 8,
  height = 5,
  dpi = 300
)

sector_barplot <- sector_breakdown_tbl %>%
  slice_head(n = 10) %>%
  mutate(
    sector_name = factor(sector_name, levels = rev(sector_name)),
    aid_billions_usd2021 = total_aid_usd2021 / 1000000000
  ) %>%
  ggplot(aes(x = sector_name, y = aid_billions_usd2021)) +
  geom_col(fill = china_red_deep) +
  coord_flip() +
  labs(
    title = "Top Chinese Aid Earmarks by Sector (2013-2021)",
    x = "Sector",
    y = "Adjusted aid, billions of constant USD 2021",
    caption = str_wrap(
      paste(
        "Sector totals are computed from project-level AidData records marked",
        "'Recommended For Aggregates = Yes' and summed using Adjusted Amount",
        "(Constant USD 2021). Only 2013-2021 is included to match the main panel."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_sector_barplot.png"),
  plot = sector_barplot,
  width = 9,
  height = 6,
  dpi = 300
)

aid_scatter <- analysis_panel %>%
  filter(!is.na(autocracy_score), !is.na(log_china_aid)) %>%
  ggplot(aes(x = autocracy_score, y = log_china_aid)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.25, color = democracy_blue) +
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
    y = "Log(1 + Chinese aid in constant USD 2021)",
    caption = str_wrap(
      paste(
        "Relational figure. Points are country-years and the line is an OLS fit with",
        "95% confidence interval. autocracy_score is coded as 3 - political_regime,",
        "so 3 = closed autocracy and 0 = liberal democracy."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_aid_scatter.png"),
  plot = aid_scatter,
  width = 8,
  height = 5,
  dpi = 300
)

regime_family_pie <- analysis_panel %>%
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
    title = "Share of Authoritarian vs Democratic Country-Years",
    fill = "Regime family",
    caption = str_wrap(
      paste(
        "Descriptive figure. Shares are calculated over country-year observations.",
        "Authoritarian = political-regime 0 or 1; Democratic = political-regime 2 or 3."
      ),
      width = 90
    )
  ) +
  theme_void(base_size = 12) +
  theme(legend.position = "right") +
  caption_theme +
  scale_fill_manual(values = c("Authoritarian" = china_red, "Democratic" = democracy_blue))

ggsave(
  filename = file.path(output_dir, "selection_model_regime_family_pie.png"),
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
        "sample year. Authoritarian = political-regime 0 or 1; Democratic = political-regime 2 or 3."
      ),
      width = 90
    )
  ) +
  theme_void(base_size = 12) +
  theme(legend.position = "right") +
  caption_theme +
  scale_fill_manual(values = c("Authoritarian" = china_red_dark, "Democratic" = democracy_blue))

ggsave(
  filename = file.path(output_dir, "selection_model_latest_regime_family_pie.png"),
  plot = latest_country_regime_pie,
  width = 7,
  height = 5,
  dpi = 300
)

diagnostic_data_m2 <- data.frame(
  fitted = fitted(m2),
  residual = resid(m2),
  std_residual = rstandard(m2),
  cooks_distance = cooks.distance(m2)
)

residual_fitted_plot <- diagnostic_data_m2 %>%
  ggplot(aes(x = fitted, y = residual)) +
  geom_point(alpha = 0.35, color = china_red_dark) +
  geom_hline(yintercept = 0, linetype = "dashed", color = china_red) +
  geom_smooth(se = FALSE, color = china_red_light, linewidth = 1) +
  labs(
    title = "Residuals vs Fitted Values (M2)",
    x = "Fitted values",
    y = "Residuals",
    caption = str_wrap(
      paste(
        "Relational diagnostic figure. Residuals from M2 are plotted against fitted values",
        "to assess functional form and heteroskedasticity in the controlled OLS model."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_residuals_vs_fitted_m2.png"),
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
    title = "Normal Q-Q Plot of Standardized Residuals (M2)",
    x = "Theoretical quantiles",
    y = "Sample quantiles",
    caption = str_wrap(
      paste(
        "Relational diagnostic figure. Standardized residuals from M2 are compared with",
        "normal quantiles to assess departures from normality."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_qqplot_m2.png"),
  plot = qq_plot_m2,
  width = 8,
  height = 5,
  dpi = 300
)

cooks_plot_m2 <- cooks_tbl %>%
  filter(model == "M2_controls") %>%
  ggplot(aes(x = reorder(paste(entity, year, sep = "-"), cooks_distance), y = cooks_distance)) +
  geom_col(fill = china_red_deep) +
  geom_hline(yintercept = 4 / nrow(m2_data), linetype = "dashed", color = china_red_light) +
  coord_flip() +
  labs(
    title = "Top 10 Cook's Distance Observations (M2)",
    x = "Country-year",
    y = "Cook's distance",
    caption = str_wrap(
      paste(
        "Relational diagnostic figure. Bars show the 10 most influential observations in M2.",
        "The dashed line marks the common 4/n Cook's distance threshold."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_cooks_distance_m2.png"),
  plot = cooks_plot_m2,
  width = 8,
  height = 6,
  dpi = 300
)

assumption_summary_tbl <- bp_tbl %>%
  mutate(
    heteroskedasticity_flag = ifelse(p_value < 0.05, "Evidence of heteroskedasticity", "No BP evidence at 5%"),
    standard_error_treatment = case_when(
      model %in% c("M1_bivariate", "M2_controls") ~ "HC1 robust SE",
      model == "M3_country_year_FE" ~ "Cluster-robust SE by country",
      TRUE ~ NA_character_
    )
  )

write.csv(
  assumption_summary_tbl,
  file.path(output_dir, "selection_model_assumption_summary.csv"),
  row.names = FALSE
)

cat("Done. Outputs written to:", normalizePath(output_dir), "\n")
cat("Main files:\n")
cat("- selection_model_panel.csv\n")
cat("- selection_model_coefficients.csv\n")
cat("- selection_model_fitstats.csv\n")
cat("- selection_model_descriptive_stats.csv\n")
cat("- selection_model_descriptive_stats_by_regime.csv\n")
cat("- selection_model_sector_breakdown.csv\n")
cat("- selection_model_sector_top_recipients.csv\n")
cat("- selection_model_regression_table.txt/.html\n")
cat("- selection_model_coefficients_robust.csv\n")
cat("- selection_model_breusch_pagan.csv\n")
cat("- selection_model_cooks_distance_top10.csv\n")
cat("- selection_model_assumption_summary.csv\n")
cat("- selection_model_aid_scatter.png\n")
cat("- selection_model_regime_barplot.png\n")
cat("- selection_model_sector_barplot.png\n")
cat("- selection_model_regime_family_pie.png\n")
cat("- selection_model_latest_regime_family_pie.png\n")
cat("- selection_model_residuals_vs_fitted_m2.png\n")
cat("- selection_model_qqplot_m2.png\n")
cat("- selection_model_cooks_distance_m2.png\n")
cat("- selection_model_vif.csv\n")
