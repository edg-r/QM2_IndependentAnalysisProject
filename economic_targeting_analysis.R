# Economic-targeting model: does Chinese aid concentrate in economically
# strategic sectors more than in social sectors?

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(ggplot2)
library(stargazer)

options(scipen = 999)

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
  bind_rows(lapply(vars, function(v) {
    rhs <- setdiff(vars, v)
    fml <- as.formula(paste(v, "~", paste(rhs, collapse = " + ")))
    r2 <- summary(lm(fml, data = data))$r.squared
    data.frame(variable = v, vif = 1 / (1 - r2))
  }))
}

breusch_pagan_test <- function(model) {
  e2 <- resid(model)^2
  aux <- lm(e2 ~ model.matrix(model)[, -1, drop = FALSE])
  stat <- length(e2) * summary(aux)$r.squared
  df <- ncol(model.matrix(model)) - 1
  p_value <- pchisq(stat, df = df, lower.tail = FALSE)
  data.frame(statistic = unname(stat), df = df, p_value = p_value)
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

caption_theme <- theme(
  plot.caption = element_text(hjust = 0, size = 9, lineheight = 1.1),
  plot.caption.position = "plot",
  plot.margin = margin(12, 18, 18, 12)
)

ore_copper <- "#8C4B1F"
ore_gold <- "#C9862A"
social_teal <- "#1E6F73"
social_teal_soft <- "#8FC7C8"
slate_gray <- "#6B7280"
family_palette <- c(
  "Strategic" = ore_gold,
  "Social" = social_teal,
  "Other" = slate_gray
)

project_dir <- "."
aid_path <- file.path(project_dir, "chinese-aid-data-2000-2021.xlsx")
owid_path <- file.path(project_dir, "our-world-in-data-2013-2023.xlsx")
output_dir <- file.path(project_dir, "output", "economic_targeting")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

strategic_sector_codes <- c("320", "230", "210")
social_sector_codes <- c("110", "120", "130", "140", "150", "160")

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
  filter(recommended_for_aggregates == "Yes") %>%
  mutate(
    sector_code = ifelse(is.na(sector_code) | sector_code == "", "998", sector_code),
    sector_name = ifelse(
      is.na(sector_name) | sector_name == "",
      "UNALLOCATED/UNSPECIFIED",
      sector_name
    ),
    sector_family = case_when(
      sector_code %in% strategic_sector_codes ~ "Strategic",
      sector_code %in% social_sector_codes ~ "Social",
      TRUE ~ "Other"
    )
  )

family_breakdown_tbl <- aid_project_level %>%
  group_by(sector_family) %>%
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
    share_of_total_aid = total_aid_usd2021 / sum(total_aid_usd2021, na.rm = TRUE),
    sector_family = factor(sector_family, levels = c("Strategic", "Social", "Other"))
  ) %>%
  arrange(sector_family)

write.csv(
  family_breakdown_tbl,
  file.path(output_dir, "economic_targeting_family_breakdown.csv"),
  row.names = FALSE
)

sector_breakdown_tbl <- aid_project_level %>%
  group_by(sector_family, sector_code, sector_name) %>%
  summarize(
    total_aid_usd2021 = sum(adjusted_amount_usd2021, na.rm = TRUE),
    project_count = n(),
    funded_project_count = sum(!is.na(adjusted_amount_usd2021)),
    recipient_count = n_distinct(entity),
    .groups = "drop"
  ) %>%
  mutate(
    avg_funded_project_size_usd2021 = ifelse(
      funded_project_count > 0,
      total_aid_usd2021 / funded_project_count,
      NA_real_
    ),
    share_of_total_aid = total_aid_usd2021 / sum(total_aid_usd2021, na.rm = TRUE)
  ) %>%
  arrange(desc(total_aid_usd2021))

write.csv(
  sector_breakdown_tbl,
  file.path(output_dir, "economic_targeting_sector_breakdown.csv"),
  row.names = FALSE
)

family_top_recipients_tbl <- aid_project_level %>%
  filter(sector_family %in% c("Strategic", "Social")) %>%
  group_by(sector_family, entity) %>%
  summarize(
    total_aid_usd2021 = sum(adjusted_amount_usd2021, na.rm = TRUE),
    project_count = n(),
    funded_project_count = sum(!is.na(adjusted_amount_usd2021)),
    .groups = "drop"
  ) %>%
  group_by(sector_family) %>%
  arrange(desc(total_aid_usd2021), .by_group = TRUE) %>%
  mutate(rank_within_family = row_number()) %>%
  filter(rank_within_family <= 10) %>%
  ungroup()

write.csv(
  family_top_recipients_tbl,
  file.path(output_dir, "economic_targeting_family_top_recipients.csv"),
  row.names = FALSE
)

family_country_year_tbl <- aid_project_level %>%
  filter(sector_family %in% c("Strategic", "Social")) %>%
  group_by(entity, year, sector_family) %>%
  summarize(
    project_count = n(),
    funded_project_count = sum(!is.na(adjusted_amount_usd2021)),
    family_aid_usd2021 = ifelse(
      funded_project_count > 0,
      sum(adjusted_amount_usd2021, na.rm = TRUE),
      NA_real_
    ),
    family_aid_usd2021_unadjusted = ifelse(
      funded_project_count > 0,
      sum(amount_usd2021, na.rm = TRUE),
      NA_real_
    ),
    loan_share = safe_mean(flow_type_simplified == "Loan"),
    development_intent_share = safe_mean(intent == "Development"),
    .groups = "drop"
  ) %>%
  filter(!is.na(family_aid_usd2021), family_aid_usd2021 > 0)

country_year_coverage_tbl <- family_country_year_tbl %>%
  group_by(entity, year) %>%
  summarize(
    strategic_present = any(sector_family == "Strategic"),
    social_present = any(sector_family == "Social"),
    strategic_aid_usd2021 = sum(family_aid_usd2021[sector_family == "Strategic"], na.rm = TRUE),
    social_aid_usd2021 = sum(family_aid_usd2021[sector_family == "Social"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    coverage = case_when(
      strategic_present & social_present ~ "Both families funded",
      strategic_present & !social_present ~ "Strategic only",
      !strategic_present & social_present ~ "Social only",
      TRUE ~ "Neither"
    ),
    strategic_share_of_two_family_aid = ifelse(
      strategic_present & social_present,
      strategic_aid_usd2021 / (strategic_aid_usd2021 + social_aid_usd2021),
      NA_real_
    )
  )

write.csv(
  country_year_coverage_tbl,
  file.path(output_dir, "economic_targeting_country_year_coverage.csv"),
  row.names = FALSE
)

country_year_ratio_tbl <- country_year_coverage_tbl %>%
  filter(
    strategic_present,
    social_present,
    strategic_aid_usd2021 > 0,
    social_aid_usd2021 > 0
  ) %>%
  mutate(
    strategic_to_social_ratio = strategic_aid_usd2021 / social_aid_usd2021,
    log_ratio_strategic_to_social = log(strategic_to_social_ratio)
  )

write.csv(
  country_year_ratio_tbl,
  file.path(output_dir, "economic_targeting_country_year_ratios.csv"),
  row.names = FALSE
)

owid_raw <- read_excel(owid_path, sheet = "our-world-in-data", col_types = "text")

owid_panel <- owid_raw %>%
  transmute(
    entity = str_trim(entity),
    year = to_numeric(year),
    cpi = to_numeric(`ti-corruption-perception-index`),
    extreme_poverty = to_numeric(`share-of-population-in-extreme-poverty`),
    gini = to_numeric(`economic-inequality-gini-index`),
    gdp_pc = to_numeric(`gdp-per-capita-worldbank`)
  ) %>%
  filter(!is.na(entity), !is.na(year), year >= 2013, year <= 2021) %>%
  mutate(log_gdp_pc = ifelse(!is.na(gdp_pc) & gdp_pc > 0, log(gdp_pc), NA_real_))

analysis_panel <- family_country_year_tbl %>%
  left_join(owid_panel, by = c("entity", "year")) %>%
  mutate(
    log_family_aid = log1p(family_aid_usd2021),
    strategic_family = as.integer(sector_family == "Strategic"),
    entity_year = paste(entity, year, sep = "__")
  )

write.csv(
  analysis_panel,
  file.path(output_dir, "economic_targeting_panel.csv"),
  row.names = FALSE
)

desc_vars <- c(
  "family_aid_usd2021",
  "log_family_aid",
  "project_count",
  "loan_share",
  "development_intent_share",
  "strategic_family",
  "log_gdp_pc",
  "cpi",
  "extreme_poverty",
  "gini"
)

desc_tbl <- describe_vars(analysis_panel, desc_vars)
write.csv(
  desc_tbl,
  file.path(output_dir, "economic_targeting_descriptive_stats.csv"),
  row.names = FALSE
)

desc_by_family_tbl <- analysis_panel %>%
  group_by(sector_family) %>%
  summarize(
    observations = n(),
    countries = n_distinct(entity),
    mean_family_aid_usd2021 = mean(family_aid_usd2021, na.rm = TRUE),
    median_family_aid_usd2021 = median(family_aid_usd2021, na.rm = TRUE),
    mean_log_family_aid = mean(log_family_aid, na.rm = TRUE),
    mean_project_count = mean(project_count, na.rm = TRUE),
    mean_loan_share = mean(loan_share, na.rm = TRUE),
    mean_development_intent_share = mean(development_intent_share, na.rm = TRUE),
    mean_log_gdp_pc = mean(log_gdp_pc, na.rm = TRUE),
    mean_cpi = mean(cpi, na.rm = TRUE),
    mean_extreme_poverty = mean(extreme_poverty, na.rm = TRUE),
    mean_gini = mean(gini, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  desc_by_family_tbl,
  file.path(output_dir, "economic_targeting_descriptive_stats_by_family.csv"),
  row.names = FALSE
)

m1_data <- analysis_panel %>%
  filter(!is.na(log_family_aid), !is.na(strategic_family))

m1 <- lm(log_family_aid ~ strategic_family, data = m1_data)

m2_data <- analysis_panel %>%
  filter(
    !is.na(log_family_aid),
    !is.na(strategic_family),
    !is.na(log_gdp_pc),
    !is.na(cpi),
    !is.na(extreme_poverty),
    !is.na(gini)
  )

m2 <- lm(
  log_family_aid ~ strategic_family + log_gdp_pc + cpi + extreme_poverty + gini,
  data = m2_data
)

m3 <- lm(
  log_family_aid ~ strategic_family + log_gdp_pc + cpi + extreme_poverty + gini +
    factor(entity) + factor(year),
  data = m2_data
)

m4_data <- analysis_panel %>%
  group_by(entity, year) %>%
  filter(n_distinct(sector_family) == 2) %>%
  ungroup()

m4 <- lm(
  log_family_aid ~ strategic_family + factor(entity_year),
  data = m4_data
)

main_controls <- c("strategic_family", "log_gdp_pc", "cpi", "extreme_poverty", "gini")
vif_tbl <- compute_vif(m2_data, main_controls)
write.csv(vif_tbl, file.path(output_dir, "economic_targeting_vif.csv"), row.names = FALSE)

bp_tbl <- bind_rows(
  breusch_pagan_test(m1) %>% mutate(model = "M1_bivariate"),
  breusch_pagan_test(m2) %>% mutate(model = "M2_controls"),
  breusch_pagan_test(m3) %>% mutate(model = "M3_country_year_FE"),
  breusch_pagan_test(m4) %>% mutate(model = "M4_country_year_pair_FE")
) %>%
  select(model, statistic, df, p_value)

write.csv(
  bp_tbl,
  file.path(output_dir, "economic_targeting_breusch_pagan.csv"),
  row.names = FALSE
)

vcov_m1_hc1 <- vcov_hc1(m1)
vcov_m2_hc1 <- vcov_hc1(m2)
vcov_m3_cluster_entity <- vcov_cluster(m3, m2_data$entity)
vcov_m4_cluster_entity <- vcov_cluster(m4, m4_data$entity)

robust_coef_tbl <- bind_rows(
  tidy_with_vcov(m1, vcov_m1_hc1, "M1_bivariate_HC1"),
  tidy_with_vcov(m2, vcov_m2_hc1, "M2_controls_HC1"),
  tidy_with_vcov(m3, vcov_m3_cluster_entity, "M3_country_year_FE_cluster_entity"),
  tidy_with_vcov(m4, vcov_m4_cluster_entity, "M4_country_year_pair_FE_cluster_entity")
)

write.csv(
  robust_coef_tbl,
  file.path(output_dir, "economic_targeting_coefficients_robust.csv"),
  row.names = FALSE
)

cooks_tbl <- bind_rows(
  cooks_distance_table(m1, m1_data, "M1_bivariate"),
  cooks_distance_table(m2, m2_data, "M2_controls"),
  cooks_distance_table(m3, m2_data, "M3_country_year_FE"),
  cooks_distance_table(m4, m4_data, "M4_country_year_pair_FE")
)

write.csv(
  cooks_tbl,
  file.path(output_dir, "economic_targeting_cooks_distance_top10.csv"),
  row.names = FALSE
)

coef_tbl <- bind_rows(
  tidy(m1, conf.int = TRUE) %>% mutate(model = "M1_bivariate"),
  tidy(m2, conf.int = TRUE) %>% mutate(model = "M2_controls"),
  tidy(m3, conf.int = TRUE) %>% mutate(model = "M3_country_year_FE"),
  tidy(m4, conf.int = TRUE) %>% mutate(model = "M4_country_year_pair_FE")
)

write.csv(
  coef_tbl,
  file.path(output_dir, "economic_targeting_coefficients.csv"),
  row.names = FALSE
)

fit_tbl <- bind_rows(
  glance(m1) %>% mutate(model = "M1_bivariate"),
  glance(m2) %>% mutate(model = "M2_controls"),
  glance(m3) %>% mutate(model = "M3_country_year_FE"),
  glance(m4) %>% mutate(model = "M4_country_year_pair_FE")
)

write.csv(
  fit_tbl,
  file.path(output_dir, "economic_targeting_fitstats.csv"),
  row.names = FALSE
)

invisible(capture.output(
  stargazer(
    m1, m2, m3, m4,
    type = "text",
    title = "Chinese Aid and Sector Targeting (Robust Standard Errors)",
    dep.var.labels = "Log(1 + family-level Chinese aid in constant USD 2021)",
    column.labels = c("Bivariate", "Controls", "Country + Year FE", "Country-Year FE"),
    covariate.labels = c(
      "Strategic sector family",
      "Log GDP per capita",
      "Corruption Perceptions Index",
      "Extreme poverty share",
      "Gini index"
    ),
    omit = c("factor\\(entity\\)", "factor\\(year\\)", "factor\\(entity_year\\)"),
    omit.stat = c("f", "ser"),
    add.lines = list(
      c("Country fixed effects", "No", "No", "Yes", "No"),
      c("Year fixed effects", "No", "No", "Yes", "No"),
      c("Country-year fixed effects", "No", "No", "No", "Yes"),
      c("Standard errors", "HC1", "HC1", "Clustered by country", "Clustered by country")
    ),
    se = list(
      sqrt(diag(vcov_m1_hc1)),
      sqrt(diag(vcov_m2_hc1)),
      sqrt(diag(vcov_m3_cluster_entity)),
      sqrt(diag(vcov_m4_cluster_entity))
    ),
    out = file.path(output_dir, "economic_targeting_regression_table.txt")
  )
))

invisible(capture.output(
  stargazer(
    m1, m2, m3, m4,
    type = "html",
    title = "Chinese Aid and Sector Targeting (Robust Standard Errors)",
    dep.var.labels = "Log(1 + family-level Chinese aid in constant USD 2021)",
    column.labels = c("Bivariate", "Controls", "Country + Year FE", "Country-Year FE"),
    covariate.labels = c(
      "Strategic sector family",
      "Log GDP per capita",
      "Corruption Perceptions Index",
      "Extreme poverty share",
      "Gini index"
    ),
    omit = c("factor\\(entity\\)", "factor\\(year\\)", "factor\\(entity_year\\)"),
    omit.stat = c("f", "ser"),
    add.lines = list(
      c("Country fixed effects", "No", "No", "Yes", "No"),
      c("Year fixed effects", "No", "No", "Yes", "No"),
      c("Country-year fixed effects", "No", "No", "No", "Yes"),
      c("Standard errors", "HC1", "HC1", "Clustered by country", "Clustered by country")
    ),
    se = list(
      sqrt(diag(vcov_m1_hc1)),
      sqrt(diag(vcov_m2_hc1)),
      sqrt(diag(vcov_m3_cluster_entity)),
      sqrt(diag(vcov_m4_cluster_entity))
    ),
    out = file.path(output_dir, "economic_targeting_regression_table.html")
  )
))

invisible(capture.output(
  stargazer(
    desc_tbl,
    type = "text",
    summary = FALSE,
    title = "Descriptive Statistics",
    out = file.path(output_dir, "economic_targeting_descriptive_stats.txt")
  )
))

invisible(capture.output(
  stargazer(
    desc_tbl,
    type = "html",
    summary = FALSE,
    title = "Descriptive Statistics",
    out = file.path(output_dir, "economic_targeting_descriptive_stats.html")
  )
))

family_barplot <- family_breakdown_tbl %>%
  mutate(
    sector_family = factor(sector_family, levels = c("Strategic", "Social", "Other")),
    aid_billions_usd2021 = total_aid_usd2021 / 1000000000,
    share_label = paste0(round(share_of_total_aid * 100, 1), "% of total aid")
  ) %>%
  ggplot(aes(x = sector_family, y = aid_billions_usd2021, fill = sector_family)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = share_label), vjust = -0.4, size = 3.6) +
  labs(
    title = "Chinese Aid Is Concentrated in Economically Strategic Sectors",
    x = "Sector family",
    y = "Adjusted aid, billions of constant USD 2021",
    caption = str_wrap(
      paste(
        "Strategic sectors are industry/mining/construction, energy, and transport/storage.",
        "Social sectors are education, health, population/reproductive health, water/sanitation,",
        "government/civil society, and other social infrastructure. AidData records are limited",
        "to projects marked Recommended For Aggregates = Yes, 2013-2021."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = family_palette, guide = "none") +
  caption_theme

ggsave(
  filename = file.path(output_dir, "economic_targeting_family_barplot.png"),
  plot = family_barplot,
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
  ggplot(aes(x = sector_name, y = aid_billions_usd2021, fill = sector_family)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Chinese Aid Earmarks by Sector (2013-2021)",
    x = "Sector",
    y = "Adjusted aid, billions of constant USD 2021",
    caption = str_wrap(
      paste(
        "Top sectors are ranked by total adjusted aid over 2013-2021.",
        "Colors show whether each sector is coded as strategic, social, or other in the new analysis."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = family_palette) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "economic_targeting_sector_barplot.png"),
  plot = sector_barplot,
  width = 9,
  height = 6,
  dpi = 300
)

family_boxplot <- analysis_panel %>%
  mutate(sector_family = factor(sector_family, levels = c("Social", "Strategic"))) %>%
  ggplot(aes(x = sector_family, y = log_family_aid, fill = sector_family)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.18) +
  geom_jitter(width = 0.15, alpha = 0.16, size = 1.2, color = "black") +
  labs(
    title = "Strategic Commitments Are Larger Than Social Commitments",
    x = "Funded sector family",
    y = "Log(1 + family-level Chinese aid in constant USD 2021)",
    caption = str_wrap(
      paste(
        "Each point is a funded country-year-sector-family observation.",
        "The main regression sample compares the size of positive strategic and social allocations."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = family_palette[c("Social", "Strategic")], guide = "none") +
  caption_theme

ggsave(
  filename = file.path(output_dir, "economic_targeting_family_boxplot.png"),
  plot = family_boxplot,
  width = 8,
  height = 5,
  dpi = 300
)

ratio_hist <- country_year_ratio_tbl %>%
  ggplot(aes(x = log_ratio_strategic_to_social)) +
  geom_histogram(binwidth = 0.5, fill = ore_copper, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = social_teal, linewidth = 1) +
  labs(
    title = "Within the Same Country-Year, Strategic Aid Usually Exceeds Social Aid",
    x = "Log strategic-to-social aid ratio",
    y = "Country-years",
    caption = str_wrap(
      paste(
        "Only country-years with both strategic and social funding are included.",
        "Values above zero indicate that strategic-sector commitments exceed social-sector commitments."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "economic_targeting_country_year_ratio_hist.png"),
  plot = ratio_hist,
  width = 8,
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
  geom_point(alpha = 0.35, color = ore_copper) +
  geom_hline(yintercept = 0, linetype = "dashed", color = social_teal) +
  geom_smooth(se = FALSE, color = ore_gold, linewidth = 1) +
  labs(
    title = "Residuals vs Fitted Values (M2)",
    x = "Fitted values",
    y = "Residuals",
    caption = str_wrap(
      paste(
        "Diagnostic plot for the controlled model. Residuals are plotted against fitted values",
        "to assess functional form and heteroskedasticity."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "economic_targeting_residuals_vs_fitted_m2.png"),
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
  geom_point(alpha = 0.35, color = ore_copper) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = social_teal) +
  labs(
    title = "Normal Q-Q Plot of Standardized Residuals (M2)",
    x = "Theoretical quantiles",
    y = "Sample quantiles",
    caption = str_wrap(
      paste(
        "Diagnostic plot for the controlled model. Standardized residuals are compared with",
        "normal quantiles to assess departures from normality."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "economic_targeting_qqplot_m2.png"),
  plot = qq_plot_m2,
  width = 8,
  height = 5,
  dpi = 300
)

cooks_plot_m2 <- cooks_tbl %>%
  filter(model == "M2_controls") %>%
  ggplot(aes(x = reorder(paste(entity, year, sep = "-"), cooks_distance), y = cooks_distance)) +
  geom_col(fill = ore_copper) +
  geom_hline(yintercept = 4 / nrow(m2_data), linetype = "dashed", color = social_teal_soft) +
  coord_flip() +
  labs(
    title = "Top 10 Cook's Distance Observations (M2)",
    x = "Country-year",
    y = "Cook's distance",
    caption = str_wrap(
      paste(
        "Bars show the most influential observations in the controlled model.",
        "The dashed line marks the common 4/n Cook's distance threshold."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "economic_targeting_cooks_distance_m2.png"),
  plot = cooks_plot_m2,
  width = 8,
  height = 6,
  dpi = 300
)

assumption_summary_tbl <- bp_tbl %>%
  mutate(
    heteroskedasticity_flag = ifelse(
      p_value < 0.05,
      "Evidence of heteroskedasticity",
      "No BP evidence at 5%"
    ),
    standard_error_treatment = case_when(
      model %in% c("M1_bivariate", "M2_controls") ~ "HC1 robust SE",
      model %in% c("M3_country_year_FE", "M4_country_year_pair_FE") ~ "Cluster-robust SE by country",
      TRUE ~ NA_character_
    )
  )

write.csv(
  assumption_summary_tbl,
  file.path(output_dir, "economic_targeting_assumption_summary.csv"),
  row.names = FALSE
)

cat("Done. Outputs written to:", normalizePath(output_dir), "\n")
