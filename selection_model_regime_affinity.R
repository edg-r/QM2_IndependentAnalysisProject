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

aid_panel <- aid_raw %>%
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
  
  # AidData guidance: use records recommended for aggregation to avoid
  # double-counting and to drop cancelled/suspended/pledge-only records.
  filter(recommended_for_aggregates == "Yes") %>%
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
    title = "Chinese Aid and Regime Type",
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
      c("Year fixed effects", "No", "No", "Yes")
    ),
    out = file.path(output_dir, "selection_model_regression_table.txt")
  )
))

invisible(capture.output(
  stargazer(
    m1, m2, m3,
    type = "html",
    title = "Chinese Aid and Regime Type",
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
      c("Year fixed effects", "No", "No", "Yes")
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
        "Closed autocracy",
        "Electoral autocracy",
        "Electoral democracy",
        "Liberal democracy"
      )
    )
  ) %>%
  ggplot(aes(x = regime_label, y = avg_log_aid)) +
  geom_col(fill = "#2C7FB8") +
  labs(
    title = "Average Chinese Aid by Regime Type (2013-2021)",
    x = "Regime type",
    y = "Average log(1 + Chinese aid in constant USD 2021)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "selection_model_regime_barplot.png"),
  plot = regime_barplot,
  width = 8,
  height = 5,
  dpi = 300
)

aid_scatter <- analysis_panel %>%
  filter(!is.na(autocracy_score), !is.na(log_china_aid)) %>%
  ggplot(aes(x = autocracy_score, y = log_china_aid)) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.25, color = "#1B4332") +
  geom_smooth(method = "lm", se = TRUE, color = "#D62828", linewidth = 1) +
  scale_x_continuous(breaks = 0:3) +
  labs(
    title = "Bivariate Relationship Between Autocracy and Chinese Aid",
    x = "Autocracy score (higher = more authoritarian)",
    y = "Log(1 + Chinese aid in constant USD 2021)"
  ) +
  theme_minimal(base_size = 12)

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
    fill = "Regime family"
  ) +
  theme_void(base_size = 12) +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("Authoritarian" = "#9D0208", "Democratic" = "#005F73"))

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
    fill = "Regime family"
  ) +
  theme_void(base_size = 12) +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("Authoritarian" = "#AE2012", "Democratic" = "#005F73"))

ggsave(
  filename = file.path(output_dir, "selection_model_latest_regime_family_pie.png"),
  plot = latest_country_regime_pie,
  width = 7,
  height = 5,
  dpi = 300
)

cat("Done. Outputs written to:", normalizePath(output_dir), "\n")
cat("Main files:\n")
cat("- selection_model_panel.csv\n")
cat("- selection_model_coefficients.csv\n")
cat("- selection_model_fitstats.csv\n")
cat("- selection_model_descriptive_stats.csv\n")
cat("- selection_model_descriptive_stats_by_regime.csv\n")
cat("- selection_model_regression_table.txt/.html\n")
cat("- selection_model_aid_scatter.png\n")
cat("- selection_model_regime_barplot.png\n")
cat("- selection_model_regime_family_pie.png\n")
cat("- selection_model_latest_regime_family_pie.png\n")
cat("- selection_model_vif.csv\n")
