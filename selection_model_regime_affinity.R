#!/usr/bin/env Rscript

# Selection model: Do countries with more authoritarian regimes receive more
# Chinese aid? This script builds a country-year panel and runs progressive
# regressions for your QM2 memo.

required_pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "broom", "ggplot2")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    paste(
      "Missing required packages:",
      paste(missing_pkgs, collapse = ", "),
      "\nInstall them first, then rerun."
    )
  )
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(broom)
library(ggplot2)

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
    autocracy_score = 3 - political_regime
  )

write.csv(analysis_panel, file.path(output_dir, "selection_model_panel.csv"), row.names = FALSE)

# -----------------------------
# 4) Regressions (progressive specs)
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
# 5) Diagnostics and outputs
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

# Quick descriptive plot for memo figure draft
p <- analysis_panel %>%
  filter(!is.na(political_regime)) %>%
  group_by(political_regime) %>%
  summarize(
    avg_log_aid = mean(log_china_aid, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = factor(political_regime), y = avg_log_aid)) +
  geom_col(fill = "#2C7FB8") +
  labs(
    title = "Average Chinese Aid by Regime Type (2013-2021)",
    x = "Political regime (0=closed autocracy ... 3=liberal democracy)",
    y = "Average log(1 + Chinese aid in constant USD 2021)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(output_dir, "selection_model_regime_barplot.png"),
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)

cat("Done. Outputs written to:", normalizePath(output_dir), "\n")
cat("Main files:\n")
cat("- selection_model_panel.csv\n")
cat("- selection_model_coefficients.csv\n")
cat("- selection_model_fitstats.csv\n")
cat("- selection_model_vif.csv\n")
cat("- selection_model_regime_barplot.png\n")
