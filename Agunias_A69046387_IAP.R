# GPCO 454 - Quantitative Methods II
# Independent Analysis Project
# Country-Level Cross-Section Script

# ---------------------------
# Research Question
# ---------------------------

# This script studies whether more autocratic countries receive more Chinese aid.

# ---------------------------
# Preliminaries
# ---------------------------

#setwd('/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/IAP')

# Load the packages used throughout the script so all dependencies are visible
# before the analysis begins.
#library(readxl)
#library(dplyr)
#library(tidyr)
library(stringr)
library(broom)
#library(ggplot2)
#library(stargazer)

# Reduce scientific notation in printed tables and console output.
options(scipen = 999)

# Store a shared caption theme so every exported figure uses the same spacing and
# caption formatting.
caption_theme <- theme(
  plot.caption = element_text(hjust = 0, size = 9, lineheight = 1.1),
  plot.caption.position = "plot",
  plot.margin = margin(12, 18, 24, 44)
)

# Define the project color palette once so descriptive and diagnostic plots stay
# visually consistent across the script.
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

# ---------------------------
# Helper Functions
# ---------------------------

# Convert imported text values to numeric while treating ".", blanks, and string
# versions of missing values as actual missing data.
to_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", ".", "NA", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

# Compute means safely so all-missing vectors remain `NA` instead of collapsing to
# misleading values.
safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

# Recover the most common non-missing category when collapsing repeated yearly
# labels to a single country-level label.
safe_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  names(sort(table(x), decreasing = TRUE))[1]
}

# Build a compact descriptive-statistics table for the country-level variables used
# in the main analysis.
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

# Calculate variance inflation factors manually so the script can export a simple,
# self-contained multicollinearity check.
compute_vif <- function(data, vars) {
  out <- lapply(vars, function(v) {
    rhs <- setdiff(vars, v)
    fml <- as.formula(paste(v, "~", paste(rhs, collapse = " + ")))
    r2 <- summary(lm(fml, data = data))$r.squared
    data.frame(variable = v, vif = 1 / (1 - r2))
  })
  bind_rows(out)
}

# Format p-values consistently before placing them in summary tables and captions.
format_p_value <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x))
  )
}

# Escape HTML-sensitive characters before writing custom HTML tables.
html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# Write a lightweight HTML table for exports that are easier to control manually
# than through stargazer.
write_simple_html_table <- function(tbl, title, subtitle = NULL, out) {
  header_cells <- paste0("<th>", html_escape(names(tbl)), "</th>", collapse = "")
  body_rows <- apply(tbl, 1, function(row) {
    cells <- paste0("<td>", html_escape(row), "</td>", collapse = "")
    paste0("<tr>", cells, "</tr>")
  })

  subtitle_html <- ""
  if (!is.null(subtitle) && nzchar(subtitle)) {
    subtitle_html <- paste0("<p class=\"subtitle\">", html_escape(subtitle), "</p>")
  }

  html <- c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "  <meta charset=\"utf-8\">",
    paste0("  <title>", html_escape(title), "</title>"),
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 32px; color: #1f2933; }",
    "    h1 { font-size: 24px; margin-bottom: 8px; }",
    "    .subtitle { margin-top: 0; margin-bottom: 18px; color: #52606d; max-width: 900px; }",
    "    table { border-collapse: collapse; width: 100%; max-width: 1100px; }",
    "    th, td { border: 1px solid #d9e2ec; padding: 10px 12px; text-align: left; vertical-align: top; }",
    "    th { background: #f0f4f8; font-weight: 700; }",
    "    tr:nth-child(even) { background: #f8fafc; }",
    "  </style>",
    "</head>",
    "<body>",
    paste0("  <h1>", html_escape(title), "</h1>"),
    paste0("  ", subtitle_html),
    "  <table>",
    paste0("    <thead><tr>", header_cells, "</tr></thead>"),
    paste0("    <tbody>", paste(body_rows, collapse = ""), "</tbody>"),
    "  </table>",
    "</body>",
    "</html>"
  )

  writeLines(html, out)
}

# Standardize numeric formatting before inserting data into HTML exports.
format_numeric_columns <- function(tbl, digits = 3) {
  tbl[] <- lapply(tbl, function(col) {
    if (is.numeric(col)) {
      sprintf(paste0("%.", digits, "f"), col)
    } else {
      as.character(col)
    }
  })
  tbl
}

# Gather the main outlier diagnostics for the controlled cross-section model in one
# reusable object.
compute_outlier_diagnostics <- function(model, data, id_cols) {
  n <- nobs(model)
  k <- length(coef(model)) - 1
  leverage_cutoff <- 2 * (k + 1) / n
  cooks_cutoff <- 4 / (n - k - 1)
  dffits_cutoff <- 2 * sqrt(k / n)

  ids <- data[id_cols]
  diag_tbl <- data.frame(
    ids,
    studentized_residual = rstudent(model),
    leverage = hatvalues(model),
    cooks_distance = cooks.distance(model),
    dffits = dffits(model),
    row.names = NULL
  ) %>%
    mutate(
      abs_studentized_residual = abs(studentized_residual),
      abs_dffits = abs(dffits),
      outlier_studentized = abs_studentized_residual > 2,
      outlier_leverage = leverage > leverage_cutoff,
      outlier_cooks = cooks_distance > cooks_cutoff,
      outlier_dffits = abs_dffits > dffits_cutoff,
      outlier_any = outlier_studentized | outlier_leverage | outlier_cooks | outlier_dffits,
      outlier_all = outlier_studentized & outlier_leverage & outlier_cooks & outlier_dffits
    )

  summary_tbl <- data.frame(
    Metric = c(
      "Model observations",
      "k (predictors)",
      "Threshold |studentized residual|",
      "Threshold leverage",
      "Threshold Cook's D",
      "Threshold |DFFITS|",
      "Flagged outliers (any threshold)",
      "Flagged egregious outliers (all thresholds)"
    ),
    Value = c(
      as.character(n),
      as.character(k),
      "> 2",
      sprintf("> %.6f", leverage_cutoff),
      sprintf("> %.6f", cooks_cutoff),
      sprintf("> %.6f", dffits_cutoff),
      as.character(sum(diag_tbl$outlier_any, na.rm = TRUE)),
      as.character(sum(diag_tbl$outlier_all, na.rm = TRUE))
    ),
    check.names = FALSE
  )

  list(
    summary_tbl = summary_tbl,
    diagnostics_tbl = diag_tbl,
    leverage_cutoff = leverage_cutoff,
    dffits_cutoff = dffits_cutoff
  )
}

# Build a single HTML diagnostics page so the model checks are exported together.
write_diagnostics_html <- function(
  vif_tbl,
  bp_tbl,
  diagnostic_tbl,
  influence_tbl,
  outlier_summary_tbl,
  flagged_outliers_tbl,
  title,
  out
) {
  vif_html <- write_simple_html_fragment(format_numeric_columns(vif_tbl))
  bp_html <- write_simple_html_fragment(format_numeric_columns(bp_tbl))
  diagnostic_html <- write_simple_html_fragment(format_numeric_columns(diagnostic_tbl))
  influence_html <- write_simple_html_fragment(format_numeric_columns(influence_tbl))
  outlier_summary_html <- write_simple_html_fragment(outlier_summary_tbl)
  flagged_outliers_html <- write_simple_html_fragment(format_numeric_columns(flagged_outliers_tbl))

  html <- c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "  <meta charset=\"utf-8\">",
    paste0("  <title>", html_escape(title), "</title>"),
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 32px; color: #1f2933; }",
    "    h1 { font-size: 24px; margin-bottom: 16px; }",
    "    h2 { font-size: 18px; margin: 24px 0 10px; }",
    "    .table-wrap { max-width: 1100px; max-height: 520px; overflow: auto; border: 1px solid #d9e2ec; }",
    "    table { border-collapse: collapse; width: 100%; }",
    "    th, td { border: 1px solid #d9e2ec; padding: 8px 10px; text-align: left; vertical-align: top; font-size: 13px; }",
    "    th { background: #f0f4f8; font-weight: 700; position: sticky; top: 0; }",
    "    tr:nth-child(even) { background: #f8fafc; }",
    "  </style>",
    "</head>",
    "<body>",
    paste0("  <h1>", html_escape(title), "</h1>"),
    "  <h2>Outlier Summary</h2>",
    paste0("  <div class=\"table-wrap\">", outlier_summary_html, "</div>"),
    "  <h2>Flagged Outliers</h2>",
    paste0("  <div class=\"table-wrap\">", flagged_outliers_html, "</div>"),
    "  <h2>VIF Values</h2>",
    paste0("  <div class=\"table-wrap\">", vif_html, "</div>"),
    "  <h2>Breusch-Pagan Test</h2>",
    paste0("  <div class=\"table-wrap\">", bp_html, "</div>"),
    "  <h2>Fitted Values and Residuals</h2>",
    paste0("  <div class=\"table-wrap\">", diagnostic_html, "</div>"),
    "  <h2>Top Cook's Distance Values</h2>",
    paste0("  <div class=\"table-wrap\">", influence_html, "</div>"),
    "</body>",
    "</html>"
  )

  writeLines(html, out)
}

# Create the reusable HTML fragment inserted into each section of the diagnostics
# export.
write_simple_html_fragment <- function(tbl) {
  header_cells <- paste0("<th>", html_escape(names(tbl)), "</th>", collapse = "")
  body_rows <- apply(tbl, 1, function(row) {
    cells <- paste0("<td>", html_escape(row), "</td>", collapse = "")
    paste0("<tr>", cells, "</tr>")
  })

  paste0(
    "<table><thead><tr>", header_cells, "</tr></thead><tbody>",
    paste(body_rows, collapse = ""),
    "</tbody></table>"
  )
}

# Compute HC1 robust variance estimates while keeping the core models as standard
# OLS objects for diagnostics and plotting.
vcov_hc1 <- function(model) {
  X <- model.matrix(model)
  u <- resid(model)
  n <- nrow(X)
  k <- ncol(X)
  bread <- solve(crossprod(X))
  meat <- crossprod(X, diag(u^2, nrow = n) %*% X)
  (n / (n - k)) * bread %*% meat %*% bread
}

# Convert an `lm` model plus a supplied covariance matrix into a tidy coefficient
# table ready for CSV export.
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

# Run a simple Breusch-Pagan test so the script can report heteroskedasticity
# diagnostics for each model.
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

# Pull the most influential observations from each model for the Cook's distance
# exports.
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

# ---------------------------
# Project Paths and Output Folders
# ---------------------------

# Define the main file paths once so later sections can reuse them cleanly.
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


# ---------------------------
# Load Datasets
# ---------------------------

# Read the raw spreadsheets before any transformations, mirroring the sample
# script's "load first, transform second" structure.
aid_raw <- read_excel(aid_path, sheet = "aid-data", col_types = "text")
owid_raw <- read_excel(owid_path, sheet = "our-world-in-data", col_types = "text")

# ---------------------------
# Processing Chinese Aid Data (2013-2021)
# ---------------------------

# Start from project-level AidData records, keep observations appropriate for
# aggregation, and retain only the fields needed for the country-level analysis.
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

# Collapse the project-level aid records directly to one row per country so the
# rest of the script never depends on a country-year dataset.
aid_country_summary <- aid_project_level %>%
  group_by(entity) %>%
  summarize(
    years_with_aid = n_distinct(year),
    china_aid_usd2021_total = sum(adjusted_amount_usd2021, na.rm = TRUE),
    china_aid_usd2021_unadjusted_total = sum(amount_usd2021, na.rm = TRUE),
    china_project_count_total = n(),
    china_loan_share = safe_mean(flow_type_simplified == "Loan"),
    china_development_intent_share = safe_mean(intent == "Development"),
    .groups = "drop"
  ) %>%
  mutate(
    china_aid_usd2021_total = ifelse(is.nan(china_aid_usd2021_total), NA_real_, china_aid_usd2021_total),
    china_aid_usd2021_unadjusted_total = ifelse(
      is.nan(china_aid_usd2021_unadjusted_total),
      NA_real_,
      china_aid_usd2021_unadjusted_total
    )
  )

# Export a one-row summary of the total aid portfolio to keep the same descriptive
# deliverable the current script already produces.
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

# Rank the top recipient countries by total aid so the later descriptive figure can
# read directly from this table.
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

# ---------------------------
# Transforming our_world_data into a Cross-Country Dataset
# ---------------------------

# Clean the OWID values and collapse them directly to country-level averages.
owid_country_cross_section <- owid_raw %>%
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
  filter(!is.na(entity), !is.na(year), year >= 2013, year <= 2021) %>%
  group_by(entity) %>%
  summarize(
    years_in_sample = n_distinct(year),
    political_regime = safe_mean(political_regime),
    democracy_index = safe_mean(democracy_index),
    cpi = safe_mean(cpi),
    extreme_poverty = safe_mean(extreme_poverty),
    gini = safe_mean(gini),
    gdp_pc = safe_mean(gdp_pc),
    modal_regime = safe_mode(case_when(
      political_regime == 0 ~ "Closed autocracy",
      political_regime == 1 ~ "Electoral autocracy",
      political_regime == 2 ~ "Electoral democracy",
      political_regime == 3 ~ "Liberal democracy",
      TRUE ~ NA_character_
    )),
    modal_regime_family = safe_mode(case_when(
      political_regime %in% c(0, 1) ~ "Authoritarian",
      political_regime %in% c(2, 3) ~ "Democratic",
      TRUE ~ NA_character_
    )),
    .groups = "drop"
  )

# ---------------------------
# Merging All Datasets into a Single Cross-Country Dataset
# ---------------------------
# Merge the country-level aid summary with the country-level covariate summary and
# create the transformed variables used in the country-level analysis.
country_cross_section <- owid_country_cross_section %>%
  left_join(aid_country_summary, by = "entity") %>%
  mutate(
    years_with_aid = replace_na(years_with_aid, 0L),
    china_aid_usd2021_total = replace_na(china_aid_usd2021_total, 0),
    china_aid_usd2021_unadjusted_total = replace_na(china_aid_usd2021_unadjusted_total, 0),
    china_project_count_total = replace_na(china_project_count_total, 0L),
    china_aid_usd2021 = ifelse(years_in_sample > 0, china_aid_usd2021_total / years_in_sample, NA_real_),
    china_project_count = ifelse(years_in_sample > 0, china_project_count_total / years_in_sample, NA_real_),
    aid_any = as.integer(years_with_aid > 0),
    log_china_aid = log1p(china_aid_usd2021),
    log_china_aid_total = log1p(china_aid_usd2021_total),
    log_gdp_pc = ifelse(!is.na(gdp_pc) & gdp_pc > 0, log(gdp_pc), NA_real_),
    autocracy_score = 3 - political_regime,
    regime_family = case_when(
      political_regime %in% c(0, 1) ~ "Authoritarian",
      political_regime %in% c(2, 3) ~ "Democratic",
      TRUE ~ modal_regime_family
    ),
    regime_label = case_when(
      round(political_regime) == 0 ~ "Closed autocracy",
      round(political_regime) == 1 ~ "Electoral autocracy",
      round(political_regime) == 2 ~ "Electoral democracy",
      round(political_regime) == 3 ~ "Liberal democracy",
      TRUE ~ modal_regime
    )
  )

write.csv(
  country_cross_section,
  file.path(output_dir, "selection_model_country_cross_section.csv"),
  row.names = FALSE
)

# ---------------------------
# View and Save Final Merged Dataset
# ---------------------------

# List the variables that will appear in the country-level descriptive table.
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

# Summarize the same country-level variables by regime family to show how the
# descriptive profile differs across democratic and authoritarian countries.
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

# ---------------------------
# Transform Outcome Variable: Log(1 + Aid)
# ---------------------------

# Build the clean estimation samples up front, following the same progressive logic
# used in the sample script.
cs_m1_data <- country_cross_section %>%
  filter(!is.na(log_china_aid), !is.na(autocracy_score))

# Estimate the simple bivariate relationship between autocracy and Chinese aid.
cs_m1 <- lm(log_china_aid ~ autocracy_score, data = cs_m1_data)

# Restrict the main model to countries with complete information on the full set of
# controls.
cs_m2_data <- country_cross_section %>%
  filter(
    !is.na(log_china_aid),
    !is.na(autocracy_score),
    !is.na(log_gdp_pc),
    !is.na(cpi),
    !is.na(extreme_poverty),
    !is.na(gini)
  )

# Estimate the main controlled country-level model.
cs_m2 <- lm(
  log_china_aid ~ autocracy_score + log_gdp_pc + cpi + extreme_poverty + gini,
  data = cs_m2_data
)

# Create a recipient-only sample to check whether the main result depends on the
# countries with zero observed aid.
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

# Re-estimate the controlled model on the recipient-only sample.
cs_m2_positive_aid <- lm(
  log_china_aid ~ autocracy_score + log_gdp_pc + cpi + extreme_poverty + gini,
  data = cs_m2_positive_aid_data
)

# ---------------------------
# Rerun Regression Analysis Using Log(1 + Aid)
# ---------------------------

# Check multicollinearity in the control set before exporting the model results.
main_controls <- c("autocracy_score", "log_gdp_pc", "cpi", "extreme_poverty", "gini")
vif_tbl <- compute_vif(cs_m2_data, main_controls)
write.csv(
  vif_tbl,
  file.path(output_dir, "selection_model_country_vif.csv"),
  row.names = FALSE
)

# Compute HC1 robust uncertainty estimates so the regression exports use the same
# inference approach throughout the project.
vcov_cs_m1_hc1 <- vcov_hc1(cs_m1)
vcov_cs_m2_hc1 <- vcov_hc1(cs_m2)
vcov_cs_m2_positive_aid_hc1 <- vcov_hc1(cs_m2_positive_aid)

# Export the robust coefficient table used for summary reporting within the
# cross-country analysis.
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

# Export the conventional coefficient table with confidence intervals.
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

# Export model fit statistics for the three country-level specifications.
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

# Export Breusch-Pagan test results for each specification.
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

# Save the most influential observations from each model for later diagnostics.
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

# Write the main regression table in text and HTML formats.
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

# Export the descriptive table in text and HTML formats for the write-up.
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

# ---------------------------
# Scatter Plot of Aid and Regime Type
# ---------------------------

# Plot average country-level aid by regime type to visualize the descriptive pattern
# behind the regression results.
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
    y = "Average log(1 + mean annual Chinese aid,\nconstant USD 2021)",
    caption = str_wrap(
      paste(
        "Descriptive figure. Each bar reports the mean of log(1 + mean annual Chinese aid",
        "in constant 2021 USD) across country-level observations created by averaging",
        "2013-2021 values within each country."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(lineheight = 0.95, margin = margin(r = 12)),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  scale_fill_manual(values = regime_gradient, guide = "none") +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_country_regime_barplot.png"),
  plot = regime_barplot,
  width = 8.8,
  height = 5.6,
  dpi = 300
)

# Plot the top country recipients of Chinese aid so readers can see which cases
# dominate the aggregate totals.
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

# Plot the bivariate relationship estimated in the simplest country-level model.
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
    y = "Log(1 + mean annual Chinese aid,\nconstant USD 2021)",
    caption = str_wrap(
      paste(
        "Relational figure. Points are countries collapsed to one observation each using",
        "2013-2021 means. The line is an OLS fit with 95% confidence interval."
      ),
      width = 95
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(lineheight = 0.95, margin = margin(r = 12)),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_country_aid_scatter.png"),
  plot = aid_scatter,
  width = 8.8,
  height = 5.6,
  dpi = 300
)

# Plot the regime-family composition of the collapsed country sample.
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

# ---------------------------
# Save Regression Results and Model Diagnostics
# ---------------------------

# Collect the core fitted values and residual diagnostics for the main controlled
# model so every downstream diagnostic uses the same base object.
diagnostic_data_m2 <- data.frame(
  entity = cs_m2_data$entity,
  fitted = fitted(cs_m2),
  residual = resid(cs_m2),
  std_residual = rstandard(cs_m2),
  cooks_distance = cooks.distance(cs_m2),
  above_cook_cutoff = cooks.distance(cs_m2) > (4 / nrow(cs_m2_data))
)

# Keep the most influential M2 observations in a compact table for export.
diagnostic_influence_m2 <- diagnostic_data_m2 %>%
  arrange(desc(cooks_distance)) %>%
  slice_head(n = 10)

# Run the outlier checks and store the thresholds used in the influence plot.
outlier_diagnostics_m2 <- compute_outlier_diagnostics(
  model = cs_m2,
  data = cs_m2_data,
  id_cols = c("entity")
)

# Keep only flagged cases for the HTML diagnostics table, while still handling the
# case where no countries exceed any threshold.
flagged_outliers_m2 <- outlier_diagnostics_m2$diagnostics_tbl %>%
  filter(outlier_any) %>%
  arrange(desc(abs_dffits), desc(leverage))

if (nrow(flagged_outliers_m2) == 0) {
  flagged_outliers_m2 <- data.frame(
    entity = "None",
    studentized_residual = NA,
    leverage = NA,
    cooks_distance = NA,
    dffits = NA,
    abs_studentized_residual = NA,
    abs_dffits = NA,
    outlier_studentized = NA,
    outlier_leverage = NA,
    outlier_cooks = NA,
    outlier_dffits = NA,
    outlier_any = NA,
    outlier_all = NA,
    check.names = FALSE
  )
}

# Export one combined diagnostics page for the main controlled model.
write_diagnostics_html(
  vif_tbl = vif_tbl,
  bp_tbl = bp_tbl,
  diagnostic_tbl = diagnostic_data_m2,
  influence_tbl = diagnostic_influence_m2,
  outlier_summary_tbl = outlier_diagnostics_m2$summary_tbl,
  flagged_outliers_tbl = flagged_outliers_m2,
  title = "Country M2 Diagnostics Table",
  out = file.path(output_dir, "selection_model_country_m2_diagnostics.html")
)

# Prepare labels for the influence plot so only the most extreme observations are
# named directly on the figure.
influence_plot_data_m2 <- outlier_diagnostics_m2$diagnostics_tbl %>%
  mutate(
    point_label = ifelse(outlier_all, entity, ""),
    outlier_label = ifelse(outlier_any, "TRUE", "FALSE")
  )

# Plot leverage against |DFFITS| to identify influential country observations.
influence_plot_m2 <- influence_plot_data_m2 %>%
  ggplot(aes(x = abs_dffits, y = leverage, color = outlier_label)) +
  geom_point(alpha = 0.75, size = 2.2) +
  geom_vline(
    xintercept = outlier_diagnostics_m2$dffits_cutoff,
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = outlier_diagnostics_m2$leverage_cutoff,
    linetype = "dashed",
    color = "blue",
    linewidth = 0.8
  ) +
  geom_text(
    data = subset(influence_plot_data_m2, outlier_all),
    aes(label = point_label),
    size = 3,
    vjust = -0.6,
    check_overlap = TRUE,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  labs(
    title = "Influence Diagnostics: Leverage vs |DFFITS| (Country M2)",
    subtitle = "Outlier screening for the controlled cross-section model; dashed lines mark thresholds.",
    x = "|DFFITS|",
    y = "Leverage",
    color = "Outlier",
    caption = str_wrap(
      paste(
        "Points are flagged as outliers when any threshold is exceeded",
        "(studentized residual, leverage, Cook's D, or |DFFITS|).",
        "Labels mark observations exceeding all thresholds."
      ),
      width = 110
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "right"
  ) +
  caption_theme

ggsave(
  filename = file.path(output_dir, "selection_model_country_influence_diagnostics_m2.png"),
  plot = influence_plot_m2,
  width = 10,
  height = 7,
  dpi = 300
)

# Plot residuals against fitted values to inspect functional form and variance.
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

# Build the quantile data needed for the normal Q-Q diagnostic.
qq_data_m2 <- data.frame(
  sample = sort(diagnostic_data_m2$std_residual),
  theoretical = qnorm(ppoints(length(diagnostic_data_m2$std_residual)))
)

# Plot standardized residuals against theoretical normal quantiles.
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

# Plot the top Cook's distance values from the main controlled model.
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

# ---------------------------
# Assumption Summary
# ---------------------------

# Turn the formal diagnostics into a short summary table that can be referenced
# directly in the write-up.
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

# Convert the BP and VIF results into plain-language text snippets for the final
# Gauss-Markov summary export.
country_bp_text <- paste(
  c(
  paste0("All-country M1 p = ", format_p_value(bp_tbl$p_value[bp_tbl$model == "CS_M1_bivariate"])),
  paste0("All-country M2 p = ", format_p_value(bp_tbl$p_value[bp_tbl$model == "CS_M2_controls"])),
  paste0("Recipient-only M2 p = ", format_p_value(bp_tbl$p_value[bp_tbl$model == "CS_M2_controls_positive_aid"]))
  ),
  collapse = "; "
)

country_max_vif <- max(vif_tbl$vif, na.rm = TRUE)

# Assemble the final Gauss-Markov summary table for the country models.
gauss_markov_tbl <- data.frame(
  Assumption = c(
    "Linearity in parameters",
    "Independent sampling / errors",
    "No perfect multicollinearity",
    "Zero conditional mean",
    "Homoskedasticity"
  ),
  `Evidence in this project` = c(
    "Residuals-vs-fitted plot is produced for the controlled cross-section model and the outcome is log(1 + mean aid).",
    "Each country enters once after averaging 2013-2021 values, but the sample is still observational rather than randomly assigned.",
    paste0("VIF check on M2 controls; max VIF = ", sprintf("%.2f", country_max_vif), "."),
    "Not directly testable; the model adds standard macro controls but cannot eliminate all omitted variables.",
    paste0("Breusch-Pagan results: ", country_bp_text, ".")
  ),
  Summary = c(
    "Specification is linear in coefficients, with functional form judged mainly from the residual plot.",
    "The cross-country sample is observational rather than randomly assigned, so independence is only approximate.",
    ifelse(
      country_max_vif < 5,
      "No strong multicollinearity signal among included controls.",
      "Some multicollinearity risk remains in the control set."
    ),
    "This remains a substantive identification assumption rather than something verified statistically.",
    ifelse(
      any(bp_tbl$p_value < 0.05, na.rm = TRUE),
      "At least one cross-section model shows BP evidence of heteroskedasticity.",
      "No BP rejection at the 5% level; one specification is borderline."
    )
  ),
  `Inference note` = c(
    "Linear OLS is a concise baseline, but nonlinear misspecification remains possible.",
    "Interpret the cross-section as descriptive association across countries.",
    "Coefficients are separately estimable without obvious collinearity failure.",
    "Treat estimates as conditional associations, not strong causal effects.",
    "Keeping HC1 robust SE is still the conservative choice."
  ),
  check.names = FALSE
)

write_simple_html_table(
  gauss_markov_tbl,
  title = "Gauss-Markov Assumption Summary",
  subtitle = "Concise diagnostic summary for the country-level cross-section models.",
  out = file.path(output_dir, "selection_model_country_gauss_markov_summary.html")
)
