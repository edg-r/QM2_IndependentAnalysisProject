# GPCO 454 - Quantitative Methods II - Winter 2025
# Independent Analysis Project - Sample Script

# ---------------------------
# Research Question
# ---------------------------

# For this analysis, we investigate: 
# "Are countries with higher income inequality more likely to receive Chinese aid?"

# Specifically, we will analyze how **income inequality**, measured as the 
# **average Gini index from 2013 to 2018** (constrained by data availability),
# influences **Chinese aid distribution from 2019 to 2021**. 
# 
# To account for potential baseline differences in aid allocation, we will control 
# for **Chinese aid received between 2013 and 2018**.

# ---------------------------
# Preliminaries
# ---------------------------

# Set working directory and load the necessary packages in R script.

setwd("~/Desktop/QM2 R Materials/Week 9") # <- Update this to your directory and comment it out after setting it!

# Install necessary packages (uncomment if not installed)
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("stargazer")
# install.packages("ggplot2")
# install.packages("patchwork")
# install.packages("interactions")
# install.packages("margins")
# install.packages("estimatr")
# install.packages("modelsummary")

# Load required libraries
library(tidyverse)
library(readxl)
library(stargazer)
library(ggplot2)
library(patchwork)
library(interactions)
library(modelsummary)
library(ggeffects)
library(margins)
library(lmtest)
library(estimatr)

# ---------------------------
# Load Datasets
# ---------------------------

# Load the datasets into R
aid_data <- read_excel("chinese-aid-data-2000-2021.xlsx")
our_world_data <- read_excel("our-world-in-data-2013-2023.xlsx")

#What uniquely identify the aid data? 
sector_col <- "Sector Name" #relevant column
id <- "AidData Record ID" #relevant column

# Count total rows
n_total <- nrow(aid_data)

# Count unique rows by different key combinations
n_entity <- n_distinct(aid_data$entity)
n_entity_year <- n_distinct(aid_data %>% select(entity, year))
n_entity_year_sector <- n_distinct(aid_data %>% select(entity, year, !!sym(sector_col)))

# Compare counts
cat("Total rows:", n_total, "\n")
cat("Unique entities:", n_entity, "\n")
cat("Unique entity-year combinations:", n_entity_year, "\n")
cat("Unique entity-year-sector combinations:", n_entity_year_sector, "\n")

#It is not just entity-year-sector.. there is more information!

# ---------------------------
# Transforming our_world_data into a Cross-Country Dataset
# ---------------------------

# The goal here is to create a **cross-country dataset** by aggregating 
# country-year observations into country-level indicators.
# We will compute **average values for key political and economic variables** 
# (e.g., inequality, governance, democracy) **up to 2018**, ensuring that 
# these predictors temporally precede the dependent variable (Chinese aid from 2019 onward).

# Select relevant variables and filter data up to 2018
variables_to_average <- c(
  "ti-corruption-perception-index",  # Corruption perception index
  "political-regime",  # Political regime classification
  "share-of-population-in-extreme-poverty",  # Poverty rate
  "economic-inequality-gini-index",  # Income inequality (Gini index)
  "gdp-per-capita-worldbank",  # GDP per capita
  "democracy-index-eiu"  # Democracy index
)

# Convert variables to numeric properly, replacing "." with NA
our_world_data <- our_world_data %>%
  mutate(across(all_of(variables_to_average), ~ as.numeric(na_if(as.character(.), "."))))

# Filter data for years up to and including 2018
our_world_data_filtered <- our_world_data %>%
  filter(year <= 2018)

# Compute the average of selected variables for each country, ensuring NA if all values are missing
averaged_our_world_data <- our_world_data_filtered %>%
  group_by(entity) %>%
  summarise(across(all_of(variables_to_average), 
                   ~ ifelse(all(is.na(.)), NA, mean(., na.rm = TRUE)), 
                   .names = "{.col}_avg"), 
            .groups = "drop")

# Optionally, save the final dataset
write.csv(averaged_our_world_data, "averaged_our_world_data.csv", row.names = FALSE)


# ---------------------------
# Processing Chinese Aid Data (2019-2021)
# ---------------------------

# The goal here is to compute:
# 1. **Total aid received by each country (aid_all_sector)** – Averaged over 2019-2021.
# 2. **Sector-specific aid received by each country** – Averaged over 2019-2021 for each sector.
# 3. Convert all aid values to **million USD** for readability.
# 4. Replace **NA values in sectoral aid with 0**, assuming no aid was given in that sector.
# 5. **Round all values to two decimal places** for readability.

# Load aid dataset
aid_data <- read_excel("chinese-aid-data-2000-2021.xlsx")

# Identify relevant columns
aid_amount_col <- "Adjusted Amount (Constant USD 2021)"
sector_col <- "Sector Name"

# Filter data for years 2019-2021
aid_data_filtered <- aid_data %>%
  filter(year >= 2019 & year <= 2021)

# ---------------------------
# Compute Total Aid per Country (Averaged Over 2019-2021)
# ---------------------------

# Average aid amounts across entity-year (total aid received per country-year)
aid_total <- aid_data_filtered %>%
  group_by(entity) %>%
  summarise(aid_all_sector = round(mean(!!sym(aid_amount_col), na.rm = TRUE) / 1e6, 2),  # Convert to million USD & round
            .groups = "drop")

# ---------------------------
# Compute Sector-Specific Aid per Country (Averaged Over 2019-2021)
# ---------------------------

# Average aid amounts across entity-year-sector
aid_by_sector <- aid_data_filtered %>%
  group_by(entity, !!sym(sector_col)) %>%
  summarise(sector_aid = round(mean(!!sym(aid_amount_col), na.rm = TRUE) / 1e6, 2),  # Convert to million USD & round
            .groups = "drop")

# Reshape data to have separate columns for each sector
aid_avg_by_sector_pivot <- aid_by_sector %>%
  pivot_wider(names_from = !!sym(sector_col), values_from = sector_aid, names_prefix = "aid_")

# ---------------------------
# Merge Total and Sector-Specific Aid Data
# ---------------------------

# Merge total aid and sector-wise aid into a single dataset
aid_final <- aid_total %>%
  left_join(aid_avg_by_sector_pivot, by = "entity")

# ---------------------------
# Handling Missing Sectoral Aid Values
# ---------------------------

# Assumption: If a country has NA in a sector, it means no aid was received in that sector for 2019-2021.
# We replace NA with 0 to reflect this assumption.
aid_final[is.na(aid_final)] <- 0

# ---------------------------
# Collapse Dataset to Cross-Country Format
# ---------------------------

# Ensure only relevant variables remain for the final dataset
cross_country_aid <- aid_final

# Optionally, save the final dataset
write.csv(cross_country_aid, "cross_country_aid_2019_2021.csv", row.names = FALSE)


# ---------------------------
# Processing Baseline Chinese Aid Data (2013-2018)
# ---------------------------

# The goal here is to compute:
# 1. **Baseline total aid received by each country (aid_all_sector_baseline)** – Averaged over 2013-2018.
# 2. **Baseline sector-specific aid received by each country** – Averaged over 2013-2018 for each sector.
# 3. Convert all aid values to **million USD** for readability.
# 4. Replace **NA values in sectoral aid with 0**, assuming no aid was given in that sector.

# Filter data for years 2013-2018
aid_data_baseline <- aid_data %>%
  filter(year >= 2013 & year <= 2018)

# ---------------------------
# Compute Baseline Total Aid per Country (Averaged Over 2013-2018)
# ---------------------------

# Average aid amounts across entity-year (total aid received per country-year)
aid_total_baseline <- aid_data_baseline %>%
  group_by(entity) %>%
  summarise(aid_all_sector_baseline = round(mean(!!sym(aid_amount_col), na.rm = TRUE) / 1e6, 2),  # Convert to million USD & round
            .groups = "drop")

# ---------------------------
# Compute Baseline Sector-Specific Aid per Country (Averaged Over 2013-2018)
# ---------------------------

# Average aid amounts across entity-year-sector
aid_by_sector_baseline <- aid_data_baseline %>%
  group_by(entity, !!sym(sector_col)) %>%
  summarise(sector_aid_baseline = round(mean(!!sym(aid_amount_col), na.rm = TRUE) / 1e6, 2),  # Convert to million USD & round
            .groups = "drop")

# Reshape data to have separate columns for each sector
aid_avg_by_sector_baseline_pivot <- aid_by_sector_baseline %>%
  pivot_wider(names_from = !!sym(sector_col), values_from = sector_aid_baseline, names_prefix = "aid_baseline_")

# ---------------------------
# Merge Baseline Total and Sector-Specific Aid Data
# ---------------------------

# Merge total baseline aid and sector-wise baseline aid into a single dataset
aid_baseline_final <- aid_total_baseline %>%
  left_join(aid_avg_by_sector_baseline_pivot, by = "entity")

# ---------------------------
# Handling Missing Sectoral Aid Values
# ---------------------------

# Assumption: If a country has NA in a sector, it means no aid was received in that sector for 2013-2018.
# We replace NA with 0 to reflect this assumption.
aid_baseline_final[is.na(aid_baseline_final)] <- 0

# ---------------------------
# Collapse Dataset to Cross-Country Format
# ---------------------------

# Ensure only relevant variables remain for the final dataset
cross_country_aid_baseline <- aid_baseline_final

# View final dataset
print(cross_country_aid_baseline)

# Optionally, save the final dataset
write.csv(cross_country_aid_baseline, "cross_country_aid_baseline_2013_2018.csv", row.names = FALSE)


# ---------------------------
# Merging All Datasets into a Single Cross-Country Dataset
# ---------------------------

# Load the datasets
averaged_our_world_data <- read.csv("averaged_our_world_data.csv")
cross_country_aid_2019_2021 <- read.csv("cross_country_aid_2019_2021.csv")
cross_country_aid_baseline_2013_2018 <- read.csv("cross_country_aid_baseline_2013_2018.csv")

# ---------------------------
# Merge the datasets by 'entity' (country name)
# ---------------------------

cross_country_final <- averaged_our_world_data %>%
  left_join(cross_country_aid_baseline_2013_2018, by = "entity") %>%
  left_join(cross_country_aid_2019_2021, by = "entity")

# ---------------------------
# View and Save Final Merged Dataset
# ---------------------------

# Print dataset preview
print(cross_country_final)

# Save merged dataset to CSV
write.csv(cross_country_final, "cross_country_final.csv", row.names = FALSE)


# ---------------------------
# Scatter Plot of Aid and Economic Inequality (Gini Index)
# ---------------------------

# Create scatter plot
ggplot(cross_country_final, aes(x = economic.inequality.gini.index_avg, y = aid_all_sector)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter points
  geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +  # Regression line
  labs(
    title = "Scatter Plot: Chinese Aid vs. Economic Inequality",
    x = "Economic Inequality (Gini Index, Avg 2013-2018)",
    y = "Total Chinese Aid (Million USD, 2019-2021)"
  ) +
  theme_minimal()


# ---------------------------
# Transform Outcome Variable: Log(1 + Aid)
# ---------------------------

# Apply log(1 + x) transformation to aid_all_sector
cross_country_final <- cross_country_final %>%
  mutate(log_aid_all_sector = log1p(aid_all_sector))  # log(1 + x) transformation

# ---------------------------
# Rerun Regression Analysis Using Log(1 + Aid)
# ---------------------------

# Model 1: Bivariate regression of log_aid_all_sector on economic inequality (Gini index)
m1_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 2: Add corruption index
m2_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg + 
                             ti.corruption.perception.index_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 3: Add political regime
m3_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 4: Add share of population in extreme poverty
m4_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg + 
                             share.of.population.in.extreme.poverty_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 5: Add GDP per capita
m5_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg + 
                             share.of.population.in.extreme.poverty_avg + 
                             gdp.per.capita.worldbank_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 6: Add democracy index
m6_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg + 
                             share.of.population.in.extreme.poverty_avg + 
                             gdp.per.capita.worldbank_avg + 
                             democracy.index.eiu_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 7: Add baseline aid (2013-2018)
m7_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg + 
                             share.of.population.in.extreme.poverty_avg + 
                             gdp.per.capita.worldbank_avg + 
                             democracy.index.eiu_avg + 
                             aid_all_sector_baseline, 
                           data = cross_country_final, se_type = "HC1")

# ---------------------------
# Save Regression Results Using modelsummary
# ---------------------------

# Define the list of models
models <- list(
  "M1" = m1_all_sector,
  "M2" = m2_all_sector,
  "M3" = m3_all_sector,
  "M4" = m4_all_sector,
  "M5" = m5_all_sector,
  "M6" = m6_all_sector,
  "M7" = m7_all_sector
)

#An example of how to look at regressions with robust 
modelsummary(
  models = models,
  out = "html",
  stars = T
)

# Save as HTML with robust standard errors
modelsummary(models, 
             stars = TRUE, 
             statistic = "std.error",  # Displays robust SEs
             output = "regression_results_log_aid.html")

# Save as LaTeX with robust standard errors
modelsummary(models, 
             stars = TRUE, 
             statistic = "std.error",  # Displays robust SEs
             output = "regression_results_log_aid.tex")

# Print the final model (optional)
print(m7_all_sector)

# Combine all three models into one regression table and save as a single text file
summary(m7_all_sector)

# ---------------------------
# Create New Variable: Liberal Democracy Indicator
# ---------------------------

cross_country_final <- cross_country_final %>%
  mutate(liberal_democracy = ifelse(political.regime_avg == 3, 1, 0))

# ---------------------------
# Rerun Regression Analysis Using Log(1 + Aid) with Interaction Term
# ---------------------------

# Model 1: Bivariate regression with interaction term
m1_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg * liberal_democracy, 
                           data = cross_country_final, se_type = "HC1")

# Model 2: Add corruption index
m2_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg * liberal_democracy + 
                             ti.corruption.perception.index_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 3: Add political regime (already captured by liberal_democracy, but for robustness)
m3_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg * liberal_democracy + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 4: Add share of population in extreme poverty
m4_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg * liberal_democracy + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg + 
                             share.of.population.in.extreme.poverty_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 5: Add GDP per capita
m5_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg * liberal_democracy + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg + 
                             share.of.population.in.extreme.poverty_avg + 
                             gdp.per.capita.worldbank_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 6: Add democracy index
m6_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg * liberal_democracy + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg + 
                             share.of.population.in.extreme.poverty_avg + 
                             gdp.per.capita.worldbank_avg + 
                             democracy.index.eiu_avg, 
                           data = cross_country_final, se_type = "HC1")

# Model 7: Add baseline aid (2013-2018)
m7_all_sector <- lm_robust(log_aid_all_sector ~ economic.inequality.gini.index_avg * liberal_democracy + 
                             ti.corruption.perception.index_avg + 
                             political.regime_avg + 
                             share.of.population.in.extreme.poverty_avg + 
                             gdp.per.capita.worldbank_avg + 
                             democracy.index.eiu_avg + 
                             aid_all_sector_baseline, 
                           data = cross_country_final, se_type = "HC1")

# ---------------------------
# Save Regression Results Using modelsummary
# ---------------------------

# Define the list of models
models <- list(
  "M1 (Bivariate + Interaction)" = m1_all_sector,
  "M2 (+ Corruption Index)" = m2_all_sector,
  "M3 (+ Political Regime)" = m3_all_sector,
  "M4 (+ Extreme Poverty)" = m4_all_sector,
  "M5 (+ GDP per Capita)" = m5_all_sector,
  "M6 (+ Democracy Index)" = m6_all_sector,
  "M7 (+ Baseline Aid)" = m7_all_sector
)

# Save as HTML with robust standard errors
modelsummary(models, 
             stars = TRUE, 
             statistic = "std.error",  # Displays robust SEs
             output = "regression_results_interaction.html")

# Save as LaTeX with robust standard errors
modelsummary(models, 
             stars = TRUE, 
             statistic = "std.error",  # Displays robust SEs
             output = "regression_results_interaction.tex")

# Print the final model (optional)
print(m7_all_sector)

#An example of how to look at regressions with robust 
modelsummary(
  models = models,
  out = "html",
  stars = T
)

# ---------------------------
# Why Analyze Subsector Aid?
# ---------------------------
# Since our initial regression on total aid (`aid_all_sector`) showed no clear effects,
# we now examine aid distribution at a more granular level (by subsector).
# This allows us to:
# - Identify whether specific aid categories are more influenced by economic inequality.
# - Account for heterogeneity in Chinese aid distribution patterns.
# - Control for past aid in each subsector to isolate the effects.

# ---------------------------
# Define List of Aid Subsectors
# ---------------------------

# List of subsector aid variables and their corresponding baselines
subsectors <- c(
  "DEVELOPMENTAL.FOOD.AID.FOOD.SECURITY.ASSISTANCE",
  "DISASTER.PREVENTION.AND.PREPAREDNESS",
  "EMERGENCY.RESPONSE",
  "GOVERNMENT.AND.CIVIL.SOCIETY",
  "HEALTH",
  "OTHER.SOCIAL.INFRASTRUCTURE.AND.SERVICES",
  "EDUCATION",
  "AGRICULTURE..FORESTRY..FISHING",
  "UNALLOCATED.UNSPECIFIED",
  "ACTION.RELATING.TO.DEBT",
  "ENERGY",
  "POPULATION.POLICIES.PROGRAMMES.AND.REPRODUCTIVE.HEALTH",
  "WATER.SUPPLY.AND.SANITATION",
  "TRADE.POLICIES.AND.REGULATIONS",
  "BANKING.AND.FINANCIAL.SERVICES",
  "BUSINESS.AND.OTHER.SERVICES",
  "COMMUNICATIONS",
  "INDUSTRY..MINING..CONSTRUCTION",
  "OTHER.MULTISECTOR",
  "TRANSPORT.AND.STORAGE",
  "GENERAL.ENVIRONMENTAL.PROTECTION",
  "OTHER.COMMODITY.ASSISTANCE",
  "RECONSTRUCTION.RELIEF.AND.REHABILITATION",
  "GENERAL.BUDGET.SUPPORT"
)

# ---------------------------
# Loop Through Each Subsector, Transform & Regress
# ---------------------------

for (subsector in subsectors) {
  
  # Define the variable names dynamically
  aid_var <- paste0("aid_", subsector)
  baseline_var <- paste0("aid_baseline_", subsector)
  log_aid_var <- paste0("log_", aid_var)
  
  # Apply log(1 + x) transformation
  cross_country_final <- cross_country_final %>%
    mutate(!!log_aid_var := log1p(!!sym(aid_var)))
  
  # ---------------------------
  # Run Progressive Regressions
  # ---------------------------
  
  # Model 1: Bivariate regression
  m1 <- lm_robust(!!sym(log_aid_var) ~ economic.inequality.gini.index_avg, 
                  data = cross_country_final, se_type = "HC1")
  
  # Model 2: Add corruption index
  m2 <- lm_robust(!!sym(log_aid_var) ~ economic.inequality.gini.index_avg + 
                    ti.corruption.perception.index_avg, 
                  data = cross_country_final, se_type = "HC1")
  
  # Model 3: Add political regime
  m3 <- lm_robust(!!sym(log_aid_var) ~ economic.inequality.gini.index_avg + 
                    ti.corruption.perception.index_avg + 
                    political.regime_avg, 
                  data = cross_country_final, se_type = "HC1")
  
  # Model 4: Add share of population in extreme poverty
  m4 <- lm_robust(!!sym(log_aid_var) ~ economic.inequality.gini.index_avg + 
                    ti.corruption.perception.index_avg + 
                    political.regime_avg + 
                    share.of.population.in.extreme.poverty_avg, 
                  data = cross_country_final, se_type = "HC1")
  
  # Model 5: Add GDP per capita
  m5 <- lm_robust(!!sym(log_aid_var) ~ economic.inequality.gini.index_avg + 
                    ti.corruption.perception.index_avg + 
                    political.regime_avg + 
                    share.of.population.in.extreme.poverty_avg + 
                    gdp.per.capita.worldbank_avg, 
                  data = cross_country_final, se_type = "HC1")
  
  # Model 6: Add democracy index
  m6 <- lm_robust(!!sym(log_aid_var) ~ economic.inequality.gini.index_avg + 
                    ti.corruption.perception.index_avg + 
                    political.regime_avg + 
                    share.of.population.in.extreme.poverty_avg + 
                    gdp.per.capita.worldbank_avg + 
                    democracy.index.eiu_avg, 
                  data = cross_country_final, se_type = "HC1")
  
  # Model 7: Add baseline aid for the same subsector
  m7 <- lm_robust(!!sym(log_aid_var) ~ economic.inequality.gini.index_avg + 
                    ti.corruption.perception.index_avg + 
                    political.regime_avg + 
                    share.of.population.in.extreme.poverty_avg + 
                    gdp.per.capita.worldbank_avg + 
                    democracy.index.eiu_avg + 
                    !!sym(baseline_var),
                  data = cross_country_final, se_type = "HC1")
  
  # ---------------------------
  # Save Regression Results (One Table Per Sector)
  # ---------------------------
  
  # List of models for this subsector
  models <- list(
    "M1" = m1,
    "M2" = m2,
    "M3" = m3,
    "M4" = m4,
    "M5" = m5,
    "M6" = m6,
    "M7" = m7
  )
  
  # Generate filenames dynamically
  html_file <- paste0("regression_results_", subsector, ".html")
  tex_file <- paste0("regression_results_", subsector, ".tex")
  
  # Save as HTML
  modelsummary(models, 
               stars = TRUE, 
               statistic = "std.error",  # Displays robust SEs
               output = html_file)
  
  # Save as LaTeX
  modelsummary(models, 
               stars = TRUE, 
               statistic = "std.error",  # Displays robust SEs
               output = tex_file)
  
  # Print message to track progress
  print(paste("Regression results saved for:", subsector))
}

