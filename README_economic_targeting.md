# QM2 Independent Analysis Project

This branch contains a separate, parallel workflow for an **economic-targeting** analysis of Chinese development finance.

**Research question:** Does China direct substantially larger aid commitments to economically strategic sectors such as industry/mining/construction, energy, and transport/storage than to social sectors such as education, health, and related services?

## Framing

- This workflow is separate from the existing regime-affinity analysis.
- The available data do **not** contain direct measures of whether recipient countries have large mining or logistics industries.
- Because of that constraint, this analysis tests **sectoral targeting in Chinese aid allocations**, which is the closest defensible operationalization in the current repo.
- The stronger claim that China `exclusively` invests in mining/logistics countries is **not** supported by the data, because social-sector aid is present. The evidence instead shows a very strong concentration of **aid volume** in strategic sectors.

## Project Scope

- Unit of analysis: **funded country-year-sector-family observation**
- Period used: **2013-2021**
- Main dependent variable: `log_family_aid = log(1 + family-level Chinese aid in constant USD 2021)`
- Main independent variable: `strategic_family`
  - `1 = Strategic`
  - `0 = Social`
- Strategic sectors:
  - `320` Industry, Mining, Construction
  - `230` Energy
  - `210` Transport and Storage
- Social sectors:
  - `110` Education
  - `120` Health
  - `130` Population / Reproductive Health
  - `140` Water Supply and Sanitation
  - `150` Government and Civil Society
  - `160` Other Social Infrastructure and Services

## Data Sources

- `chinese-aid-data-2000-2021.xlsx`
  - Sheet used: `aid-data`
  - Filter applied: `Recommended For Aggregates == "Yes"`
  - Project-level sector codes are used to build the strategic vs social family comparison.
- `our-world-in-data-2013-2023.xlsx`
  - Sheet used: `our-world-in-data`
  - Controls:
    - `gdp-per-capita-worldbank`
    - `ti-corruption-perception-index`
    - `share-of-population-in-extreme-poverty`
    - `economic-inequality-gini-index`

## Analysis Script

- `economic_targeting_analysis.R`

What the script does:

1. Loads and cleans AidData + OWID data.
2. Maps AidData sectors into `Strategic`, `Social`, and `Other`.
3. Builds family-level descriptive tables and top-recipient tables.
4. Aggregates funded strategic/social aid to the country-year-family level.
5. Runs four regression specifications:
   - `M1`: bivariate OLS
   - `M2`: OLS with controls
   - `M3`: OLS with country and year fixed effects
   - `M4`: OLS with country-year fixed effects
6. Exports memo-ready tables, figures, and diagnostics to `output/economic_targeting/`.

## How to Run

From repo root:

```bash
Rscript economic_targeting_analysis.R
```

## Headline Results

- Strategic sectors absorb about `66.7%` of total Chinese aid in the sample.
- Social sectors absorb about `5.0%` of total Chinese aid.
- The average funded strategic project is about `$297.6M`, versus about `$14.3M` for the average funded social project.
- In country-years where both families receive funding, the median strategic share is about `93.5%`.
- The `strategic_family` coefficient remains positive and highly significant in every model:
  - `M1`: `3.58`
  - `M2`: `3.41`
  - `M3`: `3.36`
  - `M4`: `2.91`

Substantively, the strictest country-year fixed-effects model implies that, within the same country-year, strategic-sector commitments are still much larger than social-sector commitments.

## Inequality Link

- The current design includes the Gini index as a control, but it does **not** identify whether Chinese aid causally raises inequality over time.
- What it does show is that the strategic-sector premium persists across the inequality distribution rather than appearing only in low-inequality settings.
- This supports a careful argument for the memo: Chinese aid is heavily concentrated in sectors that can reinforce unequal growth or elite-centered rents, but the present dataset is better suited to showing **allocation patterns** than causal changes in inequality.

## Output Files

Generated in `output/economic_targeting/`:

- `economic_targeting_panel.csv`
- `economic_targeting_family_breakdown.csv`
- `economic_targeting_sector_breakdown.csv`
- `economic_targeting_family_top_recipients.csv`
- `economic_targeting_country_year_coverage.csv`
- `economic_targeting_country_year_ratios.csv`
- `economic_targeting_descriptive_stats.csv`
- `economic_targeting_descriptive_stats_by_family.csv`
- `economic_targeting_coefficients.csv`
- `economic_targeting_coefficients_robust.csv`
- `economic_targeting_fitstats.csv`
- `economic_targeting_regression_table.txt`
- `economic_targeting_regression_table.html`
- `economic_targeting_breusch_pagan.csv`
- `economic_targeting_vif.csv`
- `economic_targeting_assumption_summary.csv`
- `economic_targeting_cooks_distance_top10.csv`
- `economic_targeting_family_barplot.png`
- `economic_targeting_regression_coefficient_plot.png`
- `economic_targeting_sector_barplot.png`
- `economic_targeting_family_boxplot.png`
- `economic_targeting_country_year_ratio_hist.png`
- `economic_targeting_inequality_quartiles.csv`
- `economic_targeting_inequality_quartile_plot.png`
- `economic_targeting_residuals_vs_fitted_m2.png`
- `economic_targeting_qqplot_m2.png`
- `economic_targeting_cooks_distance_m2.png`

## Notes

- The main regression sample uses **funded** strategic and social family observations, because the core question here is about allocation **size** once China finances a sector family.
- A country-year fixed-effects model is included to compare strategic and social allocations within the same country-year.
- Heteroskedasticity is present in the main models, so the exported regression tables use HC1 or cluster-robust standard errors.
