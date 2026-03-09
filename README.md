# QM2 Independent Analysis Project

This repository contains a reproducible starter workflow for the QM2 Independent Analysis Project focused on a **selection model**:

**Research question:** Do governments that are more authoritarian receive more Chinese development finance?

## Project Scope

- Causal framing: **Regime Affinity (Selection)**  
  - `X`: recipient regime type (`political-regime` from OWID, recoded as `autocracy_score`)  
  - `Y`: yearly Chinese aid received (AidData, aggregated to country-year)
- Period used: **2013-2021** (overlap between provided datasets)
- Unit of analysis: **country-year**

## Data Sources (provided in this repo)

- `chinese-aid-data-2000-2021.xlsx`
  - Sheet used: `aid-data`
  - Filter applied: `Recommended For Aggregates == "Yes"`
  - Main outcome: `Adjusted Amount (Constant USD 2021)` aggregated to country-year
- `our-world-in-data-2013-2023.xlsx`
  - Sheet used: `our-world-in-data`
  - Controls and key regressor:
    - `political-regime`
    - `gdp-per-capita-worldbank`
    - `ti-corruption-perception-index`
    - `share-of-population-in-extreme-poverty`
    - `economic-inequality-gini-index`

## Analysis Script

- `selection_model_regime_affinity.R`

What the script does:

1. Loads and cleans AidData + OWID data.
2. Aggregates Chinese aid to country-year level.
3. Merges to a single country-year panel (`2013-2021`).
4. Runs progressive models:
   - **M1**: bivariate OLS (`log_china_aid ~ autocracy_score`)
   - **M2**: OLS with controls
   - **M3**: OLS with country and year fixed effects
5. Exports descriptive statistics, memo-ready `stargazer` regression tables, `ggplot2` figures, and regression diagnostics.

## How to Run

From repo root:

```bash
Rscript selection_model_regime_affinity.R
```

## Output Files

Generated in `output/`:

- `selection_model_panel.csv` (merged analysis panel)
- `selection_model_coefficients.csv` (model coefficients + CIs)
- `selection_model_fitstats.csv` (fit statistics)
- `selection_model_descriptive_stats.csv` (overall summary statistics)
- `selection_model_descriptive_stats_by_regime.csv` (summary statistics by authoritarian/democratic grouping)
- `selection_model_regression_table.txt` and `.html` (`stargazer` regression tables with robust SEs)
- `selection_model_coefficients_robust.csv` (robust and clustered-SE coefficient table)
- `selection_model_breusch_pagan.csv` (heteroskedasticity test results)
- `selection_model_cooks_distance_top10.csv` (most influential observations)
- `selection_model_assumption_summary.csv` (short assumptions diagnostic summary)
- `selection_model_vif.csv` (multicollinearity check for M2 controls)
- `selection_model_aid_scatter.png` (bivariate fitted relationship)
- `selection_model_regime_barplot.png` (descriptive figure)
- `selection_model_regime_family_pie.png` (authoritarian vs democratic country-years)
- `selection_model_latest_regime_family_pie.png` (authoritarian vs democratic countries in latest year)
- `selection_model_residuals_vs_fitted_m2.png` (functional form / variance diagnostic)
- `selection_model_qqplot_m2.png` (residual normality check)
- `selection_model_cooks_distance_m2.png` (influence plot for controlled model)

## Notes

- Missing markers `"."` are converted to `NA` before numeric conversion.
- Country-years with no AidData record are treated as zero aid in the panel.
- `autocracy_score = 3 - political_regime` so higher values mean less democratic regimes.
- For the pie charts, `political-regime` values `0` and `1` are grouped as `Authoritarian`, while `2` and `3` are grouped as `Democratic`.
- This is a starter empirical design for the policy memo and diagnostics; additional robustness checks can be added as needed.
