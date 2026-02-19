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
5. Exports model outputs and diagnostics.

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
- `selection_model_vif.csv` (multicollinearity check for M2 controls)
- `selection_model_regime_barplot.png` (descriptive figure)

## Notes

- Missing markers `"."` are converted to `NA` before numeric conversion.
- Country-years with no AidData record are treated as zero aid in the panel.
- `autocracy_score = 3 - political_regime` so higher values mean less democratic regimes.
- This is a starter empirical design for the policy memo and diagnostics; additional robustness checks can be added as needed.
