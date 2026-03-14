# Write-Up Revision Flags for Cross-Section Design

Document reviewed: `/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/IAP/IAP Write Up.docx`

Primary result sources:
- `/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/IAP/output/country_cross_section/selection_model_country_regression_table.txt`
- `/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/IAP/output/country_cross_section/selection_model_panel_vs_country_comparison.csv`
- `/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/IAP/output/country_cross_section/selection_model_country_descriptive_stats.txt`
- `/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/IAP/output/country_cross_section/selection_model_country_descriptive_stats_by_regime.csv`
- `/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/IAP/output/country_cross_section/selection_model_country_assumption_summary.csv`
- `/Users/edgar/Documents/01 Projects/GPCO 454 - QM2 - Ravanilla/IAP/output/country_cross_section/selection_model_country_cooks_distance_top10.csv`

## Highest-priority changes

- Replace the current regression table. The Word document still reports the old panel models with 1,564 and 482 observations, country and year fixed effects, and clustered standard errors. The cross-sectional table uses country-level averages, HC1 robust standard errors in all models, and observations of 174, 135, and 92.
- Rewrite the data description. The design is no longer panel. The unit of analysis is now country, with 2013-2021 collapsed to country means.
- Soften the conclusion. In the full country-level controls model, autocracy is not statistically significant. But in the positive-aid-only robustness model, autocracy is positive and statistically significant.
- Remove causal phrasing. The cross-sectional design supports an associational claim, not a causal one.
- Update the limitations section. The new design loses within-country and over-time variation, has notable sample attrition from missing controls, and contains influential country cases.

## Section-by-section flags

### Title

- `Birds of a Feather Don't Flock Together` is too categorical for the new findings.
- A safer title would signal mixed or conditional evidence rather than a flat rejection.

### Memo introduction

- Keep the broad substantive setup, but update the findings summary.
- The current summary is incomplete because it says the relationship disappears with controls and stops there.
- The new results are more precise:
  - Bivariate country-level model: autocracy coefficient `4.186`, `p < 0.01`
  - Full controls, all countries: autocracy coefficient `0.948`, `p = 0.420`
  - Full controls, positive-aid recipients only: autocracy coefficient `1.083`, `p = 0.0077`
- Suggested framing:
  - There is a positive raw association between autocracy and Chinese aid.
  - That association is not robust in the full country-level model with controls.
  - Among countries that receive positive Chinese aid, more autocratic regimes still receive significantly more aid on average.

### Theory and hypothesis

- Change `Our causal question is` to `Our research question is` or `Our associational question is`.
- If you keep the hypothesis, phrase it in terms of average aid differences or association, not causal effect.

### Data description

- Replace `Our final data set utilized 482 data points between the years of 2013 to 2021.`
- The cross-sectional write-up should say that annual observations were collapsed into one observation per country using 2013-2021 means.
- Use the actual analytic samples:
  - Bivariate model: `174` countries
  - Full controls model: `135` countries
  - Positive-aid-only model: `92` countries
- Clarify the dependent variable:
  - `Log(1 + mean annual Chinese aid in constant USD 2021)`
- If you discuss descriptive differences by regime family, the cross-sectional summary supports:
  - Authoritarian countries mean aid: about `749.2` million USD
  - Democratic countries mean aid: about `272.2` million USD
- If you discuss the figure, describe it as a country-level scatter or summary based on mean annual aid, not yearly aid disbursements.

### Regression table

- Replace the entire existing table with the cross-sectional one from `selection_model_country_regression_table.txt`.
- Specific items that must change:
  - Title should reference `Country-Level Cross-Section`
  - Dependent variable should reference `mean annual Chinese aid`
  - Column 3 is no longer `Country + Year FE`; it is `Controls, positive-aid only`
  - Remove `Country fixed effects` and `Year fixed effects` rows
  - Remove `Clustered by country`; all three models use `HC1`
  - Replace observations with `174`, `135`, and `92`
  - Replace `R2` values with `0.202`, `0.594`, and `0.165`
  - Replace adjusted `R2` values with `0.197`, `0.578`, and `0.117`

### Methodology and findings

- The methodology is no longer `a simple bivariate linear regression` plus country and year controls. It is a set of country-level OLS regressions using 2013-2021 means and robust standard errors.
- Delete this claim entirely: `Finally, the data was controlled for the country and year when aid disbursement occurred...`
- Replace it with language explaining that the panel was collapsed into a cross-section, so the model compares countries' average aid and average covariates over 2013-2021.
- The current conclusion is too strong:
  - `it is clear that there is no statistically relevant correlational relationship...`
- That should be revised because the positive-aid-only model does show a statistically significant positive relationship.
- Safer conclusion:
  - The evidence does not show a stable, across-the-board relationship between regime type and Chinese aid once controls are added for the full sample.
  - However, among countries that do receive Chinese aid, more autocratic recipients tend to receive higher average aid amounts.
- Delete or soften this sentence:
  - `China has bucked the trend and is not using its aid as a tool to buy favor or cooperation from autocratic regimes.`
- The cross-sectional evidence does not support that strong of a claim.

### Limitations and conclusion

- Keep the GDP vs GDP per capita limitation if you want, but it should not be the only limitation now.
- Add these cross-section-specific limitations:
  - Collapsing 2013-2021 to country means removes year-to-year variation.
  - The full controls model falls from `174` to `135` countries because of missing covariates.
  - Several influential cases appear in diagnostics, especially `Eswatini`, `China`, `Congo`, `Guatemala`, and `Cape Verde`.
  - Because the design is observational and cross-sectional, the study cannot make a causal claim about regime type changing aid allocations.
- If you want one diagnostic sentence, you can say:
  - VIF values are all below `5`, and Breusch-Pagan tests do not show strong evidence of heteroskedasticity at the 5 percent level, though robust standard errors were retained.

## Clean replacement language you can adapt

### Findings summary

Using a country-level cross-section that averages observations from 2013 to 2021, I find a positive bivariate association between autocracy and Chinese aid. Once economic and governance controls are added, the autocracy coefficient becomes statistically insignificant in the full sample. However, in a robustness check restricted to countries that received positive Chinese aid, autocracy remains positively and significantly associated with higher average aid levels. This suggests that regime type does not robustly predict aid across all countries, but it may still matter conditional on being an aid recipient.

### Methods summary

The empirical strategy uses cross-sectional OLS models at the country level, collapsing yearly observations from 2013 to 2021 into country averages. The dependent variable is the log of one plus mean annual Chinese aid in constant 2021 USD. The models use HC1 robust standard errors and add controls for GDP per capita, corruption perceptions, extreme poverty, and inequality.

### Limitations summary

This cross-sectional design trades away within-country and over-time variation, so it cannot evaluate how changes in regime type within a country affect aid over time. The full controls model also loses observations because several covariates are missing for some countries. In addition, the estimates are somewhat sensitive to influential country cases, so the substantive conclusion should be interpreted as suggestive rather than definitive.
