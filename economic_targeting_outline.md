# Economic Targeting Regression Outline

## 1. Title and Research Question

- **Working title:** Does Chinese Development Finance Prioritize Economically Strategic Sectors Over Social Sectors?
- **Question:** Are Chinese aid commitments systematically larger in industry/mining/construction, energy, and transport/storage than in education, health, and related social sectors?

## 2. Introduction

- Explain why sectoral allocation is useful for understanding donor motives.
- Frame Chinese aid as potentially shaped by commercial and strategic interests, not only developmental need.
- Preview the core finding:
  - Chinese aid is heavily concentrated in strategic sectors.
  - The strategic-sector premium remains large and statistically significant across all regression specifications.

## 3. Background and Literature

- Use the general aid-allocation literature to establish that aid often follows donor interests.
- Connect that logic to China’s infrastructure-heavy and commercially oriented overseas financing model.
- Key sources:
  - Alesina and Dollar (2000)
  - Dreher et al. (2018)
  - Brautigam and Gallagher (2014)
  - Marson, Belingheri, and Parola (2021)
  - Bluhm, Melesky, and Reuter (2025)

## 4. Theory and Hypotheses

- **Mechanism:** Strategic sectors generate clearer commercial value for the donor through extraction, energy supply, logistics connectivity, and large capital-intensive infrastructure.
- **Contrast:** Social sectors provide broader welfare benefits but weaker direct donor payoff.
- **Hypotheses:**
  - `H1:` Chinese aid commitments are larger in strategic sectors than in social sectors.
  - `H2:` The strategic-sector premium remains after adding controls.
  - `H3:` The strategic-sector premium remains even within the same country-year.

## 5. Data and Variable Construction

- Data:
  - AidData project-level Chinese finance records
  - OWID controls for GDP per capita, corruption, poverty, and inequality
- Time period: `2013-2021`
- Unit of analysis: funded country-year-sector-family observation
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
- Dependent variable: `log(1 + family-level adjusted aid in constant USD 2021)`
- Main regressor: `strategic_family`

## 6. Methodology

- `M1:` bivariate OLS
- `M2:` OLS with controls
- `M3:` country and year fixed effects
- `M4:` country-year fixed effects
- Standard errors:
  - `M1-M2`: HC1 robust
  - `M3-M4`: cluster-robust by country
- Why `M4` matters:
  - It compares strategic and social allocations within the same country-year.

## 7. Descriptive Findings

- Strategic sectors receive about `66.7%` of total Chinese aid.
- Social sectors receive about `5.0%`.
- Average funded strategic project: about `$297.6 million`.
- Average funded social project: about `$14.3 million`.
- In country-years where both families are funded, the median strategic share is about `93.5%`.

## 8. Regression Findings

- `M1:` `strategic_family = 3.58`, `p < 0.001`
- `M2:` `strategic_family = 3.41`, `p < 0.001`
- `M3:` `strategic_family = 3.36`, `p < 0.001`
- `M4:` `strategic_family = 2.91`, `p < 0.001`
- Main interpretation:
  - The strategic premium is large, stable, and statistically robust.
  - The pattern survives the strictest within-country-year specification.

## 9. Inequality Link

- The theory can connect strategic-sector concentration to inequality because extractive and logistics-centered investment can generate concentrated rents and weaker broad social provision.
- But the current regression does **not** prove that Chinese aid causes inequality to rise over time.
- The defensible claim is:
  - Chinese aid is concentrated in sectors consistent with inequality-preserving or inequality-worsening growth patterns.

## 10. Limitations

- The analysis measures sectoral targeting, not the actual size of recipient mining or logistics industries.
- Sector codes are proxies for donor-benefiting investment.
- The design does not directly observe contract terms, collateralization, or post-aid inequality changes.

## 11. Conclusion

- Chinese aid is not exclusive to strategic sectors.
- But in monetary terms it is overwhelmingly concentrated there.
- The country-year fixed-effects model makes that conclusion much stronger.

## Recommended Figures

- `output/economic_targeting/economic_targeting_family_barplot.png`
- `output/economic_targeting/economic_targeting_regression_coefficient_plot.png`
- `output/economic_targeting/economic_targeting_country_year_ratio_hist.png`
- `output/economic_targeting/economic_targeting_inequality_quartile_plot.png`
