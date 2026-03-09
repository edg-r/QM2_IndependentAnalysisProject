# Economic Targeting Memo Outline

## Important framing note

The available data do not measure whether recipient countries objectively have `large mining industries` or `large logistics sectors`. What the data do measure well is **where Chinese aid is allocated by sector**. This memo should therefore make a narrower and stronger empirical claim:

China does invest in social sectors, but the **volume** of Chinese aid is overwhelmingly concentrated in economically strategic sectors such as industry/mining/construction, energy, and transport/storage.

That is the claim the new analysis actually tests.

## Working title

Does Chinese Development Finance Prioritize Economically Strategic Sectors Over Social Sectors?

## Recommended memo structure

### 1. Introduction

Target length: `150-200` words

Core job of this section:

- State the question clearly.
- Explain why sectoral allocation matters for understanding donor motives.
- Preview the main empirical result in one sentence.

Suggested framing:

- Chinese development finance is often discussed as a tool of commercial and geopolitical statecraft, not only as poverty-oriented aid.
- That makes sectoral allocation a useful window into donor priorities.
- Your question is whether Chinese aid commitments are substantially larger in strategic sectors that can support extraction, transport, and large infrastructure than in education, health, and related social sectors.

Safe preview sentence:

The results show that Chinese aid is far more concentrated in strategic sectors than in social sectors, and that difference remains large and statistically significant even in the strictest country-year fixed-effects specification.

### 2. Background and Literature Anchor

Target length: `250-350` words

Core job of this section:

- Situate the project in the aid-allocation literature.
- Show why commercial and strategic interests are plausible drivers of aid allocation.
- Connect that logic specifically to Chinese development finance and infrastructure-heavy lending.

Points to develop:

- The classic aid-allocation literature shows that aid is shaped by donor interests as well as recipient need.
- For China, the literature often emphasizes infrastructure, state-to-state bargaining, commodity access, and commercially strategic transport and energy investments.
- Commodity-backed and infrastructure-linked finance make it plausible that China allocates especially large volumes to sectors that support extraction and logistics.
- That logic does not imply zero social aid, but it does imply a large imbalance in favor of strategic sectors.

Academic sources you can use:

- [Alesina and Dollar (2000)](https://doi.org/10.1023/A:1009874203400), *Journal of Economic Growth*: foundational evidence that aid follows political and strategic interests, not just poverty.
- [Dreher, Fuchs, Parks, Strange, and Tierney (2018)](https://doi.org/10.1093/isq/sqx052), *International Studies Quarterly*: shows that Chinese state financing in Africa follows a different allocation logic from traditional Western aid and is linked to commercial and strategic interests.
- [Brautigam and Gallagher (2014)](https://doi.org/10.1111/1758-5899.12138), *Global Policy*: explains China's commodity-backed finance and why infrastructure and extractive projects can be tightly connected.
- [Marson, Belingheri, and Parola (2021)](https://doi.org/10.1016/j.retrec.2021.101111), *Research in Transportation Economics*: useful for the transport/logistics angle and China's role in African transport infrastructure.
- [Bluhm, Melesky, and Reuter (2025)](https://doi.org/10.1016/j.jue.2024.103730), *Journal of Urban Economics*: shows how Chinese infrastructure finance diffuses economic activity, supporting the idea that infrastructure projects are economically consequential rather than merely social spending.

Possible reusable paragraph:

The broad aid-allocation literature argues that donor governments use aid to pursue strategic interests in addition to developmental goals. That logic is especially relevant for China, whose overseas finance is frequently tied to infrastructure, commodity access, and commercially significant state-led projects. If Chinese finance is oriented toward sectors that support extraction, energy supply, and transport connectivity, then aid volumes should be systematically larger in those strategic sectors than in education, health, and related social services.

### 3. Theory and Hypotheses

Target length: `200-250` words

Core job of this section:

- State the mechanism clearly.
- Translate the user's original intuition into a defensible empirical hypothesis.
- Add one competing expectation.

Recommended theory:

- Strategic sectors such as mining-related industry, energy, and transport can generate direct commercial value for the donor or reduce the cost of moving commodities, energy, and goods.
- These sectors also align with large, capital-intensive project finance, where Chinese state lenders and firms are especially active.
- Social sectors can still receive aid, but if China is driven heavily by economic statecraft, the scale of commitments should be much larger in strategic sectors.

Suggested hypotheses:

- `H1:` Chinese aid commitments are significantly larger in strategic sectors than in social sectors.
- `H2:` The strategic-sector premium should remain after controlling for recipient income, corruption, poverty, and inequality.
- `H3:` The strategic-sector premium should remain even when comparing strategic and social allocations within the same country-year.

Important wording note:

- Avoid saying `China exclusively invests` in these sectors.
- The descriptive evidence shows that China does fund social sectors, just at much lower monetary scale.

### 4. Data and Variable Construction

Target length: `200-250` words

Core job of this section:

- Describe the datasets.
- Explain the sector-family coding.
- Define the unit of analysis and key variables.

Key points to include:

- AidData provides project-level Chinese development finance records.
- Restrict to records marked `Recommended For Aggregates = Yes`.
- Keep `2013-2021` to match the overlap with the OWID controls.
- Collapse project records to the country-year-sector-family level.
- Code `Strategic` as:
  - `320` Industry, Mining, Construction
  - `230` Energy
  - `210` Transport and Storage
- Code `Social` as:
  - `110` Education
  - `120` Health
  - `130` Population / Reproductive Health
  - `140` Water Supply and Sanitation
  - `150` Government and Civil Society
  - `160` Other Social Infrastructure and Services
- Main dependent variable: `log(1 + family-level adjusted aid in constant USD 2021)`.
- Main regressor: `strategic_family`, coded `1` for strategic and `0` for social.

### 5. Methodology

Target length: `200-250` words

Core job of this section:

- Explain the progressive regression strategy.
- Justify the controls.
- Explain why the fixed-effects model is the strongest specification.

Current code structure:

- `M1:` `log_family_aid ~ strategic_family`
- `M2:` add `log_gdp_pc`, `cpi`, `extreme_poverty`, and `gini`
- `M3:` add country and year fixed effects
- `M4:` add country-year fixed effects

Interpretation strategy:

- `M1` shows the unconditional strategic-vs-social gap.
- `M2` checks whether the gap is explained by recipient-country development conditions.
- `M3` absorbs stable country differences and common year shocks.
- `M4` is the strictest model because it compares strategic and social allocations **within the same country-year**.

Diagnostics currently available:

- VIFs are low to moderate; the highest is about `3.41`.
- Breusch-Pagan tests indicate heteroskedasticity, so the reported tables use HC1 or cluster-robust standard errors.

### 6. Findings

Target length: `300-350` words

Core job of this section:

- Present the descriptive concentration first.
- Then present the regression results.
- Translate the conclusion carefully and directly.

Descriptive results to report:

- Strategic sectors account for about `66.7%` of total Chinese aid.
- Social sectors account for about `5.0%` of total Chinese aid.
- The average funded strategic project is about `$297.6 million`.
- The average funded social project is about `$14.3 million`.
- In the `388` country-years where both sector families are funded, the median strategic share is about `93.5%`.

Regression results to report:

- `M1:` `strategic_family = 3.58`, `p < 0.001`
- `M2:` `strategic_family = 3.41`, `p < 0.001`
- `M3:` `strategic_family = 3.36`, `p < 0.001`
- `M4:` `strategic_family = 2.91`, `p < 0.001`

Interpretation:

- The strategic-sector premium is large and statistically robust across every specification.
- The effect remains strong even after country and year fixed effects and even in the within-country-year comparison.
- Because the dependent variable is logged, the coefficients imply a very large multiplicative gap in aid size, not a small marginal difference.
- A careful conclusion is that Chinese aid is not literally exclusive to strategic sectors, but the **financial scale** of Chinese aid is overwhelmingly concentrated there.

Suggested paragraph starter:

The results point in the same direction across every specification: Chinese aid commitments are dramatically larger in strategic sectors than in social sectors. The estimate is already large in the bivariate model and remains highly significant after adding controls, country and year fixed effects, and even country-year fixed effects. That pattern suggests the imbalance is not merely a byproduct of broad recipient-country characteristics.

### 7. Limitations and Conclusion

Target length: `150-200` words

Core job of this section:

- State what this design can and cannot claim.
- End with a policy-relevant takeaway.

Limitations to include:

- The data identify **sectoral allocation**, not the true size of recipient-country mining or logistics industries.
- Strategic sectors are defined by sector codes, which is an analytically defensible but still simplified proxy for donor-benefiting investment.
- The design does not directly observe contract terms, commodity collateral, or project-level bargaining motives.
- Reverse interpretation is still possible: some sectors may receive large flows because recipient demand is high, not only because China prefers them.

Conclusion points:

- China does fund social sectors, so an `exclusive investment` claim is too strong.
- But in dollar terms, Chinese aid is overwhelmingly concentrated in strategic sectors tied to industry/mining, energy, and transport.
- The finding survives a strict country-year fixed-effects design, which substantially strengthens the inference.

Possible closing sentence:

Overall, the evidence suggests that Chinese development finance is best understood less as broadly distributed social aid and more as a financing strategy heavily concentrated in sectors with clear economic and infrastructural value.

## Academic Source List

- [Alesina, Alberto, and David Dollar. 2000. "Who Gives Foreign Aid to Whom and Why?" *Journal of Economic Growth* 5(1): 33-63.](https://doi.org/10.1023/A:1009874203400)
- [Dreher, Axel, Andreas Fuchs, Bradley Parks, Austin M. Strange, and Michael J. Tierney. 2018. "Apples and Dragon Fruits: The Determinants of Aid and Other Forms of State Financing from China to Africa." *International Studies Quarterly* 62(1): 182-194.](https://doi.org/10.1093/isq/sqx052)
- [Brautigam, Deborah, and Kevin P. Gallagher. 2014. "Bartering Globalization: China's Commodity-Backed Finance in Africa and Latin America." *Global Policy* 5(3): 346-352.](https://doi.org/10.1111/1758-5899.12138)
- [Marson, Marco, Paola Belingheri, and Francesco Parola. 2021. "China's role in African infrastructure and capital projects: Focus on the transport sector." *Research in Transportation Economics* 88: 101111.](https://doi.org/10.1016/j.retrec.2021.101111)
- [Bluhm, Richard, Martin Melesky, and Oliver Reuter. 2025. "Connective Financing: Chinese Infrastructure Projects and the Diffusion of Economic Activity in Developing Countries." *Journal of Urban Economics* 145: 103730.](https://doi.org/10.1016/j.jue.2024.103730)

## Figures, Tables, and Appendix Checklist

### Main memo items

- `Figure 1:` `output/economic_targeting/economic_targeting_family_barplot.png`
- `Figure 2:` `output/economic_targeting/economic_targeting_family_boxplot.png`
- `Figure 3:` `output/economic_targeting/economic_targeting_country_year_ratio_hist.png`
- `Table 1:` `output/economic_targeting/economic_targeting_regression_table.html`
- `Table 2:` `output/economic_targeting/economic_targeting_family_breakdown.csv`

### Appendix items

- `Top recipients:` `output/economic_targeting/economic_targeting_family_top_recipients.csv`
- `Diagnostics:` `output/economic_targeting/economic_targeting_assumption_summary.csv`
- `VIF:` `output/economic_targeting/economic_targeting_vif.csv`
- `Residual plot:` `output/economic_targeting/economic_targeting_residuals_vs_fitted_m2.png`
- `Q-Q plot:` `output/economic_targeting/economic_targeting_qqplot_m2.png`
- `Cook's distance:` `output/economic_targeting/economic_targeting_cooks_distance_m2.png`
