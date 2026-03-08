# Policy Memo Outline

## Important note

The current script and outputs are built from a `2013-2021` country-year panel and include a country and year fixed-effects model. The assignment instructions require a cross-sectional dataset with one observation per country and explicitly say not to submit a panel or time-series analysis. This outline is therefore useful in two ways:

1. It gives you a write-up structure that matches the analysis you currently have.
2. It flags the places that should be revised if you convert the final analysis to a cross-section before submission.

## Working title

Do More Authoritarian Governments Receive More Chinese Development Finance?

## Recommended memo structure

### 1. Introduction

Target length: `150-200` words

Core job of this section:
- State the causal question clearly.
- Explain why Chinese aid allocation matters.
- Preview the main empirical takeaway in one sentence.

Points to cover:
- Chinese development finance is politically important because it is often described as less governance-conditional than Western aid.
- That makes regime type a plausible determinant of where Chinese aid goes.
- Your causal question: does a more authoritarian regime lead a country to receive more Chinese aid?
- Preview the result carefully: the raw and controlled association is positive, but the estimate becomes smaller and statistically indistinguishable from zero in the strictest specification currently in the code.

Possible opening sentence:

Chinese development finance has become a major part of the global aid landscape, raising an important political question: do more authoritarian governments receive more Chinese aid than more democratic ones?

Possible thesis sentence:

The current results suggest a strong positive cross-sectional association between authoritarian regime type and Chinese aid receipts, but that relationship weakens once the analysis relies on within-country over-time variation, which cautions against a strong causal claim.

### 2. Theory and Hypotheses

Target length: `200-250` words

Core job of this section:
- Explain why regime type might affect Chinese aid allocation.
- State a primary hypothesis and one competing expectation.

Points to cover:
- Chinese aid is often described as being less tied to democracy, transparency, or governance conditions.
- Authoritarian governments may be more willing to accept opaque financing arrangements or large state-led infrastructure projects.
- China may also prefer dealing with centralized political systems that can approve projects quickly and negotiate directly with ruling elites.
- Competing explanation: aid allocation may reflect economic opportunity, project demand, strategic geography, or development need rather than regime type itself.

Suggested hypotheses:
- `H1:` More authoritarian countries receive more Chinese aid.
- `H2:` The estimated relationship should shrink after adding controls for income, corruption, poverty, and inequality if part of the bivariate relationship is due to confounding.

### 3. Data and Variable Construction

Target length: `200-250` words

Core job of this section:
- Describe the two datasets.
- Explain how they were merged.
- Define the main dependent variable, main independent variable, and controls.

Points to cover:
- AidData Global Chinese Development Finance data, filtered to observations recommended for aggregates.
- Our World in Data indicators for regime type and controls.
- Time coverage used in the current script: `2013-2021`.
- Outcome variable: yearly Chinese aid received, measured as `Adjusted Amount (Constant USD 2021)` and transformed as `log(1 + aid)`.
- Main regressor: `autocracy_score = 3 - political_regime`, where higher values mean more authoritarian regimes.
- Controls in the current model: logged GDP per capita, corruption perception index, extreme poverty, and Gini inequality.
- The script treats country-years with no aid record as zero aid.

Language you can reuse:

The analysis merges AidData records on Chinese development finance with country-level political and economic indicators from Our World in Data. Aid is aggregated to the country-year level and matched to regime type and covariates for the overlapping 2013-2021 period.

Important revision note for the final submission:

If you revise the project to match the assignment rules, this section should explain how the panel was collapsed into a single cross-section per country, such as by averaging variables over the study period or using a final-period snapshot.

### 4. Methodology

Target length: `200-250` words

Core job of this section:
- Explain the progressive regression strategy.
- Justify the controls.
- Mention diagnostics and current limits.

Current code structure:
- `M1:` bivariate OLS of `log_china_aid ~ autocracy_score`
- `M2:` OLS with controls
- `M3:` OLS with country and year fixed effects

Interpretation strategy:
- `M1` shows the unconditional relationship.
- `M2` tests whether the relationship survives adjustment for major observable confounders.
- `M3` is the strictest model in the current code because it compares changes within countries over time while accounting for common year shocks.

Diagnostics currently available:
- Variance inflation factors are moderate, with the highest VIF around `5.45` for logged GDP per capita.

Important revision note for the final submission:
- The assignment requires a cross-sectional design, so the final memo should likely replace the fixed-effects discussion with a cross-sectional multiple regression using one observation per country.
- The appendix still needs fuller Gauss-Markov diagnostics beyond VIF, such as residual plots and a heteroskedasticity check.

### 5. Findings

Target length: `300-350` words

Core job of this section:
- Present the coefficient on regime type across models.
- Explain how the conclusion changes across specifications.
- Keep the interpretation substantive but cautious.

Results to report:
- `M1:` `autocracy_score = 3.39`, `p < 0.001`
- `M2:` `autocracy_score = 3.31`, `p < 0.001`
- `M3:` `autocracy_score = 1.84`, `p = 0.108`, `95% CI = [-0.41, 4.09]`

Interpretation to emphasize:
- In the bivariate and controlled models, more authoritarian countries appear to receive more Chinese aid.
- The coefficient remains positive after adding controls, which suggests the relationship is not explained away by the observed covariates alone.
- In the fixed-effects model, the estimate remains positive but loses statistical significance, which weakens the claim that changes toward greater authoritarianism within a country are associated with higher aid receipts.
- A careful conclusion is that authoritarian regime type is strongly associated with Chinese aid in between-country comparisons, but the current strongest specification does not provide decisive evidence of a robust causal effect.

Secondary findings you can mention briefly:
- In `M2`, higher GDP per capita is associated with less Chinese aid.
- In `M2`, less-corrupt countries appear to receive less aid because the CPI coefficient is negative.
- In `M2`, inequality is positively associated with aid.
- These control results become unstable and mostly insignificant in the fixed-effects model.

Possible paragraph starter:

The main pattern in the results is straightforward: countries with more authoritarian regimes receive significantly more Chinese aid in simpler specifications, but the magnitude of that relationship falls and the estimate loses statistical significance in the most demanding specification.

### 6. Limitations and Conclusion

Target length: `150-200` words

Core job of this section:
- State what the analysis can and cannot claim.
- End with a policy-relevant takeaway.

Limitations to include:
- Potential omitted variable bias from strategic alignment, natural resources, trade exposure, or geopolitical relevance.
- Measurement limits in both the aid data and the regime indicators.
- The current script is panel-based, which is not aligned with the assignment's cross-sectional requirement.
- Reverse causality is still possible if aid affects domestic political conditions.

Conclusion points:
- Chinese aid appears more common in authoritarian settings in simple comparisons.
- The evidence becomes weaker under stricter specification.
- The safest conclusion is that regime type is correlated with Chinese aid allocation, but stronger causal claims require a design that better addresses unobserved confounding.

Possible closing sentence:

Overall, the analysis suggests that authoritarianism is an important correlate of Chinese aid allocation, but the evidence is not yet strong enough to conclude confidently that regime type itself causes higher aid flows.

## Figures, tables, and appendix checklist

### Main memo items

- `Figure 1:` use the existing bar plot in `output/selection_model_regime_barplot.png` as a descriptive figure.
- `Table 1:` create a regression table with `M1`, `M2`, and either the current `M3` or a revised cross-sectional final model.
- Add a short note below the table explaining that the dependent variable is `log(1 + Chinese aid in constant USD 2021)`.

### Appendix items still needed

- Residual versus fitted plot
- Distribution of residuals or QQ plot
- Heteroskedasticity test
- VIF table
- Short discussion of omitted variable bias risk

## Recommended write-up order

1. Write the findings section first because the coefficients are already exported.
2. Then draft the introduction and theory so they match the actual result.
3. Revise the data and methods sections once you decide whether to keep the current panel analysis for internal drafting or convert the final submission to a cross-section.
4. Finish with limitations and the appendix diagnostics.

## Most important issue before final submission

If the assignment rules are binding as written, the main thing to fix is not the prose but the design: the final submitted analysis should be converted from a country-year panel into a single cross-section of countries before the memo is finalized.
