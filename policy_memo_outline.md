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

### 2. Background and Literature Anchor

Target length: `250-350` words

Core job of this section:
- Situate the project in the academic literature on aid allocation.
- Show why political similarity between donor and recipient is a plausible mechanism.
- Establish that China itself sits on the authoritarian end of standard democracy measures.

Background logic to develop:
- The broad aid-allocation literature argues that donor governments do not allocate aid on need alone; they also respond to strategic and political considerations.
- A natural extension of that literature is that donors may prefer governments that are politically or ideologically closer to themselves, because similarity reduces bargaining friction, increases trust, and makes policy coordination easier.
- For China specifically, that mechanism is plausible if Chinese state financing is more compatible with centralized, less politically constrained governments.

Academic sources you can use in this section:
- `Alesina and Dollar (2000), Journal of Economic Growth`: a foundational aid-allocation study showing that foreign aid is shaped by political and strategic relationships, not just poverty or development need. Link: <https://doi.org/10.1023/A:1009874203400>
- `Lskavyan (2021), Journal of International Development`: finds that larger donor-recipient ideological differences are associated with lower aid flows, which gives you a direct academic basis for the similarity argument. Link: <https://doi.org/10.1002/jid.3579>
- `Heurlin (2024), British Journal of Political Science`: develops the idea of `institutional complementarity`, arguing that aid from authoritarian donors can be especially compatible with authoritarian recipient institutions. Link: <https://doi.org/10.1017/S0007123423000503>
- `Lührmann, Tannenberg, and Lindberg (2018), Democratization`: introduces the `Regimes of the World` framework used to classify countries as closed autocracies, electoral autocracies, electoral democracies, or liberal democracies. Link: <https://doi.org/10.1080/13510347.2018.1453063>
- `V-Dem Institute, Democracy Report 2025`: the latest V-Dem report classifies China as a `closed autocracy`, which is consistent with the measure used in your project. Link: <https://www.v-dem.net/documents/55/v-dem_dr2025_lowres.pdf>

Point to make explicitly about China:
- In your own OWID file, China is coded `0` on `political-regime` for every year from `2013` through `2021`.
- Under the `Regimes of the World` coding used by OWID, `0` corresponds to `closed autocracy`.
- That means the project is not merely assuming China is authoritarian; it is using a standard academic democracy classification that places China at the most authoritarian end of the four-category regime scale.

Possible paragraph structure:

Paragraph 1:

Start from the general aid literature. Explain that donors often use aid to pursue political or strategic goals, so aid allocation is not expected to be politically neutral.

Paragraph 2:

Introduce the similarity mechanism. Explain that governments may direct aid toward politically similar partners because shared governing styles, lower transparency demands, and more compatible state institutions can reduce transaction costs and make large projects easier to negotiate and implement.

Paragraph 3:

Connect that logic to China directly. Note that China is classified as a closed autocracy in the V-Dem framework and is coded as such in the same democracy variable used in your analysis, which makes the expectation of authoritarian-affinity in aid allocation theoretically plausible.

Possible reusable language:

Existing scholarship shows that aid allocation reflects donor interests as well as recipient need. Building on that insight, ideological and institutional similarity between donor and recipient governments may shape aid flows because politically similar partners face fewer conflicts over governance standards, monitoring, and project implementation. This logic is especially relevant for China, which leading academic democracy datasets classify as a closed autocracy.

### 3. Theory and Hypotheses

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

Additional theory sentences you can use:

- If donor and recipient governments are institutionally similar, they may find it easier to cooperate on large state-led projects because they have more compatible expectations about bargaining, oversight, and conditionality.
- If China is less likely than liberal democratic donors to condition financing on electoral accountability or governance reform, then authoritarian recipient governments may face lower political costs when accepting Chinese aid.
- A competing explanation is that what looks like regime affinity may actually reflect omitted strategic factors such as trade, natural resources, diplomatic alignment, or regional importance.

### 4. Data and Variable Construction

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

Background sentence to add here if useful:

The main independent variable is well aligned with the theoretical discussion because it comes from a standard regime classification that places China itself in the `closed autocracy` category during the study period.

### 5. Methodology

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

### 6. Findings

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

### 7. Limitations and Conclusion

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

## Background Draft Notes

### Short background draft

One way to motivate this project is through the broader political economy of aid allocation. Classic work by Alesina and Dollar argues that foreign aid is shaped not only by recipient need but also by donor political and strategic interests. A more specific extension of that logic is that donors may favor recipients that are politically closer to them. Lskavyan provides direct evidence consistent with this view, finding that greater ideological distance between donor and recipient is associated with lower levels of economic aid. For an authoritarian donor such as China, this raises the possibility that regime similarity matters because governments with similar political institutions may be easier to work with and less likely to resist state-led, low-conditionality financing arrangements.

That mechanism is especially plausible given how major academic democracy datasets classify China. In the `Regimes of the World` framework developed by Lührmann, Tannenberg, and Lindberg, the most authoritarian regime category is `closed autocracy`. The V-Dem Institute's Democracy Report 2025 places China in that category, and the same coding appears in the OWID regime variable used in this project: China is coded `0` for every year from `2013` to `2021`, which corresponds to `closed autocracy`. This makes the paper's core hypothesis straightforward: if authoritarian donors tend to work more easily with authoritarian recipients, then more authoritarian countries should receive more Chinese development finance.

### Compressed version if you need fewer words

The academic literature suggests that aid is often allocated for political as well as developmental reasons. Alesina and Dollar show that donor interests matter, while Lskavyan finds that greater ideological distance between donors and recipients is associated with less aid. That framework is relevant for China because academic democracy datasets classify China as a closed autocracy. In the V-Dem framework, and in the OWID regime variable used here, China sits at the most authoritarian end of the regime scale. If donor-recipient political similarity lowers bargaining and monitoring frictions, then more authoritarian recipient governments may be especially likely to receive Chinese aid.

## Academic Source List

- `Alesina, Alberto, and David Dollar. 2000. "Who Gives Foreign Aid to Whom and Why?" Journal of Economic Growth 5(1): 33-63.` Link: <https://doi.org/10.1023/A:1009874203400>
- `Lskavyan, Vahe. 2021. "Donor-recipient ideological differences and economic aid." Journal of International Development 33(4): 595-619.` Link: <https://doi.org/10.1002/jid.3579>
- `Heurlin, Christopher. 2024. "How Authoritarian Is Foreign Aid?" British Journal of Political Science 54(4): 1274-1293.` Link: <https://doi.org/10.1017/S0007123423000503>
- `Lührmann, Anna, Marcus Tannenberg, and Staffan I. Lindberg. 2018. "Regimes of the World (RoW): Opening New Avenues for the Comparative Study of Political Regimes." Democratization 25(7): 1321-1341.` Link: <https://doi.org/10.1080/13510347.2018.1453063>
- `V-Dem Institute. 2025. Democracy Report 2025: 25 Years of Autocratization.` Link: <https://www.v-dem.net/documents/55/v-dem_dr2025_lowres.pdf>

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
