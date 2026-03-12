# Does Chinese Development Finance Prioritize Economically Strategic Sectors Over Social Sectors?

## Introduction

Chinese development finance is often debated in terms of total volume, debt exposure, or geopolitical influence. A more direct way to understand Chinese priorities is to examine where the money goes. If Chinese aid is shaped by economic statecraft, it should be concentrated in mining-related industry, energy, and transport rather than in education, health, sanitation, and related social services.

This paper tests that expectation using AidData project records and country-year controls from Our World in Data for 2013-2021. I group Chinese aid into a strategic sector family and a social sector family, then estimate whether commitments are systematically larger in the strategic family.

The results are clear. Strategic sectors absorb about 66.7 percent of all Chinese aid in the sample, while social sectors receive only about 5.0 percent. The average funded strategic project is about $297.6 million, compared with only $14.3 million for the average funded social project. Most importantly, the strategic-sector premium remains large and statistically significant in every regression model, including a country-year fixed-effects specification that compares strategic and social allocations within the same country and year. The evidence therefore suggests that Chinese development finance is heavily concentrated in economically strategic sectors rather than broadly distributed across social sectors.

## Background and Literature

The broader literature on aid allocation shows that foreign aid is rarely distributed on developmental need alone. Alesina and Dollar (2000) argue that aid is shaped by donor political and strategic interests, which provides the basic logic for this project.

This expectation is supported by scholarship on Chinese development finance itself. Dreher, Fuchs, Parks, Strange, and Tierney (2018) show that Chinese official finance follows a different logic from more traditional Western aid and is tied more closely to commercial and strategic considerations. Brautigam and Gallagher (2014) show how Chinese lending can be connected to energy and natural resource access through commodity-backed finance.

Transport and infrastructure research also helps explain why these sectors matter. Marson, Belingheri, and Parola (2021) highlight China’s role in major transport projects, while Bluhm, Melesky, and Reuter (2025) show that Chinese infrastructure finance can reshape economic geography. Taken together, this literature suggests that energy, mining-related industry, and transport projects offer unusually clear strategic value.

The theoretical implication is not that China provides no social aid. Rather, it is that the financial scale of Chinese aid should be much larger in sectors that support extraction, connectivity, and capital-intensive infrastructure.

## Theory and Hypotheses

The theory behind the regression is straightforward. Strategic sectors provide direct economic value in ways that social sectors generally do not. Mining-related industry can support resource access and production capacity. Energy projects help secure supply and industrial development. Transport and storage reduce the cost of moving commodities and goods. These sectors also fit China’s comparative advantage in large project finance and state-linked implementation.

Social sectors matter for development, but their political economy is different. Education, health, sanitation, and civil society projects are typically smaller in scale, less connected to extractive or logistical value chains, and less likely to generate immediate commercial returns for the donor. If China’s aid program is strongly shaped by economic statecraft, then the expected pattern is not zero social spending but a large and persistent strategic-sector premium.

This distinction also has implications for inequality. Investments in extraction, energy, and transport can produce concentrated gains, politically mediated rents, and uneven regional benefits. Social-sector investments are more likely to diffuse benefits broadly across households. The present dataset does not permit a direct causal test of whether Chinese aid later increases inequality, but it does allow an assessment of whether Chinese finance is concentrated in sectors more consistent with unequal growth than with broad social provision.

The empirical hypotheses are:

- `H1:` Chinese aid commitments are significantly larger in strategic sectors than in social sectors.
- `H2:` The strategic-sector premium remains after controlling for recipient-country income, corruption, poverty, and inequality.
- `H3:` The strategic-sector premium remains even when comparing strategic and social allocations within the same country-year.

## Data and Methods

The analysis uses two data sources already in the project repository. The first is AidData’s project-level dataset on Chinese development finance. I use only observations marked `Recommended For Aggregates = Yes`, which follows AidData’s guidance for usable aggregate analysis. The second is the Our World in Data country-year file, which provides controls for GDP per capita, corruption, extreme poverty, and inequality. The sample is restricted to 2013-2021, the overlap between the two datasets.

The unit of analysis is a funded country-year-sector-family observation. I classify industry, mining, and construction (`320`), energy (`230`), and transport and storage (`210`) as strategic sectors. I classify education (`110`), health (`120`), population and reproductive health (`130`), water supply and sanitation (`140`), government and civil society (`150`), and other social infrastructure and services (`160`) as social sectors. The dependent variable is `log(1 + family-level Chinese aid in constant USD 2021)`, and the main independent variable is `strategic_family`, coded `1` for strategic observations and `0` for social observations.

I estimate four models. Model 1 is a bivariate OLS regression of logged aid on the strategic-sector indicator. Model 2 adds controls for logged GDP per capita, the Corruption Perceptions Index, extreme poverty, and the Gini index. Model 3 adds country and year fixed effects. Model 4 is the strictest specification: it includes country-year fixed effects, so the comparison is made between strategic and social allocations within the same country and year.

The standard errors are robust throughout. Breusch-Pagan tests indicate heteroskedasticity in every model, so Models 1 and 2 use HC1 heteroskedasticity-robust standard errors, while Models 3 and 4 use cluster-robust standard errors by country. This is important because the inference reported below does not rely on a homoskedasticity assumption that the data reject.

## Findings

The descriptive evidence already points strongly toward sectoral concentration. Strategic sectors account for about 66.7 percent of total Chinese aid in the sample, while social sectors account for only about 5.0 percent. The gap is also visible in project size: the average funded strategic project is about $297.6 million, whereas the average funded social project is only about $14.3 million. Among country-years where both sector families receive funding, the median strategic share is about 93.5 percent.

The regression results confirm that this pattern is not a simple descriptive artifact. In Model 1, the coefficient on `strategic_family` is 3.58 with `p < 0.001`. In Model 2, after controlling for income, corruption, poverty, and inequality, the coefficient remains 3.41 with `p < 0.001`. In Model 3, after adding country and year fixed effects, the estimate is 3.36 with `p < 0.001`. Finally, in Model 4, the country-year fixed-effects model, the coefficient remains 2.91 with `p < 0.001`.

These coefficients are substantively large. Because the dependent variable is logged, they imply a substantial multiplicative gap in aid size rather than a marginal difference of a few dollars. The most important result is Model 4. Once the analysis compares strategic and social allocations within the same country and year, the strategic premium is still large and highly statistically significant.

The controls do not overturn the main conclusion. The strategic-sector coefficient is remarkably stable across the four models, which suggests that the concentration of Chinese aid in strategic sectors is not simply a side effect of other observable recipient characteristics.

## Inequality, Limitations, and Conclusion

The inequality argument should be framed carefully. This paper does not directly test whether Chinese aid causes inequality to rise over time because the Gini index enters the model as a contemporaneous control rather than as the dependent variable.

What the evidence does support is a narrower claim. Chinese aid is concentrated in sectors that are plausibly more compatible with unequal growth patterns than broad social provision. Strategic sectors such as mining-related industry, energy, and transport often generate concentrated rents, capital-intensive growth, and politically mediated gains. The project’s inequality figures show that the strategic premium appears across the inequality distribution rather than only in low-inequality settings.

The paper nevertheless has important limitations. First, it measures sectoral targeting rather than the actual size of recipient-country mining or logistics industries. Second, the distinction between strategic and social sectors is theoretically grounded but still a proxy for donor-benefiting investment. Third, the design does not directly observe contract structure, collateralization, or later changes in inequality.

Overall, the evidence strongly supports the conclusion that Chinese development finance is heavily concentrated in economically strategic sectors. China does provide social-sector aid, so an exclusive-investment claim would be too strong. But in financial scale, the balance is overwhelmingly tilted toward extraction-related industry, energy, and transport. That pattern survives controls, country and year fixed effects, and even country-year fixed effects. The most defensible takeaway is therefore that Chinese aid is structured less like broadly distributed social development assistance and more like a financing model centered on sectors with clear commercial, infrastructural, and strategic value.

## References

Alesina, Alberto, and David Dollar. 2000. "Who Gives Foreign Aid to Whom and Why?" *Journal of Economic Growth* 5(1): 33-63.

Bluhm, Richard, Martin Melesky, and Oliver Reuter. 2025. "Connective Financing: Chinese Infrastructure Projects and the Diffusion of Economic Activity in Developing Countries." *Journal of Urban Economics* 145: 103730.

Brautigam, Deborah, and Kevin P. Gallagher. 2014. "Bartering Globalization: China's Commodity-Backed Finance in Africa and Latin America." *Global Policy* 5(3): 346-352.

Dreher, Axel, Andreas Fuchs, Bradley Parks, Austin M. Strange, and Michael J. Tierney. 2018. "Apples and Dragon Fruits: The Determinants of Aid and Other Forms of State Financing from China to Africa." *International Studies Quarterly* 62(1): 182-194.

Marson, Marco, Paola Belingheri, and Francesco Parola. 2021. "China's role in African infrastructure and capital projects: Focus on the transport sector." *Research in Transportation Economics* 88: 101111.
