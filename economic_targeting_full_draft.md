# Does Chinese Development Finance Prioritize Economically Strategic Sectors Over Social Sectors?

## Introduction

Chinese development finance is often presented as an alternative to Western aid, but the most important question is not only how much China gives. It is also where China gives it. Sectoral allocation reveals donor priorities because different sectors carry different political and economic payoffs. If Chinese finance is oriented toward infrastructure, extraction, and commercially valuable state-led projects, then Chinese aid should be much larger in sectors such as industry, mining, energy, and transport than in education, health, and related social services.

This paper tests that proposition directly. Using AidData project records and country-year controls from Our World in Data for the period 2013 to 2021, I compare Chinese aid commitments in strategically valuable sectors against aid commitments in social sectors. The results are clear. Strategic sectors absorb about 66.7 percent of total Chinese aid in the sample, while social sectors receive only about 5.0 percent. The same pattern holds in regression analysis. The coefficient on the strategic-sector indicator remains positive and highly statistically significant in every specification, including a country-year fixed-effects model that compares strategic and social allocations within the same country-year. The evidence therefore suggests that Chinese development finance is heavily concentrated in economically strategic sectors rather than broadly distributed across social sectors.

## Background and Literature

The broader aid-allocation literature has long argued that foreign aid reflects donor interests as well as recipient need. Alesina and Dollar (2000) show that aid is shaped by political and strategic relationships rather than simply by poverty or developmental urgency. That general logic is especially relevant for China, whose overseas financing is often associated with large infrastructure projects, state-to-state bargaining, and commercially significant sectors rather than with the social-sector orientation typical of some Western aid agencies.

Research on Chinese state financing reinforces this interpretation. Dreher, Fuchs, Parks, Strange, and Tierney (2018) show that Chinese official finance follows a different allocation logic from traditional Western aid and is more closely tied to commercial and strategic considerations. Brautigam and Gallagher (2014) further emphasize the role of commodity-backed finance in China’s external economic relationships, especially in sectors linked to natural resources and infrastructure. Their argument is important because it identifies a direct mechanism through which energy, mining-related projects, and transport investments can serve both recipient development goals and Chinese material interests at the same time.

The transport and infrastructure literature points in the same direction. Marson, Belingheri, and Parola (2021) describe China’s central role in African transport infrastructure and show why ports, roads, and logistics corridors are not neutral development investments. They shape how goods move and who controls the resulting commercial flows. More recently, Bluhm, Melesky, and Reuter (2025) show that Chinese infrastructure finance can influence the spatial diffusion of economic activity, highlighting that these projects are economically transformative and not merely symbolic diplomatic gestures.

Taken together, this literature suggests a clear expectation. If Chinese development finance is strongly shaped by economic statecraft, then the largest aid volumes should go to sectors that support extraction, energy supply, and logistics connectivity. That expectation does not imply that China provides no social aid at all. Instead, it implies a strong imbalance in favor of strategically valuable sectors.

## Theory and Hypotheses

The theory behind this paper is straightforward. Strategic sectors create more direct economic value for the donor than social sectors do. Industry and mining projects can support resource access and broader productive capacity. Energy projects can stabilize supply and support industrial growth. Transport and storage projects reduce the cost of moving commodities, manufactured goods, and intermediate inputs. All three sectors are also highly compatible with large-scale project finance, engineering contracts, and state-linked implementation, which makes them especially well suited to China’s overseas financing model.

Social sectors operate differently. Education, health, water and sanitation, and civil society programs can generate important welfare gains, but they generally do not provide the same direct commercial payoff to the donor. They are also often smaller in monetary scale and less tied to large capital-intensive project finance. If China’s aid program is partly structured around economic statecraft, the expected pattern is not necessarily zero social aid. The expected pattern is that strategic-sector commitments are much larger than social-sector commitments.

This logic also helps explain why strategic-sector concentration may matter for inequality. Investment centered on extraction, energy, and logistics can produce concentrated rents, favor politically connected firms or elites, and deliver narrower economic gains than broad-based social spending. The present dataset does not allow a direct test of whether Chinese aid later causes inequality to increase, but it does allow an evaluation of whether Chinese aid is concentrated in sectors more compatible with unequal growth patterns than with broad social provision.

The empirical hypotheses are therefore:

- `H1:` Chinese aid commitments are significantly larger in strategic sectors than in social sectors.
- `H2:` The strategic-sector premium remains after controlling for recipient-country income, corruption, poverty, and inequality.
- `H3:` The strategic-sector premium remains even when comparing strategic and social allocations within the same country-year.

## Data and Variable Construction

The analysis combines two datasets already provided in the project repository. The first is AidData’s project-level dataset on Chinese development finance. I use the `aid-data` sheet from `chinese-aid-data-2000-2021.xlsx` and restrict the sample to observations marked `Recommended For Aggregates = Yes`, which follows AidData’s guidance for avoiding double counting and dropping inappropriate records. The second dataset is `our-world-in-data-2013-2023.xlsx`, which provides country-year controls for GDP per capita, corruption, extreme poverty, and inequality.

The sample period is restricted to 2013 through 2021 because that is the overlap between the two datasets. The unit of analysis is a funded country-year-sector-family observation. Sector families are constructed directly from AidData sector codes. Strategic sectors include industry, mining, and construction (`320`), energy (`230`), and transport and storage (`210`). Social sectors include education (`110`), health (`120`), population and reproductive health (`130`), water supply and sanitation (`140`), government and civil society (`150`), and other social infrastructure and services (`160`).

The dependent variable is `log(1 + family-level Chinese aid in constant USD 2021)`. The main independent variable is `strategic_family`, coded `1` for strategic observations and `0` for social observations. This coding is intentionally simple. It is designed to capture whether Chinese aid is systematically larger in sectors with clearer commercial and infrastructural value than in sectors oriented toward social provision.

The descriptive statistics already show a sharp contrast. Strategic sectors account for roughly 66.7 percent of total Chinese aid in the sample, while social sectors account for about 5.0 percent. The average funded strategic project is about 297.6 million dollars, compared with only 14.3 million dollars for the average funded social project. Among country-years in which both sector families receive funding, the median strategic share is about 93.5 percent.

## Methodology

I estimate four regression models. Model 1 is a bivariate OLS regression of logged family-level aid on the strategic-sector indicator. Model 2 adds controls for logged GDP per capita, the Corruption Perceptions Index, the extreme poverty rate, and the Gini index. Model 3 adds country and year fixed effects, which absorb stable country characteristics and common shocks over time. Model 4 is the strictest specification: it adds country-year fixed effects, allowing the comparison of strategic and social allocations within the same country-year.

This progression matters substantively. The bivariate model shows the raw gap between strategic and social aid. The controlled model tests whether that gap is simply due to richer, poorer, more corrupt, or more unequal countries receiving different kinds of aid. The fixed-effects models go further by asking whether the strategic premium survives once comparisons are narrowed to within-country variation and ultimately to within the same country-year.

The standard errors are robust throughout. Because the Breusch-Pagan diagnostics indicate heteroskedasticity in every model, Models 1 and 2 use HC1 heteroskedasticity-robust standard errors, while Models 3 and 4 use cluster-robust standard errors by country. This means the reported inference is not based on homoskedasticity assumptions that the data reject.

## Findings

The descriptive findings already tell a strong story. Chinese aid is not evenly spread across sectors. Most of the money is concentrated in sectors that have immediate economic and infrastructural value. Strategic sectors receive about two-thirds of all Chinese aid in the sample, while social sectors receive only a small fraction. The project size gap is even more striking: the average funded strategic project is more than twenty times larger than the average funded social project. In country-years where both sector families are funded, strategic aid almost always dominates, with a median strategic share of about 93.5 percent.

The regression results confirm that this is not merely a descriptive artifact. In Model 1, the coefficient on `strategic_family` is 3.58 with a p-value below 0.001. In Model 2, after adding controls, the coefficient remains 3.41 and highly significant. In Model 3, with country and year fixed effects, the coefficient is 3.36 and still highly significant. In Model 4, the country-year fixed-effects model, the coefficient remains 2.91 with a p-value below 0.001.

These are very large effects. Because the dependent variable is logged, the coefficients indicate a substantial multiplicative gap in aid size rather than a small linear difference. Most importantly, the result survives the within-country-year comparison. That means that even when China finances both strategic and social sector families in the same country and the same year, the strategic allocation is still dramatically larger. This is the strongest evidence in the paper because it removes many broad cross-country explanations for the pattern.

The control variables do not overturn the core result. The strategic-sector coefficient remains large and stable across specifications, which suggests that the concentration of Chinese aid in strategic sectors is not simply a byproduct of recipient income, corruption, poverty, or inequality. The stability of the coefficient across models is one of the most persuasive features of the results.

## Relating the Findings to Inequality

The inequality argument should be made carefully. The current regression design does not estimate whether Chinese aid causes inequality to rise in later years. In this paper, inequality enters the model as a contemporaneous control, not as the dependent variable. For that reason, it would be too strong to conclude directly that Chinese aid increases inequality.

What the evidence does support is a more cautious but still important claim. Chinese aid is concentrated in sectors that are more compatible with unequal growth patterns than broad social provision. Strategic sectors such as extraction-related industry, energy, and logistics often create concentrated gains, large rents, and capital-intensive growth that does not necessarily diffuse widely across the population. By contrast, education, health, and sanitation investments are more directly associated with broad social welfare.

The inequality-related figures reinforce this interpretation. The strategic-sector premium appears across the inequality distribution rather than only in low-inequality settings. That means the pattern is not limited to one narrow subset of countries. A defensible interpretation is therefore that China’s allocation pattern is consistent with a development model that can preserve or intensify inequality, even though this paper does not directly test post-aid changes in the Gini index.

## Limitations

This paper has three important limitations. First, it measures sectoral targeting rather than the underlying size of a recipient country’s mining or logistics industries. The analysis therefore identifies where Chinese aid goes, not whether China specifically targets countries because those sectors are already large in the domestic economy. Second, the distinction between strategic and social sectors is theoretically motivated but still a simplification. Sector codes are proxies for donor-benefiting investment rather than direct measures of Chinese material gain. Third, the paper does not directly observe project contracts, commodity collateralization, or later changes in inequality, so some of the interpretation remains inferential rather than fully causal.

These limitations matter, but they do not erase the central empirical pattern. The project does not prove every mechanism behind Chinese aid allocation, but it does show very clearly that aid volume is overwhelmingly concentrated in sectors with immediate strategic and economic value.

## Conclusion

This paper asked whether Chinese development finance is systematically larger in economically strategic sectors than in social sectors. The answer is yes. Strategic sectors receive the overwhelming majority of Chinese aid in the sample, strategic projects are far larger on average than social projects, and the strategic premium remains large and statistically significant across every regression specification. The result even survives a country-year fixed-effects model that compares strategic and social allocations within the same country-year.

The strongest conclusion is not that China exclusively avoids social aid. The data do not support that claim. Instead, the evidence shows that the financial scale of Chinese development finance is overwhelmingly concentrated in sectors tied to extraction, energy, and logistics. That pattern is consistent with the view that Chinese aid is shaped by economic statecraft and infrastructure-centered commercial interests rather than by broad social-sector development priorities alone.

The paper also suggests a broader implication. Even without directly proving that Chinese aid increases inequality over time, the concentration of aid in strategic sectors points toward a development model more compatible with concentrated rents and uneven gains than with broad-based welfare provision. That is an important political economy insight and a strong foundation for future work that would place inequality itself on the left-hand side of the model.

## References

Alesina, Alberto, and David Dollar. 2000. "Who Gives Foreign Aid to Whom and Why?" *Journal of Economic Growth* 5(1): 33-63.

Bluhm, Richard, Martin Melesky, and Oliver Reuter. 2025. "Connective Financing: Chinese Infrastructure Projects and the Diffusion of Economic Activity in Developing Countries." *Journal of Urban Economics* 145: 103730.

Brautigam, Deborah, and Kevin P. Gallagher. 2014. "Bartering Globalization: China's Commodity-Backed Finance in Africa and Latin America." *Global Policy* 5(3): 346-352.

Dreher, Axel, Andreas Fuchs, Bradley Parks, Austin M. Strange, and Michael J. Tierney. 2018. "Apples and Dragon Fruits: The Determinants of Aid and Other Forms of State Financing from China to Africa." *International Studies Quarterly* 62(1): 182-194.

Marson, Marco, Paola Belingheri, and Francesco Parola. 2021. "China's role in African infrastructure and capital projects: Focus on the transport sector." *Research in Transportation Economics* 88: 101111.
