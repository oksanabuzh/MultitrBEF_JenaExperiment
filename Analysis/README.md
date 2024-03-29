# Analysis

Contains codes for testing the effects of each diversity facet on each stock and flow across the food web diversity facets: "sowndiv" - species richness, "numfg" - number of functional groups, "leg.ef" - presence of legumes, "gr.ef" - presence of grasses, "sh.ef" - presence of short herbs, "th.ef"- presence of tall herbs.


# Files:

"run_model_all_variables" - function to run linear models for multiple responses "y"" and multiple predictors "x", where the target predictor "x first" is fitted first right after "block". The function extracts the following results for each model: response, predictor, partial R2 for each predictor, model R2, p-value (depending on type of sum of squares  to be used), estimate (slope), back-transformed effect size; standardized effect size, lambda (from boxcox transformation); and model formula.
    

"Analysis_ExtractEffects" - uses "run_model_all_variables" function to fit models for the results in both main text and supplementary. Models are fitted for each target predictor (i.e., diversity facet) and response (stock and flux in the food web): 
y ~ block + variable_of_interest + all_other_variables.
Results are returned, with different p-values for main text and supplementary. 
In the main text the p-value is calculated from type I sum of squares (using anova() function), where the target predictor is fitted first (after block).
In the supplementary the p-value is calculated from type II sum of squares (using car::Anova() function), where the target predictor is fitted last after all predictors.


"Analysis_ExtractEffects_Shoot_Root" - considers standing biomass stocks of root and shoot separately, while in the "Analysis_ExtractEffects" plant standing biomass stock is considered as one variable (as sum of stocks of root and shoot).


"check_results" file includes few selected models fitted manually, and compared their results with the results derived by the function "run_model_all_variables".


"Proportion of effects" - filters out the significant effects of each facet on each flow and stock and gives summery statistics for the proportions of effects of each biodiversity facet


"Context_dependency_Stocks" - tests whether the plant diversity effects on stocks depend on  trophic contexts. Specifically we tested if the standardized effects of each target predictor on biomass stocks (n=12) differ between 
        - trophic groups ("Plants", "Detritus", "Herbivores", "Decomposers","Omnivores", "Carnivores"),         - trophic levels (1st, 2nd, 3rd), and between 
        - aboveground and belowground subnetworks (AG vs BG: Aboveground, Belowground). 
An analysis of variance with sequential sum of squares was applied (using anova()).


"Context_dependency_Fluxes" tests whether the plant diversity effects on fluxes depend on  trophic contexts, i.e. the comparison of the effect of plant diversity on energy fluxes (n=41) between 
         - ecosystem functions (Plant respiration, Herbivory, Decomposition, Predation, Detritus production, and Respiration), 
         - aboveground and belowground subnetworks (AG vs BG: Aboveground, Belowground).
An analysis of variance with sequential sum of squares was applied (using anova()).

"Fig.3_bargraph" - code for creating plots (main text) for the proportion of flows and stocks in the network that were significantly affected by each plant diversity facet when it was fitted first in the model.

"Fig.4_new_4&5combined" - code for creating plots for the biodiversity effects on fluxes grouped by ecosystem functions, and AG vs BG and on stocks grouped by trophic groups, trophic levels and AG vs BG.

"Fig.4_new_4&5combined_with significance" - same as "Fig.4_new_4&5combined" + highlighted significance of differences among groups

"R2_Stocks" - code for creating bubble chart showing variance explained by each plant diversity facet for each standing stock (partial R2 from regression models) and for the Total Network Stock (sum of partial R2 for all stocks across the tropic network).


"R2_Fluxes" - code for creating bubble chart showing variance explained by each plant diversity facet for fluxes (partial R2 from regression models) and for the Total Network Energy Flow (sum of partial R2 for all fluxes across the tropic network).

# NOT USED CODE (to remove)

"Context_dependency_Fluxes_vs_Stocks" - tests whether the plant diversity effects differ berween dimensions of ecosystem functions: fluxes vs stocks

"Fig.4" - code for creating plots for the biodiversity effects on fluxes grouped by ecosystem functions, and AG vs BG.

"Fig.5" - code for creating plots for the biodiversity effects on stocks grouped by trophic groups, trophic levels and AG vs BG.
