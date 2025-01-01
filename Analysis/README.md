# Analysis

Contains codes for testing the effects of each diversity facet on each stock and flow across the food web diversity facets: "sowndiv" - species richness, "numfg" - number of functional groups, "leg.ef" - presence of legumes, "gr.ef" - presence of grasses, "sh.ef" - presence of short herbs, "th.ef"- presence of tall herbs, "Fdbranch"" - total branch length of the functional-trait dendrogram, "Fdis"" - average distance of species traits from the mean trait value.


# Files:

"Trait_analysis" - prepares traits and community matrix and calculates functional diversity.

"run_model_all_variables" - function to run linear models for multiple responses "y"" and multiple predictors "x", where the target predictor "x first" is fitted first right after "block". The function extracts the following results for each model: response, predictor, partial R2 for each predictor, model R2, p-value (depending on type of sum of squares  to be used), estimate (slope), back-transformed effect size; standardized effect size, lambda (from boxcox transformation); and model formula.
    

"Analysis_ExtractEffects" - uses "run_model_all_variables" function to fit models for the results in both main text and supplementary. Models are fitted for each target predictor (i.e., diversity facet) and response (stock and flux in the food web): 
y ~ block + variable_of_interest + all_other_variables.
Results are returned, with different p-values for main text and supplementary. 
In the main text the p-value is calculated from type I sum of squares (using anova() function), where the target predictor is fitted first (after block).
In contrast to all other predictors Fdbranch is fitted alone (after Block)  in the  model.
In the supplementary the p-value is calculated from type II sum of squares (using car::Anova() function), where the target predictor is fitted last after all predictors.
In addition in the supplementary, species richness was fitted alone (after Block) in the model (predictor named "sowndiv_alone"). 

"Analysis_ExtractEffects_Shoot_Root" - considers standing biomass stocks of root and shoot separately, while in the "Analysis_ExtractEffects" plant standing biomass stock is considered as one variable (as sum of stocks of root and shoot).


"Context_dependency" - tests whether the plant diversity effects on stocks and fluxes depend on  trophic contexts. 
Specifically we tested if the standardized effects of each target predictor on biomass stocks (n=12) differ between 
        - trophic groups ("Plants", "Detritus", "Herbivores", "Decomposers","Omnivores", "Carnivores"),         - trophic levels (1st, 2nd, 3rd), and between 
        - aboveground and belowground subnetworks (AG vs BG: Aboveground, Belowground). 
An analysis of variance with sequential sum of squares was applied (using anova()).
For fluxes we compared the effect of plant diversity on energy fluxes (n=41) between 
         - ecosystem functions (Plant respiration, Herbivory, Decomposition, Predation, Detritus production, and Respiration), 
         - aboveground and belowground subnetworks (AG vs BG: Aboveground, Belowground).
An analysis of variance with sequential sum of squares was applied (using anova()).


"Fig.3_Bargraph" - code for creating plots (main text) for the proportion of flows and stocks in the network that were significantly affected by each plant diversity facet when it was fitted first in the model.

"Fig4_Contex_dependency" - code for creating plots for the biodiversity effects on fluxes grouped by ecosystem functions, and AG vs BG, and on stocks grouped by trophic groups, trophic levels and AG vs BG.


## Contents of the folders

### `Appendix/`

Contains codes for producing figures and tables in Appendix. See Results/Appendix/Readme.md for details.
