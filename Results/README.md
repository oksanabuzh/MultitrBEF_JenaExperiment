# Results

# Files for main text:

`Summary_Effects_MainText.csv` - proportion of significant positive and negative plant diversity effects for AG subnetwork, BG subnetwork and Total network (all effects) for each diversity facet; data generated in `Analysis/Fig3_Bargraph.R`.

`mod_main_text.csv`- contains results of analysis of variance with sequential sum of squares (type I) for each plant diversity facet when it was fitted first in the model before all other diversity facets (i.e., analysis in the main text of the paper, Fig.2-3). Fdbranch and sowndiv_alone were fitted alone (after Block) in the model (for details see "formula" column). Same as `Main Analysis.xlsx` in the main directory. Results are generated in `Analysis/Analysis_ExtractEffects.R` using function `Analysis/run_model_all_variables.R`.

`mod_supp.csv`- contains results of analysis of variance with sum of squares (type II) for each plant diversity facet when it was fitted last in the model after all other diversity facets (i.e., analysis in Appendix: Fig. S3-Fig.S4). Same as `Sensitivity Analysis.xlsx` in the main directory. Results are generated in `Analysis/Analysis_ExtractEffects.R` using function `Analysis/run_model_all_variables.R`.

`mod_ShootRoot.csv`- contains results for shoot and root standing stock separately. Used is analysis of variance with sequential sum of squares (type I) for each plant diversity facet when it was fitted first in the model before all other diversity facets (i.e., analysis in the main text of the paper). Fdbranch and sowndiv_alone were fitted alone (after Block) in the model (for details see "formula" column). Results are generated in `Analysis/Analysis_ExtractEffects.R` using function `Analysis/run_model_all_variables.R`.

`functional_diversity.R` - contains functional diversity measures calculated using functional traits and community matrix in`Analysis/Trait_analysis.R`. Fdbranch - total branch length of the functional-trait dendrogram (Petchey and Gaston 2002), Fdis - average distance of species traits from the mean trait value (Laliberte and Legendre 2010).

`Tables_1_2.csv` -  anova() results for difference of plant diversity effect on fluxes between  ecosystem functions and AG vs BGsubnetworks. Results are generated in `Analysis/03.1_Context_dependency.R`.

`Tables_3_4_5.csv` -  anova() results for difference of plant diversity effect on stocks between  trophic groups, trophic levels and AG vs BG subnetworks. Results are generated in `Analysis/03.1_Context_dependency.R`.

`Table_S4.csv` - correlation in responses of stocks and flows to plant diversity. An analysis of variance with sequential sum of squares was applied separately for aboveground and belowground subnetworks. Results are generated in `Analysis/Appendix/Fig.S7_TableS3_mismatch_stocks_vs_flows.R`.
