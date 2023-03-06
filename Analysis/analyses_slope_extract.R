# Buzhdygan et al.   "Facets of plant diversity  differentially affect energy dynamics 
                  #  in grasslands depending on trophic contexts"
# in 'Ecology'

# Tests for the effects of each diversity facet on each stock and flow across the food web
# Main analysis (in the main text) and supplementary analeses

# Oksana Buzhdygan 
# oksana.buzh@fu-berlin.de
# 28.10.2022


rm(list=ls(all=TRUE))

# Prepare the data --------------------------------------------------------

setwd("C:/Users/Oksana/Nextcloud/Jena2/JenFlow (Sebastian Meyer)/JenFlow2/Effects_slopes/Fluxes")
Index <- readr::read_csv("net_ind_fluxes.csv")
str(Index)

# Clean the column headers
names(Index) # data have special characters: "->", "*" and white spaces
# Meaning of the characters and abbreviations:
## "->" is flow # rename to "_"
## "*" is environment # rename to "o"
    # (i.e. "*->" flow into the system; "->*" flow out of the system from external environmnt)
## AG - aboveground, BG - belowground
## SOM - soil organic matter

# Clean the names-----
# Index[14:66] <-janitor::clean_names(Index[14:66], case = c("mixed")) 
  # gives not very meaningful names

# Rename the food-web variables with new characters:
# "_" is flow
# "o" is out of system environment
#  Other abbreviations in the names of trophic groups are: 
   #"AG" - aboveground, "BG" - belowground, "SOM" - soil organic matter

names(Index)[14:66] <- c(
 # system inflow (carbon uptake by plants)
  "o_Plants",
 # internal flows (between trophic groups)
  "Plants_AG.Herbivores", "AG.Herbivores_AG.Carnivores", "AG.Decomposers_AG.Carnivores", 
  "Plants_AG.Omnivores",  "AG.Herbivores_AG.Omnivores", "AG.Decomposers_AG.Omnivores", 
  "AG.Litter_AG.Omnivores", "AG.Litter_AG.Decomposers", "BG.Herbivores_BG.Carnivores",
  "BG.Decomposers_BG.Carnivores", "Plants_BG.Herbivores", "SOM_BG.Decomposers", 
  "Plants_BG.Omnivores", "BG.Herbivores_BG.Omnivores", "BG.Decomposers_BG.Omnivores",
  "SOM_BG.Omnivores", "Soil.Microorganisms_BG.Omnivores", "Plants_SOM", "BG.Carnivores_SOM",               
  "BG.Herbivores_SOM", "BG.Decomposers_SOM", "BG.Omnivores_SOM", "AG.Litter_SOM",
  "Soil.Microorganisms_SOM", "Plants_AG.Litter", "AG.Herbivores_AG.Litter", "AG.Carnivores_AG.Litter",
  "AG.Omnivores_AG.Litter", "AG.Decomposers_AG.Litter", "SOM_Soil.Microorganisms",
 # system outflows (respirations)
  "Plants_o", "AG.Herbivores_o", "AG.Carnivores_o", "AG.Omnivores_o", "AG.Decomposers_o",
  "BG.Carnivores_o", "BG.Herbivores_o", "BG.Decomposers_o", "BG.Omnivores_o",
  "Soil.Microorganisms_o",
 # standing biomass stocks
  "Plants.Stock", "AG.Herbivores.Stock", "AG.Carnivores.Stock", "AG.Omnivores.Stock",
  "AG.Decomposers.Stock", "BG.Herbivores.Stock", "BG.Carnivores.Stock", "BG.Omnivores.Stock",
  "BG.Decomposers.Stock", "Soil.Microorganisms.Stock", "AG.Litter.Stock", "SOM.Stock"
       )
names(Index)


# Analyses --------------------------------------------------------------
  # tests of the effects of each diversity facet on each stock and flow across the food web

# load the generic function for the model run
source("function.R")

# Main analysis (in main text) --------------------------------------------------------
 # y ~ block + diversity facet (without any other term but block in the model

# Create a table with all combinations of responses  and predictors (x)
 # each line represents a combination of response and predictor
all_models <- expand.grid(
  x = c("sowndiv", "numfg", "leg.ef", "gr.ef", "sh.ef", "th.ef"),
  responses = names(Index[14:66])
)

# Run the `run_linear_model` function for each line of `all_models`
# ..1 stands for the first column of `all_models`, i.e. x
# ..2 stands for the second column of `all_models`, i.e. y
mod<-purrr::pmap_df(all_models, ~run_linear_model(x = ..1, y = ..2, dat = Index))
names(mod)


library(tidyverse)
mod <- cbind(all_models, mod) %>%  
  dplyr::select(-formula) %>% # remove column 'formula'
  dplyr::select(-responses) %>% # remove column 'responses' as it is the same as column "y"
  tidyr::pivot_longer(cols = estimate:lambda) %>% # transpose  from estimate to lambda columns into the rows and make one column
  tidyr::unite("id",c("name", "x"), sep="_") %>% # unite the names in this column with x using _ separation, and name the column id
  tidyr::pivot_wider(values_from = value, names_from =id) # transpose the rows in the id column into the separate columns
mod

write.csv(mod,  file = "results_main.csv")

library(tidyverse)
mod2 <- mod %>%
  select(sort(names(.))) %>% # sort the columns alphabetically
  relocate(y) # put the y column back as the first column

write.csv(mod2,  file = "results_main_ordered.csv")



# Supplementary analyses (S1, S2) --------------------------------------------------------

# models include all predictors (Block and all diversity facets) in the model 
# and extract slope for each diversity facet using type-II, i.e. as the last term fitted after block and all the other diversity facets).

# Analysis S1 ----------------------------------------------------------------------------
# species richness and number of functional groups, as predictors

responses <- names(Index[14:66]) # all stocks and flows across the food web

predictors_1 <- c("sowndiv", "numfg") # species richness and number of functional group
results_S1 <-purrr::map_dfr(responses, ~run_linear_model(x = predictors_1, y = .x, dat = Index))
results_S1
names(results_S1)

#rename columns in more meaningful names
names(results_S1) <- c("y", "formula", 
                       "estimate_sowndiv", "estimate_numfg", 
                       "BackTrans_sowndiv", "BackTrans_numfg",  # back-transformed slopes
                       "p_sowndiv", "p_numfg", 
                       "R2", "lambda"
) 
names(results_S1)

write.csv(results_S1,  file = "results_S1.csv")



# Analysis S2 ----------------------------------------------------------------------------
# species richness and presence of each functional groups, as predictors

predictors_2 <- c("sowndiv", "leg.ef", "gr.ef", "sh.ef", "th.ef") # species richness and presence of each functional groups
results_S2 <- purrr::map_dfr(responses, ~run_linear_model(x = predictors_2, y = .x, dat = Index))
results_S2
names(results_S2)



#rename columns in more meaningful names
names(results_S2) <- c("y", "formula", 
                       "estimate_sowndiv",  
                       "estimate_leg.ef", "estimate_gr.ef",
                       "estimate_sh.ef", "estimate_th.ef", 
                       "BackTrans_sowndiv",
                       "BackTrans_leg.ef", "BackTrans_gr.ef",
                       "BackTrans_sh.ef", "BackTrans_th.ef", 
                       "p_sowndiv", 
                       "p_leg.ef", "p_gr.ef",
                       "p_sh.ef", "p_th.ef",
                       "R2", "lambda"
)

names(results_S2)

write.csv(results_S2,  file = "results_S2.csv")
