# Tests for the effects of each diversity facet on each stock and flow across the food web
# Main analysis (in the main text) and supplementary analyses

library(tidyverse)
library(glue)
# load function to calculate models
source("Analysis/run_model_all_variables.R")

# Prepare the data --------------------------------------------------------
Index <- readr::read_csv("Data/net_ind_fluxes.csv")
str(Index)

# Read functional diversity indices
fun_div <- read_csv("Results/functional_diversity.csv")

# join both tables by Plot ID
Index <- Index %>% 
  left_join(fun_div, by = c("plotcode" = "plot")
  ) |> 
  relocate(
    all_of(c("FDis_pa", "FDis", "FDbranch")),
    .before = sowndiv 
  )

# Meaning of column names
names(Index) 
# "_" is flow
# "o" is out of system environment
#  Other abbreviations in the names of trophic groups are: 
# "AG" - aboveground, "BG" - belowground, "SOM" - soil organic matter

# Analysis -----------------------------------------------------------------

# Models are calculated with the following form:
# y ~ block + variable_of_interest + all_other_variables
# for Main text: type I sum of squares are used
# for Supplementary: type II sum of squares are used

# create a table with all models to be calculated
all_models <- expand.grid(
  x = c("sowndiv", "numfg", "leg.ef", "gr.ef", "sh.ef", "th.ef", "FDis_pa", "FDis", "FDbranch"),
  y = names(Index)[17:69]
)

# Analysis main text ------------------------------------------------------
# Run the `run_model_all_vars` function for each line of `all_models`
# ..1 stands for the first column of `all_models`, i.e. x
# ..2 stands for the second column of `all_models`, i.e. y
# use type 1 sum of squares (anova)
mod <- purrr::pmap_df(all_models, ~run_model_all_vars(
  dat = Index,
  first = ..1, 
  y = ..2, 
  type = 1 # type 1 sum of squares with anova()
))

str(mod)
summary(mod)

write_csv(mod, "Results/mod_main_text.csv")

# merge with groups of functions
group <- read_csv ("Data/EF_grouped.csv")
names(group)

df_all <- mod %>% 
  left_join(group, by = join_by(response)) 

write_csv(df_all, "Results/TableS3.csv")

# Analysis supplementary --------------------------------------------------
# Run the `run_model_all_vars` function for each line of `all_models`
# ..1 stands for the first column of `all_models`, i.e. x
# ..2 stands for the second column of `all_models`, i.e. y
# use type 2 sum of squares (car::Anova)
mod_supp <- purrr::pmap_df(all_models, ~run_model_all_vars(
  dat = Index,
  first = ..1, 
  y = ..2, 
  type = 2 # type 2 sum of squares with car::Anova()
))

str(mod_supp)
summary(mod_supp)

write_csv(mod_supp, "Results/mod_supp.csv")

# merge with groups of functions
group <- read_csv ("Data/EF_grouped.csv")
names(group)

df_all_supp <- mod_supp %>% 
  left_join(group, by = join_by(response)) 

write_csv(df_all_supp, "Results/TableS4.csv")

# Check results -----------------------------------------------------------

# only p-values should be different
diff <- mod %>% select(where(is.numeric)) - 
  mod_supp %>% select(where(is.numeric)) 

diff %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_point()
