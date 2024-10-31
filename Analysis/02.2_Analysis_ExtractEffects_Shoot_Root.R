# Extract effect sizes for standing biomass stocks of root and shoot separately
# to be used in Fig. 5 and metaanalysys for AG vs BG effects

# Packages
library(tidyverse)
library(glue)
# load function to calculate models
source("Analysis/run_model_all_variables.R")

# Prepare the data --------------------------------------------------------
Index <- readr::read_csv("Data/Shoot_Root_separately/Stocks_Shoot_Root.csv")
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


# Analysis -----------------------------------------------------------------

# Models are calculated with the following form:
# y ~ block + variable_of_interest + all_other_variables
# type I sum of squares are used


# create a table with all models to be calculated
all_models <- expand.grid(
  x = c("sowndiv", "numfg", "leg.ef", "gr.ef", "sh.ef", "th.ef", 
        "FDbranch", "FDis"),
  y = names(Index[17:18])
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

write_csv(mod, "Results/mod_ShootRoot.csv")
