# Tests for the effects of each diversity facet on each stock and flow across the food web
# Main analysis (in the main text) and supplementary analyses

library(tidyverse)
library(glue)
# load function to calculate models


run_model_all_vars <- function(dat, first, y, type,
                               all_vars = c(
                                 "sowndiv", "numfg", "leg.ef",
                                 "gr.ef", "sh.ef", "th.ef"
                               ),
                               boxcox = TRUE) {
  first <- as.character(first)
  y <- as.character(y)
  # remove the first variable from the list of all_vars
  all_vars <- all_vars[!(all_vars == first)]
  
  # Remove numfg from predictors if the first is not numfg
  # numfg is correlated with the other predictors
  if (first %in% c("numfg", "FDbranch_SI", "FDis")) {
    all_vars <- "log2(sowndiv)"
  } else if (first %in% c("FDbranch", "SR_SI")) {
    # FD branch is fitted alone in the model (with block)
    all_vars <- c()
  } else if (first == "sowndiv") {
    first <- "log2(sowndiv)"
    all_vars <- all_vars[!(all_vars == "numfg")]
  } else {
    all_vars <- all_vars[!(all_vars == "numfg")]
    all_vars[all_vars == "sowndiv"] <- "log2(sowndiv)"
  }
  
  # step 1: create the model formula as text --------------------------------
  model_formula <- paste0(
    y, " ~ block + ", first, " + ",
    paste0(all_vars, collapse = " + ")
  )
  
  # Remove trailing + from model formula if there
  model_formula <- gsub("\\s\\+\\s$", "", model_formula)
  
  
  # step 2: Do boxcox transformation if you want (by default it's do --------
  if (boxcox) {
    bc <- MASS::boxcox(as.formula(model_formula), data = dat, plotit = FALSE)
    # find the maximum y and extract lamda (see here: https://www.statology.org/box-cox-transformation-in-r/)
    lambda <- bc$x[which.max(bc$y)]
    if (lambda == 0) {
      model_formula <- paste0(
        "log(", y, ") ~ block + ", first, " + ",
        paste0(all_vars, collapse = " + ")
      )
      # Remove trailing + if needed
      model_formula <- gsub("\\s\\+\\s$", "", model_formula)
    } else {
      model_formula <- paste0(
        "(", y, " ^ lambda -1)/lambda ~ block + ", first, " + ",
        paste0(all_vars, collapse = " + ")
      )
      # Remove trailing + if needed
      model_formula <- gsub("\\s\\+\\s$", "", model_formula)
    }
  } else {
    # lambda is NA to show that there was no boxcox transformation
    lambda <- NA
  }
  
  # step 3: Fit the model ---------------------------------------------------
  
  model <- lm(as.formula(model_formula), data = dat)
  
  # step 4: Get model coefficients and p-values -----------------------------
  # partial R2
  r2_part <- r2glmm::r2beta(model, method = "nsj", partial = TRUE)
  r2_part <- filter(r2_part, Effect == first)
  r2_part <- round(r2_part$Rsq, 3)
  
  # p-value with anova or Anova
  if (type == 1) {
    pval <- tibble::as_tibble(anova(model)) %>%
      mutate(variable = rownames(anova(model)))
  } else if (type == 2) {
    pval <- tibble::as_tibble(car::Anova(model)) %>%
      mutate(variable = rownames(car::Anova(model)))
  } else {
    stop(paste0("Error when calculating significance. Type argument must be
                1 for anova or 2 vor car::Anova, but value is ", type, " instead."))
  }
  
  # Extract F value
  Fval <- filter(pval, variable == first) %>%
    pull(`F value`)
  
  # Extract p value
  pval <- filter(pval, variable == first) %>%
    pull(`Pr(>F)`)
  
  
  
  # Extract model results ---------------------------------------------------
  
  # estimates
  estimates <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("x") %>%
    filter(x == first) %>%
    pull(Estimate)
  
  # effect size
  # if first is sowndiv, we need to remove the log2 to extract the correct coeff
  if (first == "log2(sowndiv)") {
    first <- "sowndiv"
  } else if ("log2(sowndiv)" %in% all_vars) {
    all_vars[all_vars == "log2(sowndiv)"] <- "sowndiv"
  }
  
  # find min and max x
  x_min <- select(dat, all_of(first)) %>%
    min()
  x_max <- select(dat, all_of(first)) %>%
    max()
  
  # calculate means of all numerical predictors and add blocks
  x_all_vars <- summarise(dat, across(
    .cols = all_of(all_vars),
    .fns = \(x) mean(x, na.rm = TRUE)
  )) %>%
    expand_grid(block = unique(dat$block))
  
  # predict y for xmin and xmax
  y_min <- predict(model, newdata = x_all_vars %>%
                     mutate("{first}" := x_min)) %>%
    mean()
  y_max <- predict(model, newdata = x_all_vars %>%
                     mutate("{first}" := x_max)) %>%
    mean()
  
  # back-transform y_min and y_max
  if (lambda == 0) {
    back_y_min <- exp(y_min)
    back_y_max <- exp(y_max)
  } else {
    back_y_min <- (y_min * lambda + 1)^(1 / lambda)
    back_y_max <- (y_max * lambda + 1)^(1 / lambda)
  }
  
  # calculate back-transformed effect sizes
  effect_size <- (back_y_max - back_y_min) / (x_max - x_min)
  
  # standardize effect size
  # calculate standard deviation of x and y from data
  sd_values <- summarise(dat, across(
    .cols = all_of(c(first, y)),
    .fns = \(x) sd(x, na.rm = TRUE)
  ))
  
  # standardize the effect_size
  effect_size_st <- effect_size * (sd_values %>% pull(first) / sd_values %>% pull(y))
  
  
  
  # Combine and return results ----------------------------------------------
  
  result <- list(
    response = y,
    predictor = first,
    r2_part = r2_part,
    R2_model = summary(model)$r.squared
  )
  return(result)
}


# Prepare the data --------------------------------------------------------
Index <- readr::read_csv("Data/net_ind_fluxes.csv") %>% 
  mutate(SR_SI=log2(sowndiv)) %>% 
  relocate(
    all_of(c("SR_SI")), .before = sowndiv) %>% 
  select(-sowndiv)
str(Index)

# Read functional diversity indices
fun_div <- read_csv("Results/functional_diversity.csv") %>% 
  select(-FDis)

# join both tables by Plot ID
Index <- Index %>% 
  left_join(fun_div, by = c("plotcode" = "plot")
  ) |> 
  relocate(
    all_of(c("FDbranch")),
    .before = SR_SI  
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

# extract all possible response variables from the Index table
# start from o_Plants until the rest of the names
responses <- names(Index)[which(names(Index) == "o_Plants"):length(names(Index))]

# create a table with all models to be calculated
all_models <- expand.grid(
  x = c("SR_SI", "FDbranch"),
  y = responses
)

# run models where SR and FDbranch are alone (with block) ------------------------------------------------------
mod <- purrr::pmap_df(all_models, ~run_model_all_vars(
  dat = Index,
  first = ..1, 
  y = ..2, 
  type = 1 # type 1 sum of squares with anova()
))


## Plot R2 for Stocks -----

group <- read_csv ("Data/EF_grouped.csv")
names(group)

df_all <- mod %>% 
  left_join(group, by = join_by(response)) %>% 
  filter(Dimens=="stock") %>% 
  # mutate(r2_part=round(r2_part, digits=2)) %>% 
    mutate(predictor=fct_relevel(predictor, c("SR_SI", "FDbranch"))) %>% 
  mutate(predictor=fct_recode(predictor, 
                              "Species richness" = "SR_SI"))%>%
  mutate(Tr_level=as_factor(Trophic_level)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus","Herbivores",
                                         "Decomposers","Omnivores","Carnivores"))) %>% 
  mutate(response=fct_relevel(response,c("BG.Carnivores.Stock",
                                         "BG.Omnivores.Stock",
                                         "BG.Herbivores.Stock",
                                         "BG.Decomposers.Stock",
                                         "Soil.Microorganisms.Stock",
                                         "SOM.Stock",
                                         "Plants.Stock",
                                         "AG.Litter.Stock",
                                         "AG.Decomposers.Stock",
                                         "AG.Herbivores.Stock",
                                         "AG.Omnivores.Stock",
                                         "AG.Carnivores.Stock"))) %>% 
  mutate(response=fct_recode(response, "BG Carnivores" ="BG.Carnivores.Stock",
                             "BG Omnivores" = "BG.Omnivores.Stock",
                             "BG Herbivores" = "BG.Herbivores.Stock",
                             "BG Decomposers" = "BG.Decomposers.Stock",
                             "Soil Microorganisms" = "Soil.Microorganisms.Stock",
                             "SOM" = "SOM.Stock",
                             "Plants" = "Plants.Stock",
                             "AG Litter" = "AG.Litter.Stock",
                             "AG Decomposers" = "AG.Decomposers.Stock",
                             "AG Herbivores" = "AG.Herbivores.Stock",
                             "AG Omnivores" = "AG.Omnivores.Stock",
                             "AG Carnivores" = "AG.Carnivores.Stock"))


sum <- df_all %>% 
  group_by(predictor) %>% 
  summarise(r2_part=sum(r2_part)) %>% 
  mutate(response="Total Network Stock")



my.data <- rbind(df_all %>% 
                   select(response, predictor, r2_part),
                 sum)

levels(my.data$response)

my.data <- my.data %>% mutate(response=fct_relevel(response, c("Total Network Stock",
                                                               "BG Carnivores",
                                                               "BG Omnivores",
                                                               "BG Herbivores",
                                                               "BG Decomposers",
                                                               "Soil Microorganisms",
                                                               "SOM",
                                                               "Plants",
                                                               "AG Litter",
                                                               "AG Decomposers",
                                                               "AG Herbivores",
                                                               "AG Omnivores",
                                                               "AG Carnivores"))) 


sjPlot::set_theme(base = theme_bw(),
          axis.textsize.x = 0.9, axis.textsize.y = 0.9, axis.textcolor = "black",
          axis.title.color = "black", axis.title.size = 1.2,
          geom.linetype = 1, legend.pos = "bottom") 


#### plot ----
dodge_width <- 0.7

plot_Stocks <- ggplot(my.data%>%
                 filter(!response=="Total Network Stock"),
               aes(y =response, x = r2_part, 
                   color = predictor #reorder(predictor, desc(predictor))
                   )) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_point(position = position_dodge(width = dodge_width), size = 4) +
  geom_errorbarh(aes(xmin = 0, xmax = r2_part),
                 position = position_dodge(width = dodge_width), height = 0.1) +
  theme(legend.key=element_blank()) +
  # theme_bw()+
  theme(axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=11),
        axis.title=element_text(size=13, face="bold"), 
        legend.text = element_text(size=10)) +
  #  theme(axis.title.x=element_text(vjust=-0.1), axis.title.y=element_text(vjust=2))+
  labs(y=" ", color="Biodiversity",
       x= expression(paste("Variance explained, ", R^{2}))) %>% 
    scale_color_manual(values=c("#00BFC4", "#F8766D"))



plot_Stocks


## Plot R2 for Flows -----


df_all_ <- mod %>% 
  left_join(group, by = join_by(response)) %>% 
  filter(Dimens=="flow") %>% 
  # mutate(r2_part=round(r2_part, digits=2)) %>% 
  mutate(predictor=fct_relevel(predictor, c("SR_SI", "FDbranch"))) %>% 
  mutate(predictor=fct_recode(predictor, 
                              "Species richness" = "SR_SI"))%>%
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration")))

df_all_ %>% 
  filter(predictor=="Species richness",  Ecos_Function=="Predation", AG_BG=="AG")%>% 
  select(AG_BG, response, r2_part) #  %>%  summarise(r2_part=sum(r2_part))

grouped_ <- df_all_ %>% 
  group_by(Ecos_Function, AG_BG, predictor) %>% 
  summarise(r2_part=sum(r2_part)) %>% 
  unite("response", AG_BG:Ecos_Function, sep = " ", remove = FALSE) %>% 
  ungroup()

grouped_


sum_ <- grouped_ %>% 
  group_by(predictor) %>% 
  summarise(r2_part=sum(r2_part)) %>% 
  mutate(response="Total Network Energy Flow")%>% 
  ungroup()
sum_ 

my.data_ <- rbind(grouped_ %>% 
                   select(response, predictor, r2_part),
                 sum_)

my.data_

levels(factor(my.data_$response))


my.data_ <- my.data_ %>% 
  mutate(response=recode_factor(response, "AG_BG Respiration" = "Plant Respiration",
                                "AG Carbon uptake" = "Carbon uptake")) %>% 
  mutate(response=fct_relevel(response, c("Total Network Energy Flow",
                                          "BG Predation",
                                          "BG Herbivory", 
                                          "BG Decomposition",
                                          "BG Detritus production",
                                          "BG Respiration",
                                          "Plant Respiration",
                                          "Carbon uptake",
                                          "AG Respiration",
                                          "AG Detritus production",
                                          "AG Decomposition",
                                          "AG Herbivory",
                                          "AG Predation"))) 


max(my.data_$r2_part)



dodge_width <- 0.7

plot_Flow <- ggplot(my.data_%>%
                      filter(!response=="Total Network Energy Flow"),
                      aes(y =response, x = r2_part, 
                          color = reorder(predictor, desc(predictor)))) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_point(position = position_dodge(width = dodge_width), size = 4) +
  geom_errorbarh(aes(xmin = 0, xmax = r2_part),
                 position = position_dodge(width = dodge_width), height = 0.1) +
  theme(legend.key=element_blank()) +
  # theme_bw()+
  theme(axis.text.x = element_text(size=10), 
        axis.text.y = element_text(size=11),
        axis.title=element_text(size=13, face="bold"), 
        legend.text = element_text(size=10)) +
  #  theme(axis.title.x=element_text(vjust=-0.1), axis.title.y=element_text(vjust=2))+
  labs(y=" ", color="Biodiversity",
       x= expression(paste("Variance explained, ", R^{2}))) 



plot_Flow

              
