# Summery statistics for the proportions of effects of each biodiversity facet;
# filtered significant effects of each facet on each flow and stock



# Packages
library(tidyverse)
library(rcompanion)




# 1. Main text----
# for the analysis in the main text

# Data
df_main <- read_csv("Results/mod_main_text.csv")
str(df_main)


group <- read_csv ("Data/EF_grouped.csv")
names(group)

dat_perc <- merge(df_main, group)
dat_perc
str(dat_perc)

# Create new columns needed for the further analysis
dat_perc <- dat_perc %>% 
  mutate(Signif=case_when(p < 0.050 ~ "Signif", p >= 0.050 ~ "Non-sign")) %>% # create column with significant vs non-significant effects
  mutate(Eff_sighn=case_when(effect_size_st < 0 ~ "negative", p >= 0 ~ "positive")) # create column with direction of effect (positive vs negative)

dat_perc

# Summary of effects ----
# summarise data for the significant effects for each facet:

## sowndiv----
### filters out the significant effects of sowndiv on stocks
dat_perc %>% 
  filter(predictor=="sowndiv",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc %>% 
  filter(predictor=="sowndiv",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)

## numfg----
### filters out the significant effects of numfg on stocks
dat_perc %>% 
  filter(predictor=="numfg",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc %>% 
  filter(predictor=="numfg",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)




## leg.ef ----
### filters out the significant effects of leg.ef on stocks
dat_perc %>% 
  filter(predictor=="leg.ef",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc %>% 
  filter(predictor=="leg.ef",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)


## gr.ef ----
### filters out the significant effects of gr.ef on stocks
dat_perc %>% 
  filter(predictor=="gr.ef",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc %>% 
  filter(predictor=="gr.ef",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)





## sh.ef ----
### filters out the significant effects of sh.ef on stocks
dat_perc %>% 
  filter(predictor=="sh.ef",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc %>% 
  filter(predictor=="sh.ef",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)


## th.ef ----
### filters out the significant effects of th.ef on stocks
dat_perc %>% 
  filter(predictor=="th.ef",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc %>% 
  filter(predictor=="th.ef",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)

###########################-
# Percentages of significant effects ----
# calculate percentages of significant effects

count <- dat_perc %>% 
  count(predictor, Dimens, AG_BG, Signif, Eff_sighn) %>% 
  add_count(predictor, Dimens, wt=sum(n), name = "sum") %>% 
  filter(Signif=="Signif")
count

summarised <- count %>% 
  mutate(predictor=fct_relevel(predictor, c("sowndiv", "numfg",
                                            "leg.ef", "gr.ef",
                                            "sh.ef", "th.ef"))) %>% 
  pivot_wider(names_from = "AG_BG", values_from = "n") %>%
  mutate(AG = ifelse(is.na(AG), 0, AG),     
         AG_BG = ifelse(is.na(AG_BG), 0, AG_BG),
         BG = ifelse(is.na(BG ), 0, BG )) %>% 
  mutate(Total=AG+AG_BG+BG) %>% 
  mutate(AG_pr=round(AG*100/sum, digits = 2)) %>% 
  mutate(BG_pr=round(BG*100/sum, digits = 2)) %>% 
  mutate(Total_pr=round(Total*100/sum, digits = 2)) %>% 
  select(-Signif ) %>% 
  arrange(predictor, Dimens)

summarised

write_csv (summarised, "Results/Summary_Effects_MainText.csv")


summarised %>%   
   filter(Dimens=="flow")  

summarised %>%   
  filter(Dimens=="stock")   

# Calculate % total significant effects

factor(summarised$predictor)

Total <- summarised %>% 
    group_by(predictor, Dimens) %>% 
  summarise(all=mean(sum), sign=sum(Total)) %>% 
  mutate(prcnt=round(sign*100/all, digits = 2)) 

# Fluxes
Total %>%
  filter(Dimens=="flow") %>% 
  select(predictor, sign, prcnt)

# Stocks
Total %>%
  filter(Dimens=="stock") %>% 
  select(predictor, sign, prcnt)

######################-
# 2. Supplementary ----
#for the analysis in the supplementary

df_SI <- read_csv("Results/mod_supp.csv")
str(df_SI)


group <- read_csv ("Data/EF_grouped.csv")
names(group)

dat_perc_SI <- merge(df_SI, group)
dat_perc_SI
str(dat_perc_SI)

# Create new columns needed for the further analysis
dat_perc_SI <- dat_perc_SI %>% 
  mutate(Signif=case_when(p < 0.050 ~ "Signif", p >= 0.050 ~ "Non-sign")) %>% # create column with significant vs non-significant effects
  mutate(Eff_sighn=case_when(effect_size_st < 0 ~ "negative", p >= 0 ~ "positive")) # create column with direction of effect (positive vs negative)

dat_perc_SI

# Summary of effects ----
# summarise data for the significant effects for each facet:

## sowndiv----
### filters out the significant effects of sowndiv on stocks
dat_perc_SI %>% 
  filter(predictor=="sowndiv",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc_SI %>% 
  filter(predictor=="sowndiv",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)


## numfg----
### filters out the significant effects of numfg on stocks
dat_perc_SI %>% 
  filter(predictor=="numfg",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc_SI %>% 
  filter(predictor=="numfg",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)



## leg.ef ----
### filters out the significant effects of leg.ef on stocks
dat_perc_SI %>% 
  filter(predictor=="leg.ef",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc_SI %>% 
  filter(predictor=="leg.ef",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)




## gr.ef ----
### filters out the significant effects of gr.ef on stocks
dat_perc_SI %>% 
  filter(predictor=="gr.ef",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc_SI %>% 
  filter(predictor=="gr.ef",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)





## sh.ef ----
### filters out the significant effects of sh.ef on stocks
dat_perc_SI %>% 
  filter(predictor=="sh.ef",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc_SI %>% 
  filter(predictor=="sh.ef",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)



## th.ef ----
### filters out the significant effects of th.ef on stocks
dat_perc_SI %>% 
  filter(predictor=="th.ef",
         Dimens=="stock",
         Signif=="Signif") %>% 
  select(Tr_Group, Trophic_level, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus",
                                         "Herbivores","Decomposers","Omnivores","Carnivores"))) %>% 
  arrange(Tr_Group)

#### ... on flows
dat_perc_SI %>% 
  filter(predictor=="th.ef",
         Dimens=="flow",
         Signif=="Signif") %>% 
  select(Ecos_Function, response, AG_BG, r2_part, effect_size_st, predictor) %>% 
  mutate(effect_size_st=round(effect_size_st, 2),
         r2_part=round(r2_part, 2)) %>% 
  mutate(Ecos_Function=recode_factor(Ecos_Function, "Carbon_uptake" = "Carbon uptake",
                                     "Detritus_production" = "Detritus production")) %>% 
  mutate(Ecos_Function=fct_relevel(Ecos_Function,c("Carbon uptake", "Herbivory", "Decomposition", 
                                                   "Predation", "Detritus production", "Respiration"))) %>%
  arrange(Ecos_Function)


#####################################-
# Percentages of significant effects ----

count_SI <- dat_perc_SI %>% 
  count(predictor, Dimens, AG_BG, Signif, Eff_sighn) %>% 
  add_count(predictor, Dimens, wt=sum(n), name = "sum") %>% 
  filter(Signif=="Signif")
count_SI

summarised_SI <- count_SI %>% 
  mutate(predictor=fct_relevel(predictor, c("sowndiv", "numfg",
                                            "leg.ef", "gr.ef",
                                            "sh.ef", "th.ef"))) %>% 
  pivot_wider(names_from = "AG_BG", values_from = "n") %>%
  mutate(AG = ifelse(is.na(AG), 0, AG),     
         AG_BG = ifelse(is.na(AG_BG), 0, AG_BG),
         BG = ifelse(is.na(BG ), 0, BG )) %>% 
  mutate(Total=AG+AG_BG+BG) %>% 
  mutate(AG_pr=round(AG*100/sum, digits = 2)) %>% 
  mutate(BG_pr=round(BG*100/sum, digits = 2)) %>% 
  mutate(Total_pr=round(Total*100/sum, digits = 2)) %>% 
  select(-Signif ) %>% 
  arrange(predictor, Dimens)




summarised_SI


summarised_SI %>%   
  filter(Dimens=="flow")  

summarised_SI %>%   
  filter(Dimens=="stock")   


write_csv (summarised_SI, "Results/Summary_Effects_SI.csv")



# Calculate % total significant effects

factor(summarised_SI$predictor)

Total <- summarised_SI %>% 
  group_by(predictor, Dimens) %>% 
  summarise(all=mean(sum), sign=sum(Total)) %>% 
  mutate(prcnt=round(sign*100/all, digits = 2)) 




# Fluxes
Total %>%
  filter(Dimens=="flow") %>% 
  select(predictor, sign, prcnt)

# Stocks
Total %>%
  filter(Dimens=="stock") %>% 
  select(predictor, sign, prcnt)
