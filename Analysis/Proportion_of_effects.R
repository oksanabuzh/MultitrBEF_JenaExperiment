# Summery statistics for the proportions of effects of each biodiversity facet



# Packages
library(tidyverse)
library(rcompanion)




# 1. for the analysis in the main text----

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

# summarise data

count <- dat_perc %>% 
  count(predictor, Dimens, AG_BG, Signif, Eff_sighn) %>% 
  add_count(predictor, Dimens, wt=sum(n), name = "sum") %>% 
  filter(Signif=="Signif")
count

summarised <- count %>% 
  pivot_wider(names_from = "AG_BG", values_from = "n") %>%
  mutate(AG = ifelse(is.na(AG), 0, AG),     
         AG_BG = ifelse(is.na(AG_BG), 0, AG_BG),
         BG = ifelse(is.na(BG ), 0, BG )) %>% 
  mutate(Total=AG+AG_BG+BG) %>% 
  mutate(AG_pr=round(AG*100/sum, digits = 2)) %>% 
  mutate(BG_pr=round(BG*100/sum, digits = 2)) %>% 
  mutate(Total_pr=round(Total*100/sum, digits = 2)) %>% 
  select(-Signif )

summarised

write_csv (summarised, "Results/Summary_Effects_MainText.csv")

# Calculate % total significant effects

factor(summarised$predictor)

Total <- summarised %>% 
  mutate(predictor=fct_relevel(predictor, c("sowndiv", "numfg",
                                            "leg.ef", "gr.ef",
                                            "sh.ef", "th.ef"))) %>% 
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

# 2. for the analysis in the supplementary ----

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

# summarise data

count_SI <- dat_perc_SI %>% 
  count(predictor, Dimens, AG_BG, Signif, Eff_sighn) %>% 
  add_count(predictor, Dimens, wt=sum(n), name = "sum") %>% 
  filter(Signif=="Signif")
count_SI

summarised_SI <- count_SI %>% 
  pivot_wider(names_from = "AG_BG", values_from = "n") %>%
  mutate(AG = ifelse(is.na(AG), 0, AG),     
         AG_BG = ifelse(is.na(AG_BG), 0, AG_BG),
         BG = ifelse(is.na(BG ), 0, BG )) %>% 
  mutate(Total=AG+AG_BG+BG) %>% 
  mutate(AG_pr=round(AG*100/sum, digits = 2)) %>% 
  mutate(BG_pr=round(BG*100/sum, digits = 2)) %>% 
  mutate(Total_pr=round(Total*100/sum, digits = 2)) %>% 
  select(-Signif )

summarised_SI

write_csv (summarised_SI, "Results/Summary_Effects_SI.csv")
