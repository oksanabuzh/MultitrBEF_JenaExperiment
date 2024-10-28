# Do the plant diversity effects on stocks depend on  trophic contexts?

# Comparison of the effect of plant diversity on biomass stocks (n=12) between 
#         - trophic groups ("Plants", "Detritus", "Herbivores", "Decomposers","Omnivores", "Carnivores"), 
#         - trophic levels (1st, 2nd, 3rd), and between 
#         - aboveground and belowground subnetworks (AG vs BG: Aboveground, Belowground). 
# An analysis of variance with sequential sum of squares was applied.
# ----------------------------------------------------

rm(list=ls(all=TRUE))

# Packages
library(tidyverse)
library(broom)


# data  stocks -----
df_main <- read_csv("Results/mod_main_text.csv")
str(df_main)

group <- read_csv ("Data/EF_grouped.csv")
names(group)

df_all <- df_main %>% 
  left_join(group, by = join_by(response)) %>% 
  filter(Dimens=="stock") %>% 
  mutate(Tr_level=as_factor(Trophic_level)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus","Herbivores","Decomposers","Omnivores","Carnivores")))


str(df_all)
df_all$Tr_level
df_all$Tr_Group
factor(df_all$AG_BG)

# Data when shoots and roots considered separately 
df_ShootRoot <- read_csv("Results/mod_ShootRoot.csv")
str(df_ShootRoot)

group_ShootRoot <- read_csv ("Data/Shoot_Root_separately/EF_grouped_Shoot_Root.csv")
str(group_ShootRoot)

df_AG_BG <- bind_rows(df_main, df_ShootRoot) %>% 
  left_join(group_ShootRoot, by = join_by(response)) %>% 
  filter(!response=="Plants.Stock") %>% 
  filter(Dimens=="stock")

str(df_AG_BG)

# metaanalysis stocks -----

Meta_Tr_Gr_Lvl <- df_all %>% 
  filter(!predictor=="RaoQ")%>% 
  nest_by(predictor) %>%
  mutate(Model = list(car::Anova(lm(effect_size_st ~ Tr_Group, data = data))),
         Model_2 = list(car::Anova(lm(effect_size_st ~ Tr_level, data = data)))) %>%
reframe(m1=tidy(Model), m2=tidy(Model_2))  %>% # same as summarise()
  pivot_longer(cols = c(m1, m2)) %>% 
  unnest(value) %>% 
  select(-name) %>% 
  filter(!term=="Residuals")


Meta_AG_BG <- df_AG_BG %>% 
  filter(!predictor=="RaoQ")%>% 
  nest_by(predictor) %>%
  mutate(Model_3 = list(car::Anova(lm(effect_size_st ~ AG_BG, data = data)))) %>%
  reframe(m3=tidy(Model_3))  %>% 
  unnest(m3) %>% 
  filter(!term=="Residuals") 

Meta_AG_BG

Meta_Stock <- Meta_Tr_Gr_Lvl %>% 
  bind_rows(Meta_AG_BG) %>% 
  rename(F.value=statistic) %>% 
  arrange(predictor)

Meta_Stock
