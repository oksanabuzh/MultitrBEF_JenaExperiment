# Do the plant diversity effects on stocks and fluxes depend on  trophic contexts?

# Comparison of the effect of plant diversity on biomass stocks (n=12) between 
#         - trophic groups ("Plants", "Detritus", "Herbivores", "Decomposers","Omnivores", "Carnivores"), 
#         - trophic levels (1st, 2nd, 3rd), and between 
#         - aboveground and belowground subnetworks (AG vs BG: Aboveground, Belowground). 
# An analysis of variance with sequential sum of squares was applied.
#
# Comparison of the effect of plant diversity on energy fluxes (n=41) between 
#         - ecosystem functions (Plant respiration, Herbivory, Decomposition, Predation, Detritus production, and Respiration), 
#         - aboveground and belowground subnetworks (AG vs BG: Aboveground, Belowground).

# ----------------------------------------------------

rm(list=ls(all=TRUE))

# Packages
library(tidyverse)
library(broom)


# data  stocks -----
df_main <- read_csv("Results/mod_main_text.csv")%>% 
  filter(!predictor=="sowndiv_alone")
str(df_main)

group <- read_csv ("Data/EF_grouped.csv")
names(group)

df_stock <- df_main %>% 
  left_join(group, by = join_by(response)) %>% 
  filter(Dimens=="stock") %>% 
  mutate(Tr_level=as_factor(Trophic_level)) %>% 
  mutate(Tr_Group=fct_relevel(Tr_Group,c("Plants","Detritus","Herbivores","Decomposers","Omnivores","Carnivores")))


str(df_stock)
df_stock$Tr_level
df_stock$Tr_Group
factor(df_stock$AG_BG)

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

Meta_Tr_Gr_Lvl <- df_stock %>% 
  filter(!predictor=="RaoQ")%>% 
  nest_by(predictor) %>%
  mutate(Model = list(car::Anova(lm(effect_size_st ~ Tr_Group, data = data))),
         Model_2 = list(car::Anova(lm(effect_size_st ~ Tr_level, data = data)))) %>%
reframe(m1=tidy(Model), m2=tidy(Model_2))  %>% # same as summarise()
  pivot_longer(cols = c(m1, m2)) %>% 
  unnest(value) %>% 
  select(-name) %>% 
  filter(!term=="Residuals")

Meta_Tr_Gr_Lvl

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


print(Meta_Stock, n=24)

write_csv(Meta_Stock, "Results/Tables_3_4_5.csv")



## Significant effect of Tr_Group ----
m1_FDbranch<-lm(effect_size_st ~  Tr_Group, data=df_stock %>% 
            filter(predictor=="FDbranch"))

anova(m1_FDbranch)


# Marginal means and pairwise differences 

emmeans::emmeans(m1_FDbranch, list(pairwise ~ Tr_Group))
# to add letters for post-hoc test:
model_means_TG <- multcomp::cld(emmeans::emmeans(m1_FDbranch, list(pairwise ~ Tr_Group)),  
                                Letters = letters, adjust = "Tukey")
model_means_TG

library(tidyverse) 
library(scales)    

ggplot(data = model_means_TG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_Group), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_Group),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_Group, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 





# Metaanalysis for  fluxes ----

df_flux <- df_main %>% 
  left_join(group, by = join_by(response)) %>% 
  filter(Dimens=="flow") 

str(df_flux)

Meta_EcosFunc <- df_flux %>% 
  filter(!predictor=="RaoQ")%>% 
  nest_by(predictor) %>%
  mutate(Model = list(car::Anova(lm(effect_size_st ~ Ecos_Function, data = data)))) %>%
  reframe(m1=tidy(Model))  %>% 
  unnest(m1) %>% 
  filter(!term=="Residuals") 

Meta_EcosFunc

Meta_AG_BG_flux <- df_flux %>% 
  filter(!AG_BG=="AG_BG") %>% 
  filter(!predictor=="RaoQ")%>% 
  nest_by(predictor) %>%
  mutate(Model = list(car::Anova(lm(effect_size_st ~ AG_BG, data = data)))) %>%
  reframe(m1=tidy(Model))  %>% 
  unnest(m1) %>% 
  filter(!term=="Residuals") 

Meta_AG_BG_flux

Meta_Flux <- Meta_EcosFunc %>% 
  bind_rows(Meta_AG_BG_flux) %>% 
  rename(F.value=statistic) %>% 
  arrange(predictor) 

Meta_Flux



write_csv(Meta_Flux, "Results/Tables_1_2.csv")

