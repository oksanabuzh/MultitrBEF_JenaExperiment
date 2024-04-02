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
library(rcompanion)
library(scales) 

# Data main
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

# Metaanalyses----

#1. sowndiv ----

# Data
# subsets of data for sowndiv only
sowndiv <- df_all %>% 
  filter(predictor=="sowndiv")

sowndiv_AG_BG <- df_AG_BG %>% 
  filter(predictor=="sowndiv")

# Predictors
sowndiv$Tr_Group
sowndiv$Tr_level
factor(sowndiv_AG_BG$AG_BG)

## Tr_Group----
m1_SR<-lm(effect_size_st ~  Tr_Group, data=sowndiv)

par(mfrow = c(2, 2))
plot(m1_SR)
par(mfrow = c(1, 1))

min(sowndiv$effect_size_st)

m1_SR<-lm(sqrt(effect_size_st+0.2) ~  Tr_Group, data=sowndiv)

par(mfrow = c(2, 2))
plot(m1_SR)
par(mfrow = c(1, 1))

summary(m1_SR)
anova(m1_SR)


# Marginal means and pairwise differences 

emmeans::emmeans(m1_SR, list(pairwise ~ Tr_Group))
# to add letters for post-hoc test:
model_means_TG <- multcomp::cld(emmeans::emmeans(m1_SR, list(pairwise ~ Tr_Group)),  
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

## Tr_level ----
m2_SR<-lm(effect_size_st ~  Tr_level, data=sowndiv)

par(mfrow = c(2, 2))
plot(m2_SR)
par(mfrow = c(1, 1))

summary(m2_SR)
anova(m2_SR)

# Marginal means and pairwise differences 

emmeans::emmeans(m2_SR, list(pairwise ~ Tr_level))
# to add letters for post-hoc test:
model_means_TL <- multcomp::cld(emmeans::emmeans(m2_SR, list(pairwise ~ Tr_level)),  
                                 Letters = letters, adjust = "Tukey")
model_means_TL

library(tidyverse) 
library(scales)    

ggplot(data = model_means_TL) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_level), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_level),
                 width = 0.05, color = "black") +
   geom_text(aes(y = emmean, x = Tr_level, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 





## AG_BG ----
sowndiv_AG_BG$AG_BG

m3_SR<-lm(effect_size_st ~  AG_BG, data=sowndiv_AG_BG)

par(mfrow = c(2, 2))
plot(m3_SR)
par(mfrow = c(1, 1))

min(sowndiv$effect_size_st)

m3_SR<-lm(log(effect_size_st+0.2) ~  AG_BG, data=sowndiv_AG_BG)

par(mfrow = c(2, 2))
plot(m3_SR)
par(mfrow = c(1, 1))

summary(m3_SR)
anova(m3_SR)

# Marginal means and pairwise differences 

emmeans::emmeans(m3_SR, list(pairwise ~ AG_BG))
# to add letters for post-hoc test:
multcomp::cld(emmeans::emmeans(m3_SR, list(pairwise ~ AG_BG)),  
              type="response",
              Letters = letters, adjust = "none")

model_means_AG_BG <- multcomp::cld(emmeans::emmeans(m3_SR, list(pairwise ~ AG_BG)),  
                                Letters = letters, adjust = "Tukey")
model_means_AG_BG



ggplot(data = model_means_AG_BG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = AG_BG), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = AG_BG),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = AG_BG, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 



#2. numfg ----

# Data
# subsets of data for numfg only
numfg <- df_all %>% 
  filter(predictor=="numfg")

numfg_AG_BG <- df_AG_BG %>% 
  filter(predictor=="numfg")

# Predictors
numfg$Tr_Group
numfg$Tr_level
factor(numfg_AG_BG$AG_BG)

## Tr_Group----
m1_FG<-lm(effect_size_st ~  Tr_Group, data=numfg)

par(mfrow = c(2, 2))
plot(m1_FG)
par(mfrow = c(1, 1))


summary(m1_FG)
anova(m1_FG)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_FG, list(pairwise ~ Tr_Group))
# to add letters for post-hoc test:
model_means_TG <- multcomp::cld(emmeans::emmeans(m1_FG, list(pairwise ~ Tr_Group)),  
                                   Letters = letters, adjust = "Tukey")
model_means_TG



ggplot(data = model_means_TG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_Group), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_Group),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_Group, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 


## Tr_level ----
m2_FG<-lm(effect_size_st ~  Tr_level, data=numfg)

par(mfrow = c(2, 2))
plot(m2_FG)
par(mfrow = c(1, 1))

summary(m2_FG)
anova(m2_FG)

# Marginal means and pairwise differences 

emmeans::emmeans(m2_FG, list(pairwise ~ Tr_level))
# to add letters for post-hoc test:
model_means_TL <- multcomp::cld(emmeans::emmeans(m2_FG, list(pairwise ~ Tr_level)),  
                                Letters = letters, adjust = "Tukey")
model_means_TL



ggplot(data = model_means_TL) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_level), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_level),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_level, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black")

## AG_BG ----
numfg_AG_BG$AG_BG

m3_FG<-lm(effect_size_st ~  AG_BG, data=numfg_AG_BG)

par(mfrow = c(2, 2))
plot(m3_FG)
par(mfrow = c(1, 1))

min(numfg_AG_BG$effect_size_st)

m3_FG<-lm(sqrt(effect_size_st+0.3) ~  AG_BG, data=numfg_AG_BG)

par(mfrow = c(2, 2))
plot(m3_FG)
par(mfrow = c(1, 1))

summary(m3_FG)
anova(m3_FG)

# Marginal means and pairwise differences 

emmeans::emmeans(m3_FG, list(pairwise ~ AG_BG))
# to add letters for post-hoc test:
model_means_AG_BG <- multcomp::cld(emmeans::emmeans(m3_FG, list(pairwise ~ AG_BG)),  
                                   Letters = letters, adjust = "Tukey")
model_means_AG_BG



ggplot(data = model_means_AG_BG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = AG_BG), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = AG_BG),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = AG_BG, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 




#3. leg.ef ----

# Data
# subsets of data for leg.ef only
leg <- df_all %>% 
  filter(predictor=="leg.ef")

leg_AG_BG <- df_AG_BG %>% 
  filter(predictor=="leg.ef")

# Predictors
leg$Tr_Group
leg$Tr_level
factor(leg_AG_BG$AG_BG)

## Tr_Group----
m1_leg<-lm(effect_size_st ~  Tr_Group, data=leg)

par(mfrow = c(2, 2))
plot(m1_leg)
par(mfrow = c(1, 1))


summary(m1_leg)
anova(m1_leg)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_leg, list(pairwise ~ Tr_Group))
# to add letters for post-hoc test:
model_means_TG <- multcomp::cld(emmeans::emmeans(m1_leg, list(pairwise ~ Tr_Group)),  
                                Letters = letters, adjust = "Tukey")
model_means_TG



ggplot(data = model_means_TG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_Group), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_Group),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_Group, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 



## Tr_level ----
m2_leg<-lm(effect_size_st ~  Tr_level, data=leg)

par(mfrow = c(2, 2))
plot(m2_leg)
par(mfrow = c(1, 1))

summary(m2_leg)
anova(m2_leg)

# Marginal means and pairwise differences 

emmeans::emmeans(m2_leg, list(pairwise ~ Tr_level))
# to add letters for post-hoc test:
model_means_TL <- multcomp::cld(emmeans::emmeans(m2_leg, list(pairwise ~ Tr_level)),  
                                Letters = letters, adjust = "Tukey")
model_means_TL



ggplot(data = model_means_TL) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_level), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_level),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_level, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black")


## AG_BG ----
leg_AG_BG$AG_BG

m3_leg<-lm(effect_size_st ~  AG_BG, data=leg_AG_BG)

par(mfrow = c(2, 2))
plot(m3_leg)
par(mfrow = c(1, 1))


summary(m3_leg)
anova(m3_leg)

# Marginal means and pairwise differences 

emmeans::emmeans(m3_leg, list(pairwise ~ AG_BG))
# to add letters for post-hoc test:
model_means_AG_BG <- multcomp::cld(emmeans::emmeans(m3_leg, list(pairwise ~ AG_BG)),  
                                   Letters = letters, adjust = "Tukey")
model_means_AG_BG



ggplot(data = model_means_AG_BG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = AG_BG), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = AG_BG),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = AG_BG, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 





#4. gr.ef ----

# Data
# subsets of data for grass only
grass <- df_all %>% 
  filter(predictor=="gr.ef")

grass_AG_BG <- df_AG_BG %>% 
  filter(predictor=="gr.ef")

# Predictors
grass$Tr_Group
grass$Tr_level
factor(grass_AG_BG$AG_BG)

## Tr_Group----
m1_gr<-lm(effect_size_st ~  Tr_Group, data=grass)

par(mfrow = c(2, 2))
plot(m1_gr)
par(mfrow = c(1, 1))

min(grass$effect_size_st)

m1_gr<-lm(log(effect_size_st+0.3) ~  Tr_Group, data=grass)

par(mfrow = c(2, 2))
plot(m1_gr)
par(mfrow = c(1, 1))

summary(m1_gr)
anova(m1_gr)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_gr, list(pairwise ~ Tr_Group))
# to add letters for post-hoc test:
model_means_TG <- multcomp::cld(emmeans::emmeans(m1_gr, list(pairwise ~ Tr_Group)),  
                                Letters = letters, adjust = "Tukey")
model_means_TG



ggplot(data = model_means_TG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_Group), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_Group),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_Group, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 


## Tr_level ----
m2_gr<-lm(effect_size_st ~  Tr_level, data=grass)

par(mfrow = c(2, 2))
plot(m2_gr)
par(mfrow = c(1, 1))

summary(m2_gr)
anova(m2_gr)

# Marginal means and pairwise differences 

emmeans::emmeans(m2_gr, list(pairwise ~ Tr_level))
# to add letters for post-hoc test:
model_means_TL <- multcomp::cld(emmeans::emmeans(m2_gr, list(pairwise ~ Tr_level)),  
                                Letters = letters, adjust = "Tukey")
model_means_TL



ggplot(data = model_means_TL) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_level), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_level),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_level, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black")



## AG_BG ----
grass_AG_BG$AG_BG

m3_gr<-lm(effect_size_st ~  AG_BG, data=grass_AG_BG)

par(mfrow = c(2, 2))
plot(m3_gr)
par(mfrow = c(1, 1))

summary(m3_gr)
anova(m3_gr)

# Marginal means and pairwise differences 

emmeans::emmeans(m3_gr, list(pairwise ~ AG_BG))
# to add letters for post-hoc test:
model_means_AG_BG <- multcomp::cld(emmeans::emmeans(m3_gr, list(pairwise ~ AG_BG)),  
                                   Letters = letters, adjust = "Tukey")
model_means_AG_BG



ggplot(data = model_means_AG_BG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = AG_BG), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = AG_BG),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = AG_BG, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 




#5. sh.ef ----

# Data
# subsets of data for sh only
sh <- df_all %>% 
  filter(predictor=="sh.ef")

sh_AG_BG <- df_AG_BG %>% 
  filter(predictor=="sh.ef")

# Predictors
sh$Tr_Group
sh$Tr_level
factor(sh_AG_BG$AG_BG)

## Tr_Group----
m1_SH<-lm(effect_size_st ~  Tr_Group, data=sh)

par(mfrow = c(2, 2))
plot(m1_SH)
par(mfrow = c(1, 1))

min(sh$effect_size_st)

m1_SH<-lm(log(effect_size_st+0.2) ~  Tr_Group, data=sh)

par(mfrow = c(2, 2))
plot(m1_SH)
par(mfrow = c(1, 1))

summary(m1_SH)
anova(m1_SH)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_SH, list(pairwise ~ Tr_Group))
# to add letters for post-hoc test:
model_means_TG <- multcomp::cld(emmeans::emmeans(m1_SH, list(pairwise ~ Tr_Group)),  
                                Letters = letters, adjust = "Tukey")
model_means_TG



ggplot(data = model_means_TG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_Group), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_Group),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_Group, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 

## Tr_level ----
m2_SH<-lm(effect_size_st ~  Tr_level, data=sh)

par(mfrow = c(2, 2))
plot(m2_SH)
par(mfrow = c(1, 1))

summary(m2_SH)
anova(m2_SH)

# Marginal means and pairwise differences 

emmeans::emmeans(m2_SH, list(pairwise ~ Tr_level))
# to add letters for post-hoc test:
model_means_TL <- multcomp::cld(emmeans::emmeans(m2_SH, list(pairwise ~ Tr_level)),  
                                Letters = letters, adjust = "Tukey")
model_means_TL



ggplot(data = model_means_TL) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_level), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_level),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_level, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black")




## AG_BG ----
sh_AG_BG$AG_BG

m3_SH<-lm(effect_size_st ~  AG_BG, data=sh_AG_BG)

par(mfrow = c(2, 2))
plot(m3_SH)
par(mfrow = c(1, 1))

min(sh$effect_size_st)

m3_SH<-lm(log(effect_size_st+1) ~  AG_BG, data=sh_AG_BG)

par(mfrow = c(2, 2))
plot(m3_SH)
par(mfrow = c(1, 1))

summary(m3_SH)
anova(m3_SH)

# Marginal means and pairwise differences 

emmeans::emmeans(m3_SH, list(pairwise ~ AG_BG))
# to add letters for post-hoc test:
model_means_AG_BG <- multcomp::cld(emmeans::emmeans(m3_SH, list(pairwise ~ AG_BG)),  
                                   Letters = letters, adjust = "Tukey")
model_means_AG_BG



ggplot(data = model_means_AG_BG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = AG_BG), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = AG_BG),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = AG_BG, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 

#6. th.ef ----

# Data
# subsets of data for th only
th <- df_all %>% 
  filter(predictor=="th.ef")

th_AG_BG <- df_AG_BG %>% 
  filter(predictor=="th.ef")

# Predictors
th$Tr_Group
th$Tr_level
factor(th_AG_BG$AG_BG)

## Tr_Group----
m1_TH<-lm(effect_size_st ~  Tr_Group, data=th)

par(mfrow = c(2, 2))
plot(m1_TH)
par(mfrow = c(1, 1))

summary(m1_TH)
anova(m1_TH)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_TH, list(pairwise ~ Tr_Group))
# to add letters for post-hoc test:
model_means_TG <- multcomp::cld(emmeans::emmeans(m1_TH, list(pairwise ~ Tr_Group)),  
                                Letters = letters, adjust = "Tukey")
model_means_TG



ggplot(data = model_means_TG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_Group), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_Group),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_Group, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 

## Tr_level ----
m2_TH<-lm(effect_size_st ~  Tr_level, data=th)

par(mfrow = c(2, 2))
plot(m2_TH)
par(mfrow = c(1, 1))

summary(m2_TH)
anova(m2_TH)

# Marginal means and pairwise differences 

emmeans::emmeans(m2_TH, list(pairwise ~ Tr_level))
# to add letters for post-hoc test:
model_means_TL <- multcomp::cld(emmeans::emmeans(m2_TH, list(pairwise ~ Tr_level)),  
                                Letters = letters, adjust = "Tukey")
model_means_TL



ggplot(data = model_means_TL) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Tr_level), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Tr_level),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Tr_level, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black")



## AG_BG ----
th_AG_BG$AG_BG

m3_TH<-lm(effect_size_st ~  AG_BG, data=th_AG_BG)

par(mfrow = c(2, 2))
plot(m3_TH)
par(mfrow = c(1, 1))

summary(m3_TH)
anova(m3_TH)

# Marginal means and pairwise differences 

emmeans::emmeans(m3_TH, list(pairwise ~ AG_BG))
# to add letters for post-hoc test:
model_means_AG_BG <- multcomp::cld(emmeans::emmeans(m3_TH, list(pairwise ~ AG_BG)),  
                                   Letters = letters, adjust = "Tukey")
model_means_AG_BG



ggplot(data = model_means_AG_BG) +
  theme_bw()+
  geom_point(aes(y = emmean, x = AG_BG), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = AG_BG),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = AG_BG, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 






results <-rbind(anova(m1_SR)[1,], anova(m2_SR)[1,], anova(m3_SR)[1,], 
                anova(m1_FG)[1,], anova(m2_FG)[1,], anova(m3_FG)[1,],
                anova(m1_leg)[1,],anova(m2_leg)[1,], anova(m3_leg)[1,],
                anova(m1_gr)[1,],anova(m2_gr)[1,], anova(m3_gr)[1,],
                anova(m1_SH)[1,],anova(m2_SH)[1,], anova(m3_SH)[1,],
                anova(m1_TH)[1,],anova(m2_TH)[1,],anova(m3_TH)[1,]) 


row.names(results)= c("TrGr for SR", "TL for SR", "AG_BG for SR", 
                      "TrGr for FG", "TL for FG", "AG_BG for FG",
                      "TrGr for leg", "TL for leg", "AG_BG for leg",
                      "TrGr for grass", "TL for grass", "AG_BG for grass",
                      "TrGr for SH", "TL for SH", "AG_BG for SH",
                      "TrGr for TH", "TL for TH","AG_BG for TH")

results


write.csv(results, "Results/Tables_3_4_5.csv", row.names=TRUE)
