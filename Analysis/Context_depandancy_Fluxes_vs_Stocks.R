# Do the plant diversity effects on EFs depend on the dimension of EFs (whether it is measured as flux or stock)?

# Result, we found that effects of plant diversity on EFs do not depend on whether the EF is flux or stock
# ----------------------------------------------------

rm(list=ls(all=TRUE))

# Packages
library(tidyverse)

# Data 
df_main <- read_csv("Results/mod_main_text.csv")
str(df_main)

group <- read_csv ("Data/EF_grouped.csv")
names(group)

df_all <- df_main %>% 
  left_join(group, by = join_by(response)) 

str(df_all)

# Predictors
unique(df_all$Dimens)


# Metaanalyses----

#1. sowndiv ----

# Data
# subsets of data for sowndiv only
sowndiv <- df_all %>% 
  filter(predictor=="sowndiv")


##  Dimens----
m1_SR<-lm(effect_size_st ~  Dimens, data=sowndiv)

par(mfrow = c(2, 2))
plot(m1_SR)
par(mfrow = c(1, 1))

summary(m1_SR)
anova(m1_SR)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_SR, list(pairwise ~ Dimens))
# to add letters for post-hoc test:
multcomp::cld(emmeans::emmeans(m1_SR, list(pairwise ~ Dimens)),  
    type="response",
    Letters = letters, adjust = "none")


#2. numfg ----

# Data
# subsets of data for numfg only
numfg <- df_all %>% 
  filter(predictor=="numfg")

##  Dimens----
m1_FG<-lm(effect_size_st ~   Dimens, data=numfg)

par(mfrow = c(2, 2))
plot(m1_FG)
par(mfrow = c(1, 1))


summary(m1_FG)
anova(m1_FG)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_FG, list(pairwise ~  Dimens))
# to add letters for post-hoc test:
multcomp::cld(emmeans::emmeans(m1_FG, list(pairwise ~  Dimens)),  
              type="response",
              Letters = letters, adjust = "none")


#3. leg.ef ----

# Data
# subsets of data for leg.ef only
leg <- df_all %>% 
  filter(predictor=="leg.ef")
##  Dimens----
m1_leg<-lm(effect_size_st ~   Dimens, data=leg)

par(mfrow = c(2, 2))
plot(m1_leg)
par(mfrow = c(1, 1))


summary(m1_leg)
anova(m1_leg)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_leg, list(pairwise ~  Dimens))
# to add letters for post-hoc test:
multcomp::cld(emmeans::emmeans(m1_leg, list(pairwise ~  Dimens)),  
              type="response",
              Letters = letters, adjust = "none")


#4. gr.ef ----

# Data
# subsets of data for grass only
grass <- df_all %>% 
  filter(predictor=="gr.ef")

##  Dimens----
m1_gr<-lm(effect_size_st ~   Dimens, data=grass)

par(mfrow = c(2, 2))
plot(m1_gr)
par(mfrow = c(1, 1))


summary(m1_gr)
anova(m1_gr)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_gr, list(pairwise ~  Dimens))
# to add letters for post-hoc test:
multcomp::cld(emmeans::emmeans(m1_gr, list(pairwise ~  Dimens)),  
              type="response",
              Letters = letters, adjust = "none")



#5. sh.ef ----

# Data
# subsets of data for sh only
sh <- df_all %>% 
  filter(predictor=="sh.ef")

##  Dimens----
m1_SH<-lm(effect_size_st ~   Dimens, data=sh)

par(mfrow = c(2, 2))
plot(m1_SH)
par(mfrow = c(1, 1))

summary(m1_SH)
anova(m1_SH)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_SH, list(pairwise ~  Dimens))
# to add letters for post-hoc test:
multcomp::cld(emmeans::emmeans(m1_SH, list(pairwise ~  Dimens)),  
              type="response",
              Letters = letters, adjust = "none")

#6. th.ef ----

# Data
# subsets of data for th only
th <- df_all %>% 
  filter(predictor=="th.ef")


##  Dimens----
m1_TH<-lm(effect_size_st ~   Dimens, data=th)

par(mfrow = c(2, 2))
plot(m1_TH)
par(mfrow = c(1, 1))

summary(m1_TH)
anova(m1_TH)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_TH, list(pairwise ~  Dimens))
# to add letters for post-hoc test:
multcomp::cld(emmeans::emmeans(m1_TH, list(pairwise ~  Dimens)),  
              type="response",
              Letters = letters, adjust = "none")

