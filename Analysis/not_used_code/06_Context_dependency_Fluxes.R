# Do the plant diversity effects on fluxes depend on  trophic contexts?

# Comparison of the effect of plant diversity on energy fluxes (n=41) between 
#         - ecosystem functions (Plant respiration, Herbivory, Decomposition, Predation, Detritus production, and Respiration), 
#         - aboveground and belowground subnetworks (AG vs BG: Aboveground, Belowground).

# An analysis of variance with sequential sum of squares was applied.
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
  left_join(group, by = join_by(response)) %>% 
  filter(Dimens=="flow") 

str(df_all)

# Predictors
unique(df_all$Ecos_Function)
unique(df_all$AG_BG)


# Metaanalyses----

#1. sowndiv ----

# Data
# subsets of data for sowndiv only
sowndiv <- df_all %>% 
  filter(predictor=="sowndiv")

sowndiv_AG_BG <- sowndiv %>%
  filter(!AG_BG=="AG_BG")

# Predictors
sowndiv$Ecos_Function
factor(sowndiv_AG_BG$AG_BG)

## Ecos_Function----
m1_SR<-lm(effect_size_st ~  Ecos_Function, data=sowndiv)

par(mfrow = c(2, 2))
plot(m1_SR)
par(mfrow = c(1, 1))

min(sowndiv$effect_size_st)

m1_SR<-lm(log(effect_size_st+0.2) ~  Ecos_Function, data=sowndiv)

par(mfrow = c(2, 2))
plot(m1_SR)
par(mfrow = c(1, 1))

# Box-Cox transformation
m1_SR<-lm(effect_size_st+0.2 ~  Ecos_Function, data=sowndiv)

bc <- MASS::boxcox(m1_SR, data = sowndiv, plotit = FALSE)
lambda <- bc$x[which.max(bc$y)]
lambda


#fit new linear regression model using the Box-Cox transformation
library(tidyverse)

sowndiv$effect_s_tr <- ((sowndiv$effect_size_st+0.2)^lambda - 1 )/ lambda

sowndiv$effect_size_st
sowndiv$effect_s_tr


m1_SR<-lm(effect_s_tr ~  Ecos_Function , data=sowndiv)

par(mfrow = c(2, 2))
plot(m1_SR)
par(mfrow = c(1, 1))

summary(m1_SR)
anova(m1_SR)

# Marginal means and pairwise differences 

emmeans::emmeans(m1_SR, list(pairwise ~ Ecos_Function))
# to add letters for post-hoc test:
model_means_EF <- multcomp::cld(emmeans::emmeans(m1_SR, list(pairwise ~ Ecos_Function)),  
                                Letters = letters, adjust = "Tukey")
model_means_EF



 ggplot(data = model_means_EF) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Ecos_Function), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Ecos_Function),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Ecos_Function, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 



## AG_BG ----
sowndiv_AG_BG$AG_BG

m3_SR<-lm(effect_size_st ~  AG_BG, data=sowndiv_AG_BG)

par(mfrow = c(2, 2))
plot(m3_SR)
par(mfrow = c(1, 1))

min(sowndiv$effect_size_st)

m3_SR<-lm((effect_size_st) ~  AG_BG, data=sowndiv_AG_BG)

par(mfrow = c(2, 2))
plot(m3_SR)
par(mfrow = c(1, 1))

summary(m3_SR)
anova(m3_SR)


# Marginal means and pairwise differences 

emmeans::emmeans(m3_SR, list(pairwise ~ AG_BG))
# to add letters for post-hoc test:
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

numfg_AG_BG <- numfg %>%
  filter(!AG_BG=="AG_BG")

# Predictors
numfg$Ecos_Function
factor(numfg_AG_BG$AG_BG)

## Ecos_Function----
m1_FG<-lm(effect_size_st ~  Ecos_Function, data=numfg)

par(mfrow = c(2, 2))
plot(m1_FG)
par(mfrow = c(1, 1))


summary(m1_FG)
anova(m1_FG)


# Marginal means and pairwise differences 

emmeans::emmeans(m1_FG, list(pairwise ~ Ecos_Function))
# to add letters for post-hoc test:
model_means_EF <- multcomp::cld(emmeans::emmeans(m1_FG, list(pairwise ~ Ecos_Function)),  
                                Letters = letters, adjust = "Tukey")
model_means_EF



ggplot(data = model_means_EF) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Ecos_Function), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Ecos_Function),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Ecos_Function, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 

## AG_BG ----
numfg_AG_BG$AG_BG

m3_FG<-lm(effect_size_st ~  AG_BG, data=numfg_AG_BG)

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

leg_AG_BG <- leg %>%
  filter(!AG_BG=="AG_BG")

# Predictors
leg$Ecos_Function
factor(leg_AG_BG$AG_BG)

## Ecos_Function----
m1_leg<-lm(effect_size_st ~  Ecos_Function, data=leg)

par(mfrow = c(2, 2))
plot(m1_leg)
par(mfrow = c(1, 1))


summary(m1_leg)
anova(m1_leg)


# Marginal means and pairwise differences 

m_means <- emmeans::emmeans(m1_leg, list(pairwise ~ Ecos_Function),   adjust = "none")
# to add letters for post-hoc test:
model_means_EF <- multcomp::cld(emmeans::emmeans(m1_leg, list(pairwise ~ Ecos_Function)),  
                                Letters = letters, adjust = "Tukey")
model_means_EF

plot(m_means, comparisons = TRUE)
# lower.CL and upper.CL in  emmeans are are not the confidence intervals for the means 
# they are the confidence intervals for the pairwise differences of means
# https://stackoverflow.com/questions/61779348/how-does-emmeans-calculate-confidence-intervals-used-to-compare-means



ggplot(data = model_means_EF) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Ecos_Function), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Ecos_Function),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Ecos_Function, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 


## AG_BG ----
leg_AG_BG$AG_BG

m3_leg<-lm(effect_size_st ~  AG_BG, data=leg_AG_BG)

par(mfrow = c(2, 2))
plot(m3_leg)
par(mfrow = c(1, 1))

m3_leg<-lm(log(effect_size_st+0.3) ~  AG_BG, data=leg_AG_BG)

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

grass_AG_BG <- grass %>%
  filter(!AG_BG=="AG_BG")

# Predictors
grass$Ecos_Function
factor(grass_AG_BG$AG_BG)

## Ecos_Function----
m1_gr<-lm(effect_size_st ~  Ecos_Function, data=grass)

par(mfrow = c(2, 2))
plot(m1_gr)
par(mfrow = c(1, 1))

min(grass$effect_size_st)

m1_gr<-lm(log(effect_size_st+0.4) ~  Ecos_Function, data=grass)

par(mfrow = c(2, 2))
plot(m1_gr)
par(mfrow = c(1, 1))

# Box-Cox transformation

m1_gr<-lm(effect_size_st+0.4 ~  Ecos_Function, data=grass)

bc <- MASS::boxcox(m1_gr, data = grass, plotit = FALSE)
lambda <- bc$x[which.max(bc$y)]
lambda


#fit new linear regression model using the Box-Cox transformation

grass$effect_s_tr <- ((grass$effect_size_st+0.4)^lambda - 1 )/ lambda
         
grass$effect_size_st
grass$effect_s_tr


m1_gr<-lm(effect_s_tr ~  Ecos_Function, data=grass)

par(mfrow = c(2, 2))
plot(m1_gr)
par(mfrow = c(1, 1))

summary(m1_gr)
anova(m1_gr)


# Marginal means and pairwise differences 

emmeans::emmeans(m1_gr, list(pairwise ~ Ecos_Function))
# to add letters for post-hoc test:
model_means_EF <- multcomp::cld(emmeans::emmeans(m1_gr, list(pairwise ~ Ecos_Function)),  
                                Letters = letters, adjust = "Tukey")
model_means_EF



ggplot(data = model_means_EF) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Ecos_Function), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Ecos_Function),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Ecos_Function, label = str_trim(.group)),
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

sh_AG_BG <- sh %>%
  filter(!AG_BG=="AG_BG")

# Predictors
sh$Ecos_Function
factor(sh_AG_BG$AG_BG)

## Ecos_Function----
m1_SH<-lm(effect_size_st ~  Ecos_Function, data=sh)

par(mfrow = c(2, 2))
plot(m1_SH)
par(mfrow = c(1, 1))

min(sh$effect_size_st)

m1_SH<-lm(log(effect_size_st+0.2) ~  Ecos_Function, data=sh)

par(mfrow = c(2, 2))
plot(m1_SH)
par(mfrow = c(1, 1))

summary(m1_SH)
anova(m1_SH)


# Marginal means and pairwise differences 

emmeans::emmeans(m1_SH, list(pairwise ~ Ecos_Function))
# to add letters for post-hoc test:
model_means_EF <- multcomp::cld(emmeans::emmeans(m1_SH, list(pairwise ~ Ecos_Function)),  
                                Letters = letters, adjust = "Tukey")
model_means_EF



ggplot(data = model_means_EF) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Ecos_Function), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Ecos_Function),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Ecos_Function, label = str_trim(.group)),
            position = position_nudge(x = 0.1), hjust = 0,color = "black") 

## AG_BG ----
sh_AG_BG$AG_BG

m3_SH<-lm(effect_size_st ~  AG_BG, data=sh_AG_BG)

par(mfrow = c(2, 2))
plot(m3_SH)
par(mfrow = c(1, 1))

min(sh$effect_size_st)

m3_SH<-lm(log(effect_size_st+0.2) ~  AG_BG, data=sh_AG_BG)

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

th_AG_BG <- th %>%
  filter(!AG_BG=="AG_BG")

# Predictors
th$Ecos_Function
 
factor(th_AG_BG$AG_BG)

## Ecos_Function----
m1_TH<-lm(effect_size_st ~  Ecos_Function, data=th)

par(mfrow = c(2, 2))
plot(m1_TH)
par(mfrow = c(1, 1))

summary(m1_TH)
anova(m1_TH)


# Marginal means and pairwise differences 

emmeans::emmeans(m1_TH, list(pairwise ~ Ecos_Function))
# to add letters for post-hoc test:
model_means_EF <- multcomp::cld(emmeans::emmeans(m1_TH, list(pairwise ~ Ecos_Function)),  
                                Letters = letters, adjust = "Tukey")
model_means_EF



ggplot(data = model_means_EF) +
  theme_bw()+
  geom_point(aes(y = emmean, x = Ecos_Function), size = 2, color = "black") +
  geom_errorbar( aes(ymin = lower.CL, ymax = upper.CL, x = Ecos_Function),
                 width = 0.05, color = "black") +
  geom_text(aes(y = emmean, x = Ecos_Function, label = str_trim(.group)),
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






results <-rbind(anova(m1_SR)[1,],anova(m3_SR)[1,], 
         anova(m1_FG)[1,], anova(m3_FG)[1,],
         anova(m1_leg)[1,],anova(m3_leg)[1,],
         anova(m1_gr)[1,],anova(m3_gr)[1,],
         anova(m1_SH)[1,],anova(m3_SH)[1,],
         anova(m1_TH)[1,],anova(m3_TH)[1,]) 


row.names(results)= c("Ecosystem function for SR", "AG_BG for SR", 
                "Ecosystem function for FG", "AG_BG for FG",
                "Ecosystem function for leg", "AG_BG for leg",
                "Ecosystem function for grass", "AG_BG for grass",
                "Ecosystem function for SH", "AG_BG for SH",
                "Ecosystem function for TH", "AG_BG for TH")

results


write.csv(results, "Results/Tables_1_2.csv", row.names=TRUE)
