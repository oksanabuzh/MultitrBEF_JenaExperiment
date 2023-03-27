# The Jena Experiment II
# Stocks Metaanalysis
# Comparison of slopes of the effect of plant diversity biomass stocks (n=12)
# between trophic groups ("Plants", "Detritus", "Herbivores", "Decomposers","Omnivores", "Carnivores"), 
#         trophic levels (1st, 2nd, 3rd), and between 
#         aboveground and belowground subnetworks (AG vs BG: Aboveground, Belowground). 
# An analysis of variance with sequential sum of squares (type I) was applied.
# Buzhdygan, Meyer
# April 29, 2020
# ----------------------------------------------------

rm(list=ls(all=TRUE))

setwd("C:/Users/AG Petermann/LRZ Sync+Share/JenFlow (Sebastian Meyer)/JenFlow2/Effects_slopes/Stocks")

library(rcompanion)


Slopes <- read.csv ( "Slopes_stocks.csv", header=T)[,2:8]
Index <- read.csv ("stock_groups.csv", header=T)
names(Index)
Index3 <- merge(Index, Slopes)
Index3
names(Index3)

Index3$Trophic_level<-factor(Index3$Trophic_level) # Trophic level as factor


# Dataset for AG vs BG (model4): roots and shoots separately
setwd("C:/Users/Oksana/Nextcloud/Jena2/JenFlow (Sebastian Meyer)/JenFlow2/Effects_slopes/Stocks")

Slopes1 <- read.csv ( "separately_root_shoot/Slopes_stocks.csv", header=T)
Index1 <- read.csv ("separately_root_shoot/stock_groups.csv", header=T)
names(Index1)
Index2 <- merge(Index1, Slopes1)
Index2
names(Index2)


par(mfrow=c(2,2))
# Comparison of slopes of the diversity effect on stocks 

#.# Species richness effect

hist(Index3$sowndiv)

model1_SR<-lm(sowndiv ~  Tr_Group, data=Index3)
plot(model1_SR)
anova(model1_SR)
summary(model1_SR)

model2_SR<-lm(sowndiv ~  Trophic_level, data=Index3)
plot(model2_SR)
anova(model2_SR)
summary(model2_SR)

model3_SR<-lm(sowndiv ~  AG_BG, data=Index3[-which(Index3$compartments=="Plants"),]) # Plant compartment was removed from the dataset
plot(model3_SR)
anova(model3_SR)
summary(model3_SR)

hist(Index2$sowndiv)
min(Index2$sowndiv)
hist(log(Index2$sowndiv+0.08))

model4_SR<-lm(log(sowndiv+0.08) ~ AG_BG, data=Index2)  #  Plant stock is separated among roots and shoots
plot(model4_SR)
anova(model4_SR)
summary(model4_SR)


#.# Number functional groups effect
hist(Index3$numfg)
min(Index3$numfg)

model1_nFG<-lm(numfg ~ Tr_Group , data=Index3)
plot(model1_nFG)
anova(model1_nFG)
summary(model1_nFG)


model2_nFG<-lm(numfg ~  Trophic_level, data=Index3)
anova(model2_nFG)
summary(model2_nFG)


model3_nFG<-lm(numfg ~  AG_BG , data=Index3[-which(Index3$compartments=="Plants"),])# Plant compartment was removed from the dataset
anova(model3_nFG)
summary(model3_nFG)


model4_nFG<-lm(numfg ~ AG_BG, data=Index2)# Plant stock is separated among roots and shoots
anova(model4_nFG)
summary(model4_nFG)

#.# Legume presence effect

hist(Index3$leg.ef)
hist(log(Index3$leg.ef+0.27))

model1_leg.ef<-lm(log(leg.ef+0.27) ~ Tr_Group , data=Index3)
plot(model1_leg.ef)
anova(model1_leg.ef)
summary(model1_leg.ef)


model2_leg.ef<-lm(log(leg.ef+0.27) ~   Trophic_level   , data=Index3)
anova(model2_leg.ef)
summary(model2_leg.ef)


model3_leg.ef<-lm(log(leg.ef+0.27) ~  AG_BG , data=Index3[-which(Index3$compartments=="Plants"),])
anova(model3_leg.ef)
summary(model3_leg.ef)

hist(Index2$leg.ef)

model4_leg.ef<-lm((leg.ef) ~  AG_BG, data=Index2)
anova(model4_leg.ef)
summary(model4_leg.ef)


#.# Grass presence effect
hist(Index3$gr.ef)

model1_gr.ef<-lm(gr.ef ~ Tr_Group , data=Index3)
anova(model1_gr.ef)
summary(model1_gr.ef)


model2_gr.ef<-lm(gr.ef ~ Trophic_level  , data=Index3)
anova(model2_gr.ef)
summary(model2_gr.ef)


model3_gr.ef<-lm(gr.ef ~  AG_BG , data=Index3[-which(Index3$compartments=="Plants"),])
anova(model3_gr.ef)
summary(model3_gr.ef)

hist(Index2$gr.ef)
min(Index2$gr.ef)
hist(log(Index2$gr.ef+0.87))

model4_gr.ef<-lm(log(gr.ef+0.87) ~  AG_BG, data=Index2)
plot(model4_gr.ef)
anova(model4_gr.ef)
summary(model4_gr.ef)



#.# Small herb presence effect
hist(Index3$sh.ef)

model1_sh.ef<-lm(sh.ef ~ Tr_Group , data=Index3)
anova(model1_sh.ef)
summary(model1_sh.ef)


model2_sh.ef<-lm(sh.ef ~ Trophic_level  , data=Index3)
anova(model2_sh.ef)
summary(model2_sh.ef)


model3_sh.ef<-lm(sh.ef ~  AG_BG , data=Index3[-which(Index3$compartments=="Plants"),])
anova(model3_sh.ef)
summary(model3_sh.ef)

hist(Index2$sh.ef)
model4_sh.ef<-lm((sh.ef) ~   AG_BG , data=Index2)
plot(model4_sh.ef)
anova(model4_sh.ef)
summary(model4_sh.ef)


#.# Tall herb presence effect
hist(Index3$th.ef)
model1_th.ef<-lm(th.ef ~ Tr_Group , data=Index3)
anova(model1_th.ef)
summary(model1_th.ef)


model2_th.ef<-lm(th.ef ~ Trophic_level , data=Index3)
anova(model2_th.ef)
summary(model2_th.ef)


model3_th.ef<-lm(th.ef ~  AG_BG , data=Index3[-which(Index3$compartments=="Plants"),])
anova(model3_th.ef)
summary(model3_th.ef)

hist(Index2$th.ef)
model4_th.ef<-lm(th.ef ~  AG_BG , data=Index2)
anova(model4_th.ef)
summary(model4_th.ef)
