
# Extract Slopes separately for roots and shoots
#  the Jena ENA   stocks statistics
# # Buzhdygan
# April 29. 2020 
# ----------------------------------------------------

rm(list=ls(all=TRUE))
setwd("C:/Users/AG Petermann/LRZ Sync+Share/JenFlow (Sebastian Meyer)/JenFlow2/Effects_slopes/Stocks")
Index <- read.csv ("net_ind_fluxes.csv", header=T)
names(Index)


# Species Richness

model_SpR_1<-lm(root.StandBiom ~  block +  log2(sowndiv), data=Index)
model_SpR_1 <-update(model_SpR_1, sqrt(.) ~ . )
anova(model_SpR_1)
summary(model_SpR_1)

model_SpR_2<-lm(shoot.StandBiom ~  block +  log2(sowndiv), data=Index)
model_SpR_2 <-update(model_SpR_2, log(.) ~ . )
anova(model_SpR_2)
summary(model_SpR_2)


# Stocks (n=12) _ Legumes

model_Leg_1<-lm(root.StandBiom ~  block +  log2(sowndiv)+ leg.ef, data=Index)
model_Leg_1 <-update(model_Leg_1, sqrt(.) ~ . )
anova(model_Leg_1)
summary(model_Leg_1)

model_Leg_2<-lm(shoot.StandBiom ~  block +  log2(sowndiv)+ leg.ef, data=Index)
model_Leg_2 <-update(model_Leg_2, log(.) ~ . )
anova(model_Leg_2)
summary(model_Leg_2)

# Stocks (n=12) _ Grasses

model_Gr_1<-lm(root.StandBiom ~  block +  log2(sowndiv) + leg.ef + gr.ef, data=Index)
model_Gr_1 <-update(model_Gr_1, sqrt(.) ~ . )
anova(model_Gr_1)
summary(model_Gr_1)


model_Gr_2<-lm(shoot.StandBiom ~  block +  log2(sowndiv) + leg.ef + gr.ef, data=Index)
model_Gr_2 <-update(model_Gr_2, log(.) ~ . )
anova(model_Gr_2)
summary(model_Gr_2)


# Stocks (n=12) _ Short Herbs

model_SH_1<-lm(root.StandBiom ~  block +  log2(sowndiv) + leg.ef + gr.ef+ sh.ef, data=Index)
model_SH_1 <-update(model_SH_1, sqrt(.) ~ . )
anova(model_SH_1)
summary(model_SH_1)


model_SH_2<-lm(shoot.StandBiom ~  block +  log2(sowndiv) + leg.ef + gr.ef+ sh.ef, data=Index)
model_SH_2 <-update(model_SH_2, log(.) ~ . )
anova(model_SH_2)
summary(model_SH_2)


# Stocks (n=12) _ Tall Herbs

model_TH_1<-lm(root.StandBiom ~  block +  log2(sowndiv) + leg.ef + gr.ef+ sh.ef + th.ef, data=Index)
model_TH_1 <-update(model_TH_1, sqrt(.) ~ . )
anova(model_TH_1)
summary(model_TH_1)


model_TH_2<-lm(shoot.StandBiom ~  block +  log2(sowndiv) + leg.ef + gr.ef+ sh.ef + th.ef, data=Index)
model_TH_2 <-update(model_TH_2, log(.) ~ . )
anova(model_TH_2)
summary(model_TH_2)



# Stocks (n=12) _ Number of funct groups

model_NumFG_1<-lm(root.StandBiom ~  block +  log2(sowndiv) + numfg, data=Index)
model_NumFG_1 <-update(model_NumFG_1, sqrt(.) ~ . )
anova(model_NumFG_1)
summary(model_NumFG_1)


model_NumFG_2<-lm(shoot.StandBiom ~  block +  log2(sowndiv) + numfg, data=Index)
model_NumFG_2 <-update(model_NumFG_2, log(.) ~ . )
anova(model_NumFG_2)
summary(model_NumFG_2)


# Analysis of slopes
## extract values for flux models (41) and stocks models (12): 12 slopes
Slopes <- as.data.frame(matrix(NA, 2,2))
names(Slopes) <- c("flux", "sowndiv")
for (i in 1:2) {
  Slopes$flux[i] <- all.vars(formula(paste("model_SpR_", i,  sep="")))[1] #extract response variable
  Slopes$sowndiv[i] <- summary(get(paste("model_SpR_", i,  sep="")))$coefficients["log2(sowndiv)","Estimate"]
  Slopes$leg.ef[i] <- summary(get(paste("model_Leg_", i,  sep="")))$coefficients["leg.ef","Estimate"]
  Slopes$gr.ef[i] <- summary(get(paste("model_Gr_", i,  sep="")))$coefficients["gr.ef","Estimate"]
  Slopes$sh.ef[i] <- summary(get(paste("model_SH_", i,  sep="")))$coefficients["sh.ef","Estimate"]
  Slopes$th.ef[i] <- summary(get(paste("model_TH_", i,  sep="")))$coefficients["th.ef","Estimate"]
  Slopes$numfg[i] <- summary(get(paste("model_NumFG_", i,  sep="")))$coefficients["numfg","Estimate"]
}

Slopes

write.csv(Slopes,  file = "C:/Users/AG Petermann/LRZ Sync+Share/JenFlow (Sebastian Meyer)/JenFlow2/Effects_slopes/Stocks/separately for root&shoot for the AGvsBG test/Slopes_stocks.csv")


P_val <- as.data.frame(matrix(NA, 2,2))
names(P_val) <- c("flux", "sowndiv")
for (i in 1:2) {
  P_val$flux[i] <- all.vars(formula(paste("model_SpR_", i,  sep="")))[1] #extract response variable
  P_val$sowndiv[i] <- summary(get(paste("model_SpR_", i,  sep="")))$coefficients["log2(sowndiv)","Pr(>|t|)"]
  P_val$leg.ef[i] <- summary(get(paste("model_Leg_", i,  sep="")))$coefficients["leg.ef","Pr(>|t|)"]
  P_val$gr.ef[i] <- summary(get(paste("model_Gr_", i,  sep="")))$coefficients["gr.ef","Pr(>|t|)"]
  P_val$sh.ef[i] <- summary(get(paste("model_SH_", i,  sep="")))$coefficients["sh.ef","Pr(>|t|)"]
  P_val$th.ef[i] <- summary(get(paste("model_TH_", i,  sep="")))$coefficients["th.ef","Pr(>|t|)"]
  P_val$numfg[i] <- summary(get(paste("model_NumFG_", i,  sep="")))$coefficients["numfg","Pr(>|t|)"]
}

P_val

write.csv(P_val,  file = "C:/Users/AG Petermann/LRZ Sync+Share/JenFlow (Sebastian Meyer)/JenFlow2/Effects_slopes/Stocks/separately for root&shoot for the AGvsBG test/P(slopes)_stocks.csv")
