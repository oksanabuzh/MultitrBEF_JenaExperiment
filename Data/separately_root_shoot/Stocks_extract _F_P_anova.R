
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


model_TH_2<-lm(shoot.StandBiom ~  block +  log2(sowndiv) + leg.ef + gr.ef+ sh.ef+ th.ef , data=Index)
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


# Extract F and P
## extract values for flux models (41) and stocks models (12): 12 slopes
P_val <- as.data.frame(matrix(NA, 12,2))
names(P_val) <- c("flux", "block")

for (i in 1:12) {
  P_val$flux[i] <- all.vars(formula(paste("model_SpR_", i,  sep="")))[1] #extract response variable
  P_val$block[i] <- anova(get(paste("model_TH_", i,  sep="")))$"Pr(>F)"[1]  
  P_val$sowndiv[i] <- anova(get(paste("model_TH_", i,  sep="")))$"Pr(>F)"[2]
  P_val$leg.ef[i] <- anova(get(paste("model_TH_", i,  sep="")))$"Pr(>F)"[3]
  P_val$gr.ef[i] <- anova(get(paste("model_TH_", i,  sep="")))$"Pr(>F)"[4]
  P_val$sh.ef[i] <- anova(get(paste("model_TH_", i,  sep="")))$"Pr(>F)"[5]
  P_val$th.ef[i] <- anova(get(paste("model_TH_", i,  sep="")))$"Pr(>F)"[6]
  P_val$block1[i] <- anova(get(paste("model_NumFG_", i,  sep="")))$"Pr(>F)"[1]
  P_val$sowndiv1[i] <- anova(get(paste("model_NumFG_", i,  sep="")))$"Pr(>F)"[2]
  P_val$numfg[i] <- anova(get(paste("model_NumFG_", i,  sep="")))$"Pr(>F)"[3]
}


P_val


F_val <- as.data.frame(matrix(NA, 12,2))
names(F_val) <- c("flux", "block")
for (i in 1:12) {
  F_val$flux[i] <- all.vars(formula(paste("model_SpR_", i,  sep="")))[1] #extract response variable
  F_val$block[i] <- anova(get(paste("model_TH_", i,  sep="")))$"F value"[1]  
  F_val$sowndiv[i] <- anova(get(paste("model_TH_", i,  sep="")))$"F value"[2]
  F_val$leg.ef[i] <- anova(get(paste("model_TH_", i,  sep="")))$"F value"[3]
  F_val$gr.ef[i] <- anova(get(paste("model_TH_", i,  sep="")))$"F value"[4]
  F_val$sh.ef[i] <- anova(get(paste("model_TH_", i,  sep="")))$"F value"[5]
  F_val$th.ef[i] <- anova(get(paste("model_TH_", i,  sep="")))$"F value"[6]
  F_val$block2[i] <- anova(get(paste("model_NumFG_", i,  sep="")))$"F value"[1]
  F_val$sowndiv2[i] <- anova(get(paste("model_NumFG_", i,  sep="")))$"F value"[2]
  F_val$numfg[i] <- anova(get(paste("model_NumFG_", i,  sep="")))$"F value"[3]
}
F_val


write.csv(P_val,  file = "P(F)_values_stocks.csv")

write.csv(F_val,  file = "F_values_stocks.csv")

