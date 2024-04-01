# Diagnostics for roots and shoots stocks
#  the Jena ENA   stocks statistics
# # Buzhdygan
# April 29. 2020
# ----------------------------------------------------

rm(list=ls(all=TRUE))
setwd("C:/Users/AG Petermann/LRZ Sync+Share/JenFlow (Sebastian Meyer)/JenFlow2/Effects_slopes/Stocks")
Index <- read.csv ("net_ind_fluxes.csv", header=T)
names(Index)

model_SpR_1<-lm(root.StandBiom ~  block +  log2(sowndiv), data=Index)
par(mfrow=c(2,2)); plot(model_SpR_1)
model_SpR_1a <-update(model_SpR_1, sqrt(.) ~ . )
par(mfrow=c(2,2)); plot(model_SpR_1a)
model_SpR_1b <-update(model_SpR_1, log(.) ~ . )
par(mfrow=c(2,2)); plot(model_SpR_1b)
transformTukey(Index$root.StandBiom)
model_SpR_1c <-update(model_SpR_1, .^0.25 ~ . )
par(mfrow=c(2,2)); plot(model_SpR_1c)

anova(model_SpR_1a)
summary(model_SpR_1a)



model_SpR_2<-lm(shoot.StandBiom ~  block +  log2(sowndiv), data=Index)
par(mfrow=c(2,2)); plot(model_SpR_2)
model_SpR_2a <-update(model_SpR_2, sqrt(.) ~ . )
par(mfrow=c(2,2)); plot(model_SpR_2a)
model_SpR_2b <-update(model_SpR_2, log(.) ~ . )
par(mfrow=c(2,2)); plot(model_SpR_2b)
transformTukey(Index$shoot.StandBiom)
model_SpR_2c <-update(model_SpR_2, -1*.^-0.1 ~ . )
par(mfrow=c(2,2)); plot(model_SpR_2c)

anova(model_SpR_2b)
summary(model_SpR_2b)