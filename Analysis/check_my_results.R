# check the model results 
# run manually different models randomly and check the results with the ones derived by the function

library(tidyverse)
Index <- readr::read_csv("Data/net_ind_fluxes.csv")
str(Index)

Index <- Index %>% 
  mutate(sowndiv_log2=log2(sowndiv)) # added   log2(sowndiv) in case if we need


# Example 1 -----


# Model for the main text:----
# the target predictor is fitted first (but always after block)
# for this we use anova()  -- type I

m1<-lm(Plants.Stock ~  block +  log2(sowndiv) + leg.ef + gr.ef + sh.ef + th.ef, data=Index)

anova(m1)
summary(m1)


par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))

bc <- MASS::boxcox(m1, data = Index, plotit = FALSE)
lambda <- bc$x[which.max(bc$y)]
lambda
# [1] -0.1

#fit new linear regression model using the Box-Cox transformation
library(tidyverse)
Index <- Index %>% 
  mutate(Plants.Stock_bc = (Plants.Stock ^ lambda - 1) / lambda)

Index$Plants.Stock_bc

m2<-lm(Plants.Stock_bc ~  block +  log2(sowndiv) + leg.ef + gr.ef + sh.ef + th.ef, data=Index)
anova(m2)
summary(m2)

# Back transform of the effect size:----
# 1. predict from m1 for the max level of the target predictor variable (i.e., sowndiv) -> y_max
# 2. predict from m1 for the min level of the target predictor variable (i.e., sowndiv) -> y_min
# 3. back-transform the predicted values -> back_y_max  and  back_y_min
# 4. calculate the back-transformed size of the effect:
# back_tr_predicted_estimate = (back_y_max - back_y_min)/ (x_max - x_min)


# 1.
newdat1 <- expand.grid(sowndiv = max(Index$sowndiv), 
                       block = unique(factor(Index$block)), 
                       leg.ef=mean(Index$leg.ef),
                       gr.ef=mean(Index$gr.ef),
                       sh.ef=mean(Index$sh.ef),
                       th.ef=mean(Index$th.ef))
pred1 <- predict(m2, newdata = newdat1)
y_max <- mean(pred1)
y_max

# 2.
newdat2 <- expand.grid(sowndiv = min(Index$sowndiv), 
                       block = unique(factor(Index$block)), 
                       leg.ef=mean(Index$leg.ef),
                       gr.ef=mean(Index$gr.ef),
                       sh.ef=mean(Index$sh.ef),
                       th.ef=mean(Index$th.ef))
pred2 <- predict(m2, newdata = newdat2)
y_min <- mean(pred2)
y_min

# 3.
back_y_min <- (y_min * lambda + 1)^(1 / lambda)
back_y_max <- (y_max * lambda + 1)^(1 / lambda)

#4.
x_max <- max(Index$sowndiv)
x_min<- min(Index$sowndiv)
back_tr_predicted_estimate = (back_y_max - back_y_min)/ (x_max - x_min)
back_tr_predicted_estimate


# Standardize the back transformed effect size:----
# Standardization is implemented by multiplying the effect size by the ratio of 
# the standard deviation of x over the standard deviation in y.
x <- Index$sowndiv
y <- Index$Plants.Stock
st_estimate <- back_tr_predicted_estimate * (sd(x)/sd(y))
st_estimate


# Compare with the results derived by the function: ----

df_main <- read.csv("Results/mod_main_text.csv")


lambda
coef(m2)["log2(sowndiv)"] # estimate
anova(m2)["log2(sowndiv)", "Pr(>F)"] # p value
#R2 model
summary(m2)$r.squared 
#R2 partial
r2glmm::r2beta(m2, method = "nsj", partial = TRUE)%>%
filter(Effect == "sowndiv_log2") %>% pull(Rsq) %>% round(3)

back_tr_predicted_estimate #  back-transformed effect size

st_estimate # standard. effect size (back-transformed)



df_main %>% 
  filter(response=="Plants.Stock", predictor=="sowndiv") %>% 
  select(lambda, estimate, p, R2_model, r2_part, effect_size, effect_size_st)


# check the range of the standardized coefficients
df_main %>%
  summarise(min=min(effect_size_st), max=max(effect_size_st), mean=mean(effect_size_st))

# compare with the supplementary ----
# for this we use car::Anova()  -- type II
# the difference in the SI from the  analysis in the main text sould be  only in p value:

car::Anova(m2)["log2(sowndiv)", "Pr(>F)"] # p value

# result derived by the function
df_SI <- read.csv("Results/mod_supp.csv")
df_SI %>% 
  filter(response=="Plants.Stock", predictor=="sowndiv") %>% 
  select(p)



# all results match completely


##############-

# Example 2 -----



m3<-lm(Plants.Stock ~  block + numfg + log2(sowndiv), data=Index)


#fit new linear regression model using the Box-Cox transformation

bc_2 <- MASS::boxcox(m3, data = Index, plotit = FALSE)
lambda_2 <- bc$x[which.max(bc_2$y)]
lambda_2

library(tidyverse)
Index <- Index %>% 
  mutate(Plants.Stock_bc_2 = (Plants.Stock ^ lambda_2 - 1) / lambda_2)

Index$Plants.Stock_bc_2

m4<-lm(Plants.Stock_bc_2 ~  block + numfg + log2(sowndiv), data=Index)
anova(m4)
summary(m4)

# Back transform of the effect size:----
# 1. predict from m1 for the max level of the target predictor variable  -> y_max
# 2. predict from m1 for the min level of the target predictor variable  -> y_min
# 3. back-transform the predicted values -> back_y_max  and  back_y_min
# 4. calculate the back-transformed size of the effect:
# back_tr_predicted_estimate = (back_y_max - back_y_min)/ (x_max - x_min)


# 1.
newdat1 <- expand.grid(numfg = max(Index$numfg), 
                       block = unique(factor(Index$block)), 
                       sowndiv=mean(Index$sowndiv))
pred1 <- predict(m4, newdata = newdat1)
y_max <- mean(pred1)
y_max

# 2.
newdat2 <- expand.grid(numfg = min(Index$numfg), 
                       block = unique(factor(Index$block)), 
                       sowndiv=mean(Index$sowndiv))
pred2 <- predict(m4, newdata = newdat2)
y_min <- mean(pred2)
y_min

# 3.
back_y_min <- (y_min * lambda_2 + 1)^(1 / lambda_2)
back_y_max <- (y_max * lambda_2 + 1)^(1 / lambda_2)

#4.
x_max <- max(Index$numfg)
x_min<- min(Index$numfg)
back_tr_predicted_estimate = (back_y_max - back_y_min)/ (x_max - x_min)
back_tr_predicted_estimate


# Standardize the back transformed effect size:----
# Standardization is implemented by multiplying the effect size by the ratio of 
# the standard deviation of x over the standard deviation in y.
x <- Index$numfg
y <- Index$Plants.Stock
st_estimate <- back_tr_predicted_estimate * (sd(x)/sd(y))
st_estimate


# Compare with the results derived by the function: ----

df_main <- read.csv("Results/mod_main_text.csv")


lambda_2
coef(m4)["numfg"] # estimate
anova(m4)["numfg", "Pr(>F)"] # p value
#R2 model
summary(m4)$r.squared 
#R2 partial
r2glmm::r2beta(m4, method = "nsj", partial = TRUE)%>%
  filter(Effect == "numfg") %>% pull(Rsq) %>% round(3)

back_tr_predicted_estimate #  back-transformed effect size

st_estimate # standard. effect size (back-transformed)



df_main %>% 
  filter(response=="Plants.Stock", predictor=="numfg") %>% 
  select(lambda, estimate, p, R2_model, r2_part, effect_size, effect_size_st)


# compare with the supplementary ----
# for this we use car::Anova()  -- type II
# the difference in the SI from the  analysis in the main text sould be  only in p value:

car::Anova(m4)["numfg", "Pr(>F)"] # p value

# result derived by the function
df_SI <- read.csv("Results/mod_supp.csv")
df_SI %>% 
  filter(response=="Plants.Stock", predictor=="numfg") %>% 
  select(p)



# the results of the comparisons match the Example 1, except that the Estimate now is the slope for block 1, while in the Example 1 it is the intercept
# but what we need is that the Estimate should be the slope of our turget predictor



################


# AG.Herbivores_AG.Carnivores??????????????????????ß

# Example 3 -----


# Model for the main text:----
# the target predictor is fitted first (but always after block)
# for this we use anova()  -- type I

m1<-lm(AG.Herbivores_AG.Carnivores ~  block +  log2(sowndiv) + leg.ef + gr.ef + sh.ef + th.ef, data=Index)

anova(m1)
summary(m1)


par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))

bc <- MASS::boxcox(m1, data = Index, plotit = FALSE)
lambda <- bc$x[which.max(bc$y)]
lambda
# [1] 0.4

#fit new linear regression model using the Box-Cox transformation
library(tidyverse)
Index <- Index %>% 
  mutate(AG.Herbivores_AG.Carnivores_c = (AG.Herbivores_AG.Carnivores ^ lambda - 1) / lambda)

Index$AG.Herbivores_AG.Carnivores_c

m2<-lm(AG.Herbivores_AG.Carnivores_c ~  block +  log2(sowndiv) + leg.ef + gr.ef + sh.ef + th.ef, data=Index)
anova(m2)
summary(m2)

# Back transform of the effect size:----
# 1. predict from m1 for the max level of the target predictor variable (i.e., sowndiv) -> y_max
# 2. predict from m1 for the min level of the target predictor variable (i.e., sowndiv) -> y_min
# 3. back-transform the predicted values -> back_y_max  and  back_y_min
# 4. calculate the back-transformed size of the effect:
# back_tr_predicted_estimate = (back_y_max - back_y_min)/ (x_max - x_min)


# 1.
newdat1 <- expand.grid(sowndiv = max(Index$sowndiv), 
                       block = unique(factor(Index$block)), 
                       leg.ef=mean(Index$leg.ef),
                       gr.ef=mean(Index$gr.ef),
                       sh.ef=mean(Index$sh.ef),
                       th.ef=mean(Index$th.ef))
pred1 <- predict(m2, newdata = newdat1)
y_max <- mean(pred1)
y_max

# 2.
newdat2 <- expand.grid(sowndiv = min(Index$sowndiv), 
                       block = unique(factor(Index$block)), 
                       leg.ef=mean(Index$leg.ef),
                       gr.ef=mean(Index$gr.ef),
                       sh.ef=mean(Index$sh.ef),
                       th.ef=mean(Index$th.ef))
pred2 <- predict(m2, newdata = newdat2)
y_min <- mean(pred2)
y_min

# 3.
back_y_min <- (y_min * lambda + 1)^(1 / lambda)
back_y_max <- (y_max * lambda + 1)^(1 / lambda)

#4.
x_max <- max(Index$sowndiv)
x_min<- min(Index$sowndiv)
back_tr_predicted_estimate = (back_y_max - back_y_min)/ (x_max - x_min)
back_tr_predicted_estimate


# Standardize the back transformed effect size:----
# Standardization is implemented by multiplying the effect size by the ratio of 
# the standard deviation of x over the standard deviation in y.
x <- Index$sowndiv
y <- Index$AG.Herbivores_AG.Carnivores
st_estimate <- back_tr_predicted_estimate * (sd(x)/sd(y))
st_estimate


# Compare with the results derived by the function: ----

df_main <- read.csv("Results/mod_main_text.csv")


lambda
coef(m2)["log2(sowndiv)"] # estimate
anova(m2)["log2(sowndiv)", "Pr(>F)"] # p value
#R2 model
summary(m2)$r.squared 
#R2 partial
r2glmm::r2beta(m2, method = "nsj", partial = TRUE)%>%
  filter(Effect == "sowndiv_log2") %>% pull(Rsq) %>% round(3)

back_tr_predicted_estimate #  back-transformed effect size

st_estimate # standard. effect size (back-transformed)



df_main %>% 
  filter(response=="AG.Herbivores_AG.Carnivores", predictor=="sowndiv") %>% 
  select(lambda, estimate, p, R2_model, r2_part, effect_size, effect_size_st)


# check the range of the standardized coefficients
df_main %>%
  summarise(min=min(effect_size_st), max=max(effect_size_st), mean=mean(effect_size_st))

# compare with the supplementary ----
# for this we use car::Anova()  -- type II
# the difference in the SI from the  analysis in the main text sould be  only in p value:

car::Anova(m2)["log2(sowndiv)", "Pr(>F)"] # p value

# result derived by the function
df_SI <- read.csv("Results/mod_supp.csv")
df_SI %>% 
  filter(response=="AG.Herbivores_AG.Carnivores", predictor=="sowndiv") %>% 
  select(p)

m3<-lm(AG.Herbivores_AG.Carnivores_c ~  block + leg.ef + gr.ef + sh.ef + th.ef +  log2(sowndiv) , data=Index)
anova(m3)
car::Anova(m3)
summary(m3)

# all results match completely