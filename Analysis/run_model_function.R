# Buzhdygan et al.   "Facets of plant diversity  differentially affect energy dynamics in grasslands depending on trophic contexts"

# in "Ecology"

# correspondence to oksana.buzh@fu-berlin.de


# Function to run linear models of the generic type y ~ block + x
# Arguments:
# dat: the name of the data frame frome which to build the model
# x: a character vector of all predictors.
# y: the name of the response variable
# boxcox: TRUE/FALSE depending on whether to do a boxcox transformation or not




run_model <- function(dat, x, y, boxcox = TRUE) {
  
  # if the predictors contain "sowndiv", turn it into "log2(sowndiv)"
  x <- stringr::str_replace(x, "sowndiv", "log2(sowndiv)")
  
  # step 1: create the model formula as text
  model_formula <- paste0(y, " ~ block + ", paste0(x, collapse = "+"))
  
  # step 2: Do boxcox transformation if you want (by default it's done)
  if (boxcox) {
    bc <- MASS::boxcox(as.formula(model_formula), data = dat ,  plotit = FALSE)
    # find the maximum y and extract lamda (see here: https://www.statology.org/box-cox-transformation-in-r/)
    lambda <- bc$x[which.max(bc$y)]
    if (lambda == 0) {
      model_formula <- paste0("log(", y, ") ~ block + ", paste0(x, collapse = "+"))
    } else {
      model_formula <- paste0("(", y, " ^ lambda -1)/lambda ~ block + ", paste0(x, collapse = "+"))
    }
  } else{
    # lambda is NA to show that there was no boxcox transformation
    lambda <- NA
  }
  
  # step 2: Fit the model
  model <- lm(as.formula(model_formula), data = dat)
  
  # define the estimate (need for back-transformation, step 3)
  estimate <- summary(model)$coefficients[x, "Estimate"]
  
  
  # we need to back transform not slopes but the predictions
 #  y_min = predict(min) # predict() for minimum value of the focal variable (e.g., 0 for  legume presence; 1 for SR)
 #  y_max = predict(max) # predict() for maximum value of the focal variable (e.g., 1 for legume presence; 60 for SR)
       # back-transform y_min and 
 # back_y_min
 # back_y_max
 
   # step 3: back-transform the estimate
  if (lambda == 0) {
    BackTrans <- exp(estimate)
  } else {
    BackTrans <- (estimate * lambda + 1) ^ (1/lambda)
  }
  
 
  # calculate back transformed effect sizes (to be used in all next figures)
  back_predicted_estimate = (back_y_max - back_y_min)/ (x_max - y_max)

  # add partial R2 calculation for each response variable  in the model
  
    # step 4: Extract the results

  results <- list(
    y = all.vars(formula(model_formula))[1],
    formula = model_formula,
    estimate = estimate,
    BackTrans = BackTrans,
    p = car::Anova(model)[x, "Pr(>F)"],
    R2 = summary(model)$r.squared,
    lambda = lambda
    # back_predicted_estimate
   # R2_partial 
    )
  # unlist the list with the results
  # results <- unlist(results)
  # return(results)
 
  return(unlist(results)) 
}

