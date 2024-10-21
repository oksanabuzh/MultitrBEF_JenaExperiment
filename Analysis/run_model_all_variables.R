#' Run linear models with multiple predictors
#'
#' @param dat a tibble or data frame containing the variables used in the model
#' @param first the name of the first predictor variable after block
#' @param y the name of the response variable in `dat`
#' @param type which type sum of squares should be used? If type = 1, anova is used,
#'    if type = 2, car::Anova function is used to calculate significance
#' @param all_vars a vector with all other predictor variables used in the model after
#'     the `first` variable
#' @param boxcox logical, should the response be transformed with boxcos?
#'
#' @return a tibble with the partial r2 for the first predictor, information on the
#'     formula used in the model and the respons variable
run_model_all_vars <- function(dat, first, y, type,
                               all_vars = c(
                                 "sowndiv", "numfg", "leg.ef",
                                 "gr.ef", "sh.ef", "th.ef"
                               ),
                               boxcox = TRUE) {
  first <- as.character(first)
  y <- as.character(y)
  # remove the first variable from the list of all_vars
  all_vars <- all_vars[!(all_vars == first)]

  # Remove numfg from predictors if the first is not numfg
  # numfg is correlated with the other predictors
  if (first %in% c("numfg", "RaoQ", "FDis")) {
    all_vars <- "log2(sowndiv)"
  } else if (first == "FDbranch") {
    # FD branch is fitted alone in the model (with block)
    all_vars <- c()
  } else if (first == "sowndiv") {
    first <- "log2(sowndiv)"
    all_vars <- all_vars[!(all_vars == "numfg")]
  } else {
    all_vars <- all_vars[!(all_vars == "numfg")]
    all_vars[all_vars == "sowndiv"] <- "log2(sowndiv)"
  }

  # step 1: create the model formula as text --------------------------------
    model_formula <- paste0(
      y, " ~ block + ", first, " + ",
      paste0(all_vars, collapse = " + ")
    )
  
  # Remove trailing + from model formula if there
  model_formula <- gsub("\\s\\+\\s$", "", model_formula)
  
  
  # step 2: Do boxcox transformation if you want (by default it's do --------
  if (boxcox) {
    bc <- MASS::boxcox(as.formula(model_formula), data = dat, plotit = FALSE)
    # find the maximum y and extract lamda (see here: https://www.statology.org/box-cox-transformation-in-r/)
    lambda <- bc$x[which.max(bc$y)]
    if (lambda == 0) {
      model_formula <- paste0(
        "log(", y, ") ~ block + ", first, " + ",
        paste0(all_vars, collapse = " + ")
      )
      # Remove trailing + if needed
      model_formula <- gsub("\\s\\+\\s$", "", model_formula)
    } else {
      model_formula <- paste0(
        "(", y, " ^ lambda -1)/lambda ~ block + ", first, " + ",
        paste0(all_vars, collapse = " + ")
      )
      # Remove trailing + if needed
      model_formula <- gsub("\\s\\+\\s$", "", model_formula)
    }
  } else {
    # lambda is NA to show that there was no boxcox transformation
    lambda <- NA
  }

  # step 3: Fit the model ---------------------------------------------------

  model <- lm(as.formula(model_formula), data = dat)

  # step 4: Get model coefficients and p-values -----------------------------
  # partial R2
  r2_part <- r2glmm::r2beta(model, method = "nsj", partial = TRUE)
  r2_part <- filter(r2_part, Effect == first)
  r2_part <- round(r2_part$Rsq, 3)

  # p-value with anova or Anova
  if (type == 1) {
    pval <- tibble::as_tibble(anova(model)) %>%
      mutate(variable = rownames(anova(model)))
  } else if (type == 2) {
    pval <- tibble::as_tibble(car::Anova(model)) %>%
      mutate(variable = rownames(car::Anova(model)))
  } else {
    stop(paste0("Error when calculating significance. Type argument must be
                1 for anova or 2 vor car::Anova, but value is ", type, " instead."))
  }

  # Extract F value
  Fval <- filter(pval, variable == first) %>%
    pull(`F value`)

  # Extract p value
  pval <- filter(pval, variable == first) %>%
    pull(`Pr(>F)`)



  # Extract model results ---------------------------------------------------

  # estimates
  estimates <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("x") %>%
    filter(x == first) %>%
    pull(Estimate)

  # effect size
  # if first is sowndiv, we need to remove the log2 to extract the correct coeff
  if (first == "log2(sowndiv)") {
    first <- "sowndiv"
  } else if ("log2(sowndiv)" %in% all_vars) {
    all_vars[all_vars == "log2(sowndiv)"] <- "sowndiv"
  }

  # find min and max x
  x_min <- select(dat, all_of(first)) %>%
    min()
  x_max <- select(dat, all_of(first)) %>%
    max()

  # calculate means of all numerical predictors and add blocks
  x_all_vars <- summarise(dat, across(
    .cols = all_of(all_vars),
    .fns = \(x) mean(x, na.rm = TRUE)
  )) %>%
    expand_grid(block = unique(dat$block))

  # predict y for xmin and xmax
  y_min <- predict(model, newdata = x_all_vars %>%
    mutate("{first}" := x_min)) %>%
    mean()
  y_max <- predict(model, newdata = x_all_vars %>%
    mutate("{first}" := x_max)) %>%
    mean()

  # back-transform y_min and y_max
  if (lambda == 0) {
    back_y_min <- exp(y_min)
    back_y_max <- exp(y_max)
  } else {
    back_y_min <- (y_min * lambda + 1)^(1 / lambda)
    back_y_max <- (y_max * lambda + 1)^(1 / lambda)
  }

  # calculate back-transformed effect sizes
  effect_size <- (back_y_max - back_y_min) / (x_max - x_min)

  # standardize effect size
  # calculate standard deviation of x and y from data
  sd_values <- summarise(dat, across(
    .cols = all_of(c(first, y)),
    .fns = \(x) sd(x, na.rm = TRUE)
  ))

  # standardize the effect_size
  effect_size_st <- effect_size * (sd_values %>% pull(first) / sd_values %>% pull(y))



  # Combine and return results ----------------------------------------------

  result <- list(
    response = y,
    predictor = first,
    r2_part = r2_part,
    R2_model = summary(model)$r.squared,
    p = pval,
    F_val = Fval,
    estimate = estimates,
    effect_size = effect_size,
    effect_size_st = effect_size_st,
    lambda = lambda,
    formula = model_formula
  )
  return(result)
}
