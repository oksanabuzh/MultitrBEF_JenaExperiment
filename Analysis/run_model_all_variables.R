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
  # to debug
  # dat <- Index
  # y <- "o_Plants"
  # first <- "leg.ef"
  # type <- 1
  # boxcox <- TRUE
  # all_vars = c(
  #   "sowndiv", "numfg", "leg.ef",
  #   "gr.ef", "sh.ef", "th.ef"
  # )


  # remove the first variable from the list of all_vars
  all_vars <- all_vars[!(all_vars == first)]

  # Remove numfg from predictors if the first is not numfg
  # numfg is correlated with the other predictors
  if (first == "numfg") {
    all_vars <- "sowndiv"
  } else {
    all_vars <- all_vars[!(all_vars == "numfg")]
  }

  # step 1: create the model formula as text --------------------------------
  model_formula <- paste0(
    y, " ~ block + ", first, " + ",
    paste0(all_vars, collapse = " + ")
  )


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
    } else {
      model_formula <- paste0(
        "(", y, " ^ lambda -1)/lambda ~ block + ", first, " + ",
        paste0(all_vars, collapse = " + ")
      )
    }
  } else {
    # lambda is NA to show that there was no boxcox transformation
    lambda <- NA
  }

  # step 3: Fit the model ---------------------------------------------------

  model <- lm(as.formula(model_formula), data = dat)


  # step 4: Get model coefficients and p-values -----------------------------

  # partial R2
  r2 <- tibble::as_tibble(r2glmm::r2beta(model, method = "nsj", partial = TRUE))

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

  # effect size

  # find min and max x
  x_min <- dat %>%
    select(all_of(first)) %>%
    min()
  x_max <- dat %>%
    select(all_of(first)) %>%
    max()

  # calculate means of all numerical predictors and add blocks
  x_all_vars <- dat %>%
    summarise(across(
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
  sd_values <- dat %>% summarise(across(
    .cols = all_of(c(first, y)),
    .fns = \(x) sd(x, na.rm = TRUE)
  ))

  # standardize the effect_size
  effect_size_st <- effect_size * (sd_values %>% pull(first) / sd_values %>% pull(y))

  # estimates
  estimates <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("x")


  # Combine and return results ----------------------------------------------

  result <- tibble(
    response = y,
    predictor = first,
    r2_part = r2 %>% filter(Effect == first) %>% pull(Rsq) %>% round(3),
    R2_model = summary(model)$r.squared,
    p = pval %>% filter(variable == first) %>% pull(`Pr(>F)`),
    estimate = estimates %>% filter(x == first) %>% pull(Estimate),
    effect_size = effect_size,
    effect_size_st = effect_size_st,
    lambda = lambda,
    formula = model_formula
  )
  return(result)
}