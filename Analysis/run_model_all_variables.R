#' Run linear models with multiple predictors 
#'
#' @param dat a tibble or data frame containing the variables used in the model
#' @param y the name of the response variable in `dat`
#' @param first the name of the rist predictor variable after block
#' @param all_vars a vector with all other predictor variables used in the model after
#'     the `first` variable
#' @param boxcox logical, should the response be transformed with boxcos?
#'
#' @return a tibble with the partial r2 for the first predictor, information on the
#'     formula used in the model and the respons variable
run_model_all_vars <- function(dat, y, first,
                               all_vars = c(
                                 "sowndiv", "numfg", "leg.ef",
                                 "gr.ef", "sh.ef", "th.ef"
                               ),
                               boxcox = TRUE) {
  # remove the first variable from the list of all_vars
  all_vars <- all_vars[!(all_vars == first)]

  # Turn sowndiv into log2
  if (first == "sowndiv") {
    first <- "log2(sowndiv)"
  } else {
    all_vars[all_vars == "sowndiv"] <- "log2(sowndiv)"
  }

  # Remove numfg from predictors if the first is not numfg
  # numfg is correlated with the other predictors
  if (first == "numfg") {
    all_vars <- "log2(sowndiv)"
  } else {
    all_vars <- all_vars[!(all_vars == "numfg")]
  }

  # step 1: create the model formula as text
  model_formula <- paste0(
    y, " ~ block + ", first, " + ",
    paste0(all_vars, collapse = " + ")
  )

  # step 2: Do boxcox transformation if you want (by default it's done)
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

  # step 2: Fit the model
  model <- lm(as.formula(model_formula), data = dat)
  
  # Step 3: Get partial R2
  r2 <- tibble::as_tibble(rsq::rsq.partial(model))
  
  # put result in a list
  result <- tibble(
    y = y, 
    rsq_part = r2 %>% filter(variable == first) %>% pull(partial.rsq) %>% round(4),
    predictor = first,
    formula = model_formula
  )
  
  # Return the model formula and the partial r2 for the variable of interest
  return(result)
}
