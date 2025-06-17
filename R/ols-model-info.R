#' Model formula
#'
#' Returns the model formula.
#'
#' @param model An object of class \code{lm}.
#'
#' @return An object of class \code{formula}.
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_get_formula(model)
#'
#' @family model info
#'
#' @export
#'
ols_get_formula <- function(model) {
  formula(model)
}

#' Interaction terms
#'
#' Returns interaction terms present in the model.
#'
#' @param model An object of class \code{lm}.
#'
#' @return \code{Character} vector or \code{NULL}.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)
#' ols_get_interaction_terms(model)
#'
#' @export
#'
ols_get_interaction_terms <- function(model) {
  terms <- model$terms
  i <- attr(terms, 'order')
  if (any(i > 1)) {
    iterms <- i > 1
    return(attr(terms, 'term.labels')[iterms])
  } else {
    return(NULL)
  }
}

#' Model variables
#' 
#' Return model variables.
#'
#' @param model An object of class \code{lm}.
#'
#' @return List with response and predictors.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)
#' ols_get_variables(model)
#'
#' @export
#'
ols_get_variables <- function(model) {
  vars  <- names(model$model)
  n     <- length(vars)
  resp  <- vars[1]
  if (n == 1) {
    preds <- NULL
  } else {
    preds <- vars[2:n] 
  }
  list(response = resp, predictors = preds)
}

#' Model data
#' 
#' Returns data used in model.
#' 
#' @param model An object of class \code{lm}.
#'
#' @return An object of class \code{data.frame}.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)
#' ols_get_data(model)
#'
#' @export
#'
ols_get_data <- function(model) {
  model$model
}


#' Degrees of freedom
#' 
#' Returns residual degrees of freedom.
#' 
#' @param model An object of class \code{lm}.
#'
#' @return Residual degrees of freedom.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl * hp * disp + gear * drat, data = mtcars)
#' ols_get_df(model)
#'
#' @export
#'
ols_get_df <- function(model) {
  model$df.residual
}
