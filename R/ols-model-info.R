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


#' Model intercept
#' 
#' Returns the value of the model intercept. 
#' 
#' @param model An object of class \code{lm}.
#'
#' @return The value of the intercept. 
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_intercept(model)
#'
#' model <- lm(mpg ~ 0 + wt + cyl + hp + disp, data = mtcars)
#' ols_get_intercept(model)
#'
#' @export
#'
ols_get_intercept <- function(model) {
  if (ols_has_intercept(model)) {
    return(model$coefficients[[1]])
  }
  return(NULL)
}

#' Model matrix
#' 
#' Returns the design matrix. 
#' 
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_model_matrix(model)
#'
#' @export
#'
ols_get_model_matrix <- function(model) {
  model.matrix(model)
}

#' Fitted values
#' 
#' Returns the fitted values. 
#' 
#' @param model An object of class \code{lm}.
#'
#' @return Fitted values. 
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_predicted(model)
#'
#' @export
#'
ols_get_predicted <- function(model) {
  as.numeric(model$fitted.values)
}

#' Model residuals 
#' 
#' Returns the residuals from the model. 
#' 
#' @param model An object of class \code{lm}.
#'
#' @return Residuals from the regression model. 
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_residuals(model)
#'
#' @export
#'
ols_get_residuals <- function(model) {
  as.numeric(model$residuals)
}

#' Sigma 
#' 
#' Returns the residual standard deviation.
#' 
#' @param model An object of class \code{lm}.
#'
#' @return Residual standard deviation.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_sigma(model)
#'
#' @export
#'
ols_get_sigma <- function(model) {
  summary(model)$sigma
}

#' Variance covariance matrix
#' 
#' Returns the variance covariance matrix from the model. 
#' 
#' @param model An object of class \code{lm}.
#'
#' @return Variance covariance matrix. 
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_vcov(model)
#'
#' @export
#'
ols_get_vcov <- function(model) {
  vcov(model)
}

#' Deviance 
#' 
#' Returns the model deviance. 
#' 
#' @param model An object of class \code{lm}.
#'
#' @return Model deviance. 
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_deviance(model)
#'
#' @export
#'
ols_get_deviance <- function(model) {
  deviance(model)
}

#' Model parameters 
#' 
#' Returns the coefficients. 
#' 
#' @param model An object of class \code{lm}.
#'
#' @return A \code{data.frame} with model coefficients.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_parameters(model)
#'
#' @export
#'
ols_get_parameters <- function(model) {
  params <- model$coefficients
  data.frame(Parameter = names(params), Estimate = unname(params))
}

#' Predictor variable data
#' 
#' Returns the data of all model predictors.
#' 
#' @param model An object of class \code{lm}.
#'
#' @return A \code{data.frame} with model predictors.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_predictors(model)
#'
#' @export
#'
ols_get_predictors <- function(model) {
  model$model[, -1]
}

#' Response variable data
#' 
#' Returns the values of the response variable of a model.
#' 
#' @param model An object of class \code{lm}.
#'
#' @return Values of the response variable.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_response(model)
#'
#' @export
#'
ols_get_response <- function(model) {
  model$model[, 1]
}

#' Model function call 
#' 
#' Returns the model's function call. 
#' 
#' @param model An object of class \code{lm}.
#'
#' @return Model's function call. 
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl + hp + disp + gear + drat, data = mtcars)
#' ols_get_call(model)
#'
#' @export
#'
ols_get_call <- function(model) {
  model$call
}
