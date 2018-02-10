#' @importFrom stats hatvalues
#' @title Leverage
#' @description The leverage of an observation is based on how much the observation's value on the predictor variable
#' differs from the mean of the predictor variable. The greater an observation's leverage, the more potential it has
#' to be an influential observation.
#' @param model an object of class \code{lm}
#' @return leverage
#' @references Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_leverage(model)
#' @export
#'
ols_leverage <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  lev <- model %>% hatvalues() %>% unname()
  return(lev)
}


#' @title Hadi's Measure
#' @description Measure of influence based on the fact that influential observations in either
#' the response variable or in the predictors or both.
#' @param model an object of class \code{lm}
#' @return hadi's measure
#' @references Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_hadi(model)
#' @export
#'
ols_hadi <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  potential <- hadipot(model)
  residual <- hadires(model)
  hi <- potential + residual

  result <- list(
    hadi = hi,
    potential = potential,
    residual = residual
  )

  return(result)
}

#' @title PRESS (Prediction Sum of Squares)
#' @description PRESS tells you how well the model will predict new data.
#' @param model an object of class \code{lm}
#' @details The prediction sum of squares (PRESS) is the sum of squares of the prediction error. Each fitted

#' to obtain the predicted value for the ith observation. Use PRESS to assess your model's predictive ability.
#' Usually, the smaller the PRESS value, the better the model's predictive ability.
#' @return Predicted Sum of Squares
#' @references Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_press(model)
#' @export
#'
ols_press <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  k <- 1 %>%
    `-`(model %>% ols_leverage())

  out <- model %>%
    residuals() %>%
    `/`(k) %>%
    `^`(2) %>%
    sum()


  return(out)
}

#' @title Predicted Rsquare
#' @description Use predicted rsquared to determine how well the model predicts responses for new observations.
#' Larger values of predicted R2 indicate models of greater predictive ability.
#' @param model an object of class \code{lm}
#' @return Predicted Rsquare
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_pred_rsq(model)
#' @export
#'
ols_pred_rsq <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  tss <- model %>%
    anova() %>%
    `[[`(2) %>%
    sum()

  prts <- model %>%
    ols_press() %>%
    `/`(tss)

  predrsq <- 1 %>%
    `-`(prts)

  return(predrsq)
}
