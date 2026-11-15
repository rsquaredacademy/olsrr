#' Leverage
#'
#' @description
#' The leverage of an observation is based on how much the observation's value
#' on the predictor variable differs from the mean of the predictor variable.
#' The greater an observation's leverage, the more potential it has to be an
#' influential observation.
#'
#' @param model An object of class \code{lm}.
#'
#' @return Leverage of the model.
#'
#' @references
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_leverage(model)
#'
#' @family influence measures
#'
#' @importFrom stats hatvalues
#'
#' @export
#'
ols_leverage <- function(model) {
  check_model(model)
  unname(hatvalues(model))  
}


#' Hadi's influence measure
#'
#' @description
#' Measure of influence based on the fact that influential observations in
#' either the response variable or in the predictors or both.
#'
#' @param model An object of class \code{lm}.
#'
#' @return Hadi's measure of the model.
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_hadi(model)
#'
#' @family influence measures
#'
#' @export
#'
ols_hadi <- function(model) {

  check_model(model)

  potential <- hadipot(model)
  residual  <- hadires(model)
  hi        <- potential + residual

  list(hadi      = hi,
       potential = potential,
       residual  = residual)

}

hadipot <- function(model) {

  lev <- ols_leverage(model)
  pii <- 1 - lev
  lev / pii

}

hadires <- function(model) {

  pii    <- 1 - ols_leverage(model)
  q      <- model$rank
  p      <- q - 1
  aov_m  <- anova(model)
  j      <- length(aov_m$Df)
  den    <- sqrt(aov_m[j, 2])
  dii    <- (model$residuals / den) ^ 2    
  first  <- (p + 1) / pii
  second <- dii / (1 - dii)

  first * second

}


#' PRESS
#'
#' @description
#' PRESS (prediction sum of squares) tells you how well the model will predict
#' new data.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' The prediction sum of squares (PRESS) is the sum of squares of the prediction
#' error. Each fitted to obtain the predicted value for the ith observation. Use
#' PRESS to assess your model's predictive ability. Usually, the smaller the
#' PRESS value, the better the model's predictive ability.
#'
#' For models fitted with prior weights, PRESS is the weighted sum of squared
#' deleted residuals, \eqn{\sum w_i (e_i / (1 - h_i))^2}. It therefore scales
#' with the weights: multiplying every weight by a constant multiplies PRESS by
#' the same constant. Unweighted models are the special case where every weight
#' equals one.
#'
#' @return Predicted sum of squares of the model.
#'
#' @references
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_press(model)
#'
#' @family influence measures
#'
#' @export
#'
ols_press <- function(model) {

  check_model(model)
  model_press_value(model, "ols_press")

}

# PRESS with the unit-leverage policy of model_influence_basis(): when deleting
# an observation leaves its prediction unestimable (leverage 1 within
# 10 * eps), the deleted residual is undefined, so PRESS is NA with a warning
# naming the rows rather than a total built from rounding error.
model_press_value <- function(model, fn) {

  lev <- model_hatvalues_used(model)
  res <- model_residuals_used(model)
  wts <- model_weights_used(model)

  unit <- which(lev > 1 - 10 * .Machine$double.eps)

  if (length(unit) > 0) {
    obs <- model_obs_ids(model)[model_used_rows(model)][unit]
    warning("`", fn, "()` is undefined: observation(s) ",
            paste(obs, collapse = ", "),
            " have leverage 1, so their deleted residuals cannot be estimated; returning NA.",
            call. = FALSE)
    return(NA_real_)
  }

  sum(wts * (res / (1 - lev)) ^ 2)

}

#' Predicted rsquare
#'
#' @description
#' Use predicted rsquared to determine how well the model predicts responses for
#' new observations. Larger values of predicted R2 indicate models of greater
#' predictive ability.
#'
#' @param model An object of class \code{lm}.
#'
#' @return Predicted rsquare of the model.
#'
#' @details Both the PRESS statistic and the total sum of squares use the
#' model's prior weights, so predicted R-squared is invariant to a constant
#' rescaling of the weights even though PRESS itself is not: PRESS is a weighted
#' sum of squared deleted residuals and scales with the weights.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_pred_rsq(model)
#'
#' @family influence measures
#'
#' @export
#'
ols_pred_rsq <- function(model) {

  check_model(model)
  tss  <- sum(anova(model)[[2]])
  prts <- model_press_value(model, "ols_pred_rsq") / tss
  1 - prts

}
