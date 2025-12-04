#' Model Performance
#'
#' @description
#' Evaluate performance of regression models.
#'
#' @param model An object of class \code{lm}.
#' @param ... Other arguments.
#'
#' @return \code{ols_model_performance} returns an object of class
#' \code{"ols_model_performance"}. An object of class \code{"ols_regress"} is a
#' list containing the following components:
#'
#' \item{r}{square root of rsquare, correlation between observed and predicted values of dependent variable}
#' \item{rsq}{coefficient of determination or r-square}
#' \item{adjr}{adjusted rsquare}
#' \item{prsq}{predicted rsquare}
#' \item{aic}{akaike information criteria}
#' \item{sbc}{bayesian information criteria}
#' \item{sbic}{sawa bayesian information criteria}
#' \item{rmse}{root mean squared error}
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#'
#' # model performance
#' ols_model_performance(model)
#'
#' @export
#'
ols_model_performance <- function(model, ...) UseMethod("ols_model_performance")

#' @export
#'
ols_model_performance.default <- function(model, ...) {
  rsq  <- summary(model)$r.squared
  r    <- sqrt(rsq)
  adjr <- summary(model)$adj.r.squared
  rmse <- sqrt(mean(model$residuals ^ 2))
  aic  <- ols_aic(model)
  sbc  <- ols_sbc(model)
  sbic <- ols_sbic(model, model)
  prsq <- ols_pred_rsq(model)

  result <- list(
    r    = r,
    rsq  = rsq,
    adjr = adjr,
    prsq = prsq,
    aic  = aic,
    sbc  = sbc,
    sbic = sbic,
    rmse = rmse
  )

  class(result) <- "ols_model_performance"

  return(result)

}

#' @export
#'
print.ols_model_performance <- function(x, ...) {
  print_model_perf(x)
}
