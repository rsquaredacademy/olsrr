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

#' Compare model performance
#'
#' @description
#' Compare performance of regression models using multiple metrics.
#'
#' @param ... Multiple objects of class \code{lm}.
#'
#' @examples
#' model_1 <- lm(mpg ~ disp + hp, data = mtcars)
#' model_2 <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' model_3 <- lm(mpg ~ disp + hp + wt + qsec + + drat, data = mtcars)
#' ols_compare_model_performance(model_1, model_2, model_3)
#'
#' @export
#'
ols_compare_model_performance <- function(...) UseMethod("ols_compare_model_performance")

#' @export
#'
ols_compare_model_performance.lm <- function(...) {

  models <- list(...)
  n <- length(models)
  model_names <- sapply(substitute(list(...)), deparse)[-1]
  result <- data.frame(matrix(ncol = 9, nrow = 0))

  for (i in seq_len(n)) {
    result <-
      rbind(
        result,
        unlist(olsrr::ols_model_performance(models[[i]]))[-1]
      )
  }

  sigma <- vector(mode = "numeric", length = n)
  for (i in seq_len(n)) {
    sigma[i] = summary(models[[i]])$sigma
  }

  result <- cbind(model_names, result)
  result <- cbind(result, sigma)

  col_names <- c("name", "r2", "r2_adj", "r2_pred", "aic", "sbc", "sbic",
    "rmse", "sigma")
  colnames(result) <- col_names

  class(result) <- c("ols_compare_model_performance", "data.frame")

  return(result)
}

