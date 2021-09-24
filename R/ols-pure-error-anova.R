#' Lack of fit F test
#'
#' Assess how much of the error in prediction is due to lack of model fit.
#'
#' @param model An object of class \code{lm}.
#' @param ... Other parameters.
#'
#' @details
#' The residual sum of squares resulting from a regression can be decomposed
#' into 2 components:
#'
#' \itemize{
#'   \item Due to lack of fit
#'   \item Due to random variation
#' }
#'
#' If most of the error is due to lack of fit and not just random error, the
#' model should be discarded and a new model must be built.
#'
#' @note The lack of fit F test works only with simple linear regression.
#' Moreover, it is important that the data contains repeat observations i.e.
#' replicates for at least one of the values of the predictor x. This
#' test generally only applies to datasets with plenty of replicates.
#'
#' @return \code{ols_pure_error_anova} returns an object of class
#' \code{"ols_pure_error_anova"}. An object of class \code{"ols_pure_error_anova"} is a
#' list containing the following components:
#'
#' \item{lackoffit}{lack of fit sum of squares}
#' \item{pure_error}{pure error sum of squares}
#' \item{rss}{regression sum of squares}
#' \item{ess}{error sum of squares}
#' \item{total}{total sum of squares}
#' \item{rms}{regression mean square}
#' \item{ems}{error mean square}
#' \item{lms}{lack of fit mean square}
#' \item{pms}{pure error mean square}
#' \item{rf}{f statistic}
#' \item{lf}{lack of fit f statistic}
#' \item{pr}{p-value of f statistic}
#' \item{pl}{p-value pf lack of fit f statistic}
#' \item{mpred}{\code{data.frame} containing data for the response and predictor of the \code{model}}
#' \item{df_rss}{regression sum of squares degrees of freedom}
#' \item{df_ess}{error sum of squares degrees of freedom}
#' \item{df_lof}{lack of fit degrees of freedom}
#' \item{df_error}{pure error degrees of freedom}
#' \item{final}{data.frame; contains computed values used for the lack of fit f test}
#' \item{resp}{character vector; name of \code{response variable}}
#' \item{preds}{character vector; name of \code{predictor variable}}
#'
#' @references
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#'
#' @examples
#' model <- lm(mpg ~ disp, data = mtcars)
#' ols_pure_error_anova(model)
#'
#' @importFrom stats coefficients
#'
#' @export
#'
ols_pure_error_anova <- function(model, ...) UseMethod("ols_pure_error_anova")

#' @export
#'
ols_pure_error_anova.default <- function(model, ...) {

  check_model(model)

  ln <- length(coefficients(model))

  if (ln > 2) {
    stop("Lack of fit F test is available only for simple linear regression.", call. = FALSE)
  }

  k <- peanova(model)

  result <- list(lackoffit = k$lackoffit, pure_error = k$pure_error, rss = k$rss,
    ess = k$ess, total = k$total, rms = k$rms, ems = k$ems, lms = k$lms,
    pms = k$pms, rf = k$rf, lf = k$lf, pr = k$pr, pl = k$pl, mpred = k$mpred,
    df_rss = k$df_rss, df_ess = k$df_ess, df_lof = k$df_lof,
    df_error = k$df_error, final = k$final, resp = k$resp, preds = k$preds)

  class(result) <- "ols_pure_error_anova"

  return(result)

}

#' @export
#'
print.ols_pure_error_anova <- function(x, ...) {
  print_pure_error_anova(x)
}

peanova <- function(model) {

  lfit   <- NULL
  rerror <- NULL

  n         <- model_rows(model)
  nd        <- pred_table_length(model)
  comp      <- pea_data(model)
  final     <- comp$result
  mean_pred <- comp$mean_pred
  pred_name <- comp$pred_name
  dep_name  <- comp$resp
  lackoffit <- sum(final$lfit)
  
  random_error <- sum(final$rerror)

  rss      <- rss_model(model)
  ess      <- sum(lackoffit, random_error)
  total    <- sum(ess, rss)
  df_rss   <- 1
  df_lof   <- nd - 2
  df_error <- n - nd
  df_ess   <- sum(df_lof, df_error)
  rms      <- rss / df_rss
  ems      <- ess / df_ess
  lms      <- lackoffit / df_lof
  pms      <- random_error / df_error
  rf       <- rms / pms
  lf       <- lms / pms
  pr       <- pf(rf, df_rss, df_ess, lower.tail = F)
  pl       <- pf(lf, df_lof, df_error, lower.tail = F)

  list(
    lackoffit = lackoffit, pure_error = random_error, rss = rss, ess = ess,
    total = total, rms = rms, ems = ems, lms = lms, pms = pms, rf = rf, lf = lf,
    pr = pr, pl = pl, mpred = mean_pred, df_rss = df_rss, df_ess = df_ess,
    df_lof = df_lof, df_error = df_error, final = final, resp = dep_name,
    preds = pred_name
  )

}

pea_data <- function(model) {

  data      <- model.frame(model)
  pred_name <- names(data)[2]
  resp      <- names(data)[1]
  pred_u    <- pred_table(model)
  mean_pred <- predictor_mean(data, pred_name, resp)
  mean_rep  <- replicate_mean(mean_pred, pred_u)
  result    <- pea_data_comp(data, model, mean_rep)

  list(pred_name = pred_name,
       resp      = resp,
       result    = result,
       mean_pred = mean_pred)

}

pred_table <- function(model) {
  table(model.frame(model)[[2]])
}

pred_table_length <- function(model) {
  length(pred_table(model))
}

predictor_mean <- function(data, pred_name, resp) {

  d   <- split(data[[resp]], data[[pred_name]])
  out <- lapply(d, mean)
  data.frame(n = names(out), mean = as.numeric(out))

}

rss_model <- function(model) {
  anova(model)[1, 2]
}


replicate_mean <- function(mean_pred, pred_u) {

  out <- as.data.frame(rep(mean_pred[[2]], times = pred_u))
  colnames(out) <- c("ybar")
  return(out)

}


pea_data_comp <- function(data, model, mean_rep) {

  pred <- NULL
  ybar <- NULL
  yhat <- NULL
  y    <- NULL

  data$yhat      <- fitted(model)
  colnames(data) <- c("y", "pred", "yhat")
  data           <- data[order(data$pred), ]
  data           <- cbind(data, mean_rep)
  data$lfit      <- (data$ybar - data$yhat) ^ 2
  data$rerror    <- (data$y - data$ybar) ^ 2

  return(data)

}
