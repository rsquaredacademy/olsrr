#' @importFrom stats coefficients
#' @importFrom dplyr group_by_ select_ funs
#' @title Lack of Fit F Test
#' @description Assess how much of the error in prediction is due to lack of model fit.
#' @param model an object of class \code{lm}
#' @param ... other parameters
#' @details The residual sum of squares resulting from a regression can be decomposed into 2 components:
#'
#' \itemize{
#'   \item Due to lack of fit
#'   \item Due to random variation
#' }
#'
#' If most of the error is due to lack of fit and not just random error, the model should be discarded and
#' a new model must be built.
#'
#' @note The lack of fit F test works only with simple linear regression. Moreover, it is important that the
#' data contains repeat observations i.e. replicates for at least one of the values of the predictor x. This
#' test generally only applies to datasets with plenty of replicates.
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
#' \item{mpred}{tibble containing data for the response and predictor of the \code{model}}
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
#' @export
#'
ols_pure_error_anova <- function(model, ...) UseMethod("ols_pure_error_anova")

#' @export
#'
ols_pure_error_anova.default <- function(model, ...) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  ln <- length(coefficients(model))
  if (ln > 2) {
    stop("Lack of fit F test is available only for simple linear regression.", call. = FALSE)
  }

  k <- peanova(model)
  result <- list(
    lackoffit = k$lackoffit,
    pure_error = k$pure_error,
    rss = k$rss,
    ess = k$ess,
    total = k$total,
    rms = k$rms,
    ems = k$ems,
    lms = k$lms,
    pms = k$pms,
    rf = k$rf,
    lf = k$lf,
    pr = k$pr,
    pl = k$pl,
    mpred = k$mpred,
    df_rss = k$df_rss,
    df_ess = k$df_ess,
    df_lof = k$df_lof,
    df_error = k$df_error,
    final = k$final,
    resp = k$resp,
    preds = k$preds
  )

  class(result) <- "ols_pure_error_anova"

  return(result)
}

#' @export
#'
print.ols_pure_error_anova <- function(x, ...) {
  print_pure_error_anova(x)
}

#' @importFrom dplyr arrange
peanova <- function(model) {
  data <- model.frame(model)
  dep <- data[[1]]
  pred <- data[[2]]
  n <- nrow(data)
  nam <- names(data)
  dep_name <- nam[1]
  pred_name <- nam[2]
  yhat <- model$fitted.values
  pred_u <- table(pred)
  nd <- length(pred_u)

  mean_pred <- data %>%
    group_by_(pred_name) %>%
    select_(dep_name, pred_name) %>%
    summarise_all(funs(mean))

  mean_rep <- rep(mean_pred[[2]], as.vector(pred_u))
  fin <- data.frame(dep, yhat, pred)
  finl <- arrange(fin, pred)
  final <- cbind(finl, mean_rep)
  colnames(final) <- c("y", "yhat", "pred", "ybar")

  final$lfit <- (final$ybar - final$yhat) ^ 2
  final$rerror <- (final$y - final$ybar) ^ 2
  # final <- mutate(final,
  #   lfit   = (ybar - yhat) ^ 2,
  #   rerror = (y - ybar) ^ 2
  # )

  lackoffit <- sum(final$lfit)
  random_error <- sum(final$rerror)
  rss <- anova(model)[1, 2]
  ess <- sum(lackoffit, random_error)
  total <- sum(ess, rss)
  df_rss <- 1
  df_lof <- nd - 2
  df_error <- n - nd
  df_ess <- sum(df_lof, df_error)
  rms <- rss / df_rss
  ems <- ess / df_ess
  lms <- lackoffit / df_lof
  pms <- random_error / df_error
  rf <- rms / pms
  lf <- lms / pms
  pr <- pf(rf, df_rss, df_ess, lower.tail = F)
  pl <- pf(lf, df_lof, df_error, lower.tail = F)

  result <- list(
    lackoffit = lackoffit,
    pure_error = random_error,
    rss = rss,
    ess = ess,
    total = total,
    rms = rms,
    ems = ems,
    lms = lms,
    pms = pms,
    rf = rf,
    lf = lf,
    pr = pr,
    pl = pl,
    mpred = mean_pred,
    df_rss = df_rss,
    df_ess = df_ess,
    df_lof = df_lof,
    df_error = df_error,
    final = final,
    resp = dep_name,
    preds = pred_name
  )
  return(result)
}
