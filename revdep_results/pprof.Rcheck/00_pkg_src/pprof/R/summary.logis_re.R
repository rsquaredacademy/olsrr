#' Result Summaries of Covariate Estimates from a fitted `logis_re` or `logis_cre` object
#'
#' Provide the summary statistics for the covariate estimates for a random/correlated random effect logistic model.
#'
#' @param object a model fitted from \code{logis_re} or \code{logis_cre}.
#' @param parm specifies a subset of covariates for which the result summaries should be output.
#' By default, all covariates are included.
#' @param level the confidence level during the hypothesis test, meaning a significance level of \eqn{1 - \text{level}}.
#' The default value is 0.95.
#' @param null a number defining the null hypothesis for the covariate estimates. The default value is \code{0}.
#' @param \dots additional arguments that can be passed to the function.
#'
#' @return A data frame containing summary statistics for covariate estimates, with the following columns:
#' \item{Estimate}{the estimates of covariate coefficients.}
#' \item{Std.Error}{the standard error of the estimate.}
#' \item{Stat}{the test statistic.}
#' \item{p value}{the p-value for the hypothesis test.}
#' \item{CI.upper}{the lower bound of the confidence interval.}
#' \item{CI.lower}{the upper bound of the confidence interval.}
#'
#' @examples
#' \donttest{
#' data(ExampleDataBinary)
#' outcome <- ExampleDataBinary$Y
#' covar <- ExampleDataBinary$Z
#' ProvID <- ExampleDataBinary$ProvID
#' fit_re <- logis_re(Y = outcome, Z = covar, ProvID = ProvID)
#' summary(fit_re)
#' }
#'
#' @importFrom stats pnorm qnorm confint
#'
#' @exportS3Method summary logis_re

summary.logis_re <- function(object, parm, level = 0.95, null = 0, ...) {
  alpha <- 1 - level

  if (missing(object)) stop ("Argument 'object' is required!",call.=F)
  if (!class(object) %in% c("logis_re")) stop("Object `object` is not of the classes 'logis_re'!",call.=F)

  model <- attributes(object)$model

  covar_char <- c("(intercept)", object$char_list$Z.char)

  FE_est <- object$coefficient$FE
  se.FE <- sqrt(diag(object$variance$FE))
  stat <- (FE_est - null) / se.FE
  p_value <- 2 * (1 - pnorm(stat))
  p_value <- format.pval(p_value, digits = 7, eps = 1e-10)
  CI <- confint(model, parm = "beta_", method = "Wald", level = level)

  result <- data.frame(FE = FE_est, se.FE = se.FE, stat = stat, p_value = p_value,
                       lower_bound = CI[,1], upper_bound = CI[,2])
  colnames(result) <- c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper")

  if (missing(parm)) {
    ind <- 1:length(covar_char)
  } else if (is.character(parm)) {
    ind <- which(covar_char %in% parm)
  } else if (is.numeric(parm) & max(abs(as.integer(parm) - parm)) == 0 & !(0 %in% parm)) {
    ind <- parm
  } else {
    stop("Argument 'parm' includes invalid elements!")
  }

  result <- result[ind, ]
  return(result)
}
