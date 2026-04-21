#' Result Summaries of Covariate Estimates from a fitted `linear_fe`, `linear_re` or `linear_cre` object
#'
#' Provide the summary statistics for the covariate estimates for a fixed/random/correlated random effect linear model.
#'
#' @param object a model fitted from \code{linear_fe} or \code{linear_re} or \code{linear_cre}.
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
#' data(ExampleDataLinear)
#' outcome <- ExampleDataLinear$Y
#' covar <- ExampleDataLinear$Z
#' ProvID <- ExampleDataLinear$ProvID
#' fit_fe <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
#' summary(fit_fe)
#'
#' @importFrom stats pt qt
#'
#' @exportS3Method summary linear_fe

summary.linear_fe <- function(object, parm, level = 0.95, null = 0, ...) {
  alpha <- 1 - level

  if (missing(object)) stop ("Argument 'object' is required!",call.=F)
  if (!class(object) %in% c("linear_fe")) stop("Object `object` is not of the classes 'linear_fe'!",call.=F)

  Z.char <- object$char_list$Z.char
  beta <- object$coefficient$beta
  se.beta <- sqrt(diag(object$variance$beta))
  m <- length(object$coefficient$gamma)
  p <- length(object$coefficient$beta)
  n <- nrow(object$data_include)

  # Test Statistics
  stat <- (beta - null) / se.beta

  p_value <- 2 * (1 - pt(abs(stat), df = n - p - m))
  crit_value <- qt(1 - alpha / 2, df = n - p - m)
  lower_bound <- beta - crit_value * se.beta
  upper_bound <- beta + crit_value * se.beta

  # if (alternative == "two.sided") {
  #   p_value <- 2 * (1 - pt(abs(stat), df = n - p - m))
  #   crit_value <- qt(1 - alpha / 2, df = n - p - m)
  #   lower_bound <- beta - crit_value * se.beta
  #   upper_bound <- beta + crit_value * se.beta
  # }
  # else if (alternative == "greater") {
  #   p_value <- 1 - pt(stat, df = n - p - m)
  #   crit_value <- qt(level, df = n - p - m)
  #
  #   lower_bound <- beta - crit_value * se.beta
  #   upper_bound <- Inf
  # }
  # else if (alternative == "less") {
  #   p_value <- pt(stat, df = n - p - m)
  #   crit_value <- qt(level, df = n - p - m)
  #
  #   lower_bound <- -Inf
  #   upper_bound <- beta + crit_value * se.beta
  # }
  # else {
  #   stop("Argument 'alternative' should be one of 'two.sided', 'less', 'greater'.")
  # }

  p_value <- format.pval(p_value, digits = 7, eps = 1e-10)

  result <- data.frame(beta = beta, se.beta = se.beta, stat = stat, p_value = p_value,
                       lower_bound = lower_bound, upper_bound = upper_bound)
  colnames(result) <- c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper")

  if (missing(parm)) {
    ind <- 1:length(Z.char)
  } else if (is.character(parm)) {
    ind <- which(Z.char %in% parm)
  } else if (is.numeric(parm) & max(abs(as.integer(parm) - parm)) == 0 & !(0 %in% parm)) {
    ind <- parm
  } else {
    stop("Argument 'parm' includes invalid elements!")
  }

  result <- result[ind, ]
  return(result)
}
