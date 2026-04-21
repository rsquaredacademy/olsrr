#' Conduct hypothesis testing for provider effects from a fitted `linear_fe` object
#'
#' Conduct hypothesis tests on provider effects and identify outlying providers for a fixed effect linear model.
#'
#' @param fit a model fitted from \code{linear_fe}.
#' @param parm specifies a subset of providers for which confidence intervals are to be given.
#' By default, all providers are included. The class of `parm` should match the class of the provider IDs.
#' @param level the confidence level during the hypothesis test, meaning a significance level of \eqn{1 - \text{level}}.
#' The default value is 0.95.
#' @param null a character string or a number defining the null hypothesis for the provider effects.
#' The default value is \code{"median"}. The possible values are:
#' \itemize{
#'   \item{\code{"median"}}: The median of the provider effect estimates (\eqn{\hat{\gamma}_i}).
#'   \item{\code{"mean"}}: The weighted average of the provider effect estimates (\eqn{\hat{\gamma}_i}), where the weights correspond to the sample size of each provider.
#'   \item{numeric}: A user-defined numeric value representing the null hypothesis.
#' }
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#' \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param \dots additional arguments that can be passed to the function.
#'
#' @return A data frame containing the results of the hypothesis test, with the following columns:
#' \item{flag}{a flagging indicator where \code{1} means statistically higher than expected
#' and \code{-1} means statistically lower than expected.}
#' \item{p-value}{the p-value of the hypothesis test.}
#' \item{stat}{the test statistic.}
#' \item{Std.Error}{the standard error of the provider effect estimate.}
#'
#' @details
#' The function identifies outlying providers based on hypothesis test results.
#' For two-sided tests, \code{1} indicates performance significantly higher than expected, \code{-1} indicates lower,
#' For one-sided tests, \code{1} (right-tailed) or \code{-1} (left-tailed) flags are used.
#' Providers whose performance falls within the central range are flagged as \code{0}.
#' Outlying providers are determined by the test statistic falling beyond the threshold based on the significance level \eqn{1 - \text{level}}.
#'
#' @examples
#' data(ExampleDataLinear)
#' outcome <- ExampleDataLinear$Y
#' covar <- ExampleDataLinear$Z
#' ProvID <- ExampleDataLinear$ProvID
#' fit_linear <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
#' test(fit_linear)
#'
#' @importFrom stats pt qt median
#'
#' @exportS3Method test linear_fe

test.linear_fe <- function(fit, parm, level = 0.95, null = "median", alternative = "two.sided", ...) {
  alpha <- 1 - level

  data <- fit$data_include
  ProvID.char <- fit$char_list$ProvID.char
  gamma <- fit$coefficient$gamma
  se.gamma <- sqrt(fit$variance$gamma)
  n.prov <- sapply(split(data[, fit$char_list$Y.char], data[, ProvID.char]), length)
  m <- length(fit$coefficient$gamma)
  p <- length(fit$coefficient$beta)
  n <- nrow(fit$data_include)
  gamma.null <- ifelse(null=="median", median(gamma),
                       ifelse(null=="mean", sum(n.prov*gamma)/n,
                              ifelse(class(null)=="numeric", null[1],
                                     stop("Argument 'null' NOT as required!",call.=F))))


  # test statistics
  stat <- (gamma - gamma.null)/se.gamma

  prob <- switch(attributes(fit$variance$gamma)$description,
                 "simplified" = pnorm(stat, lower.tail=F),
                 "full" = pt(stat, df = n - m - p, lower.tail = F))

  if (alternative == "two.sided") {
    flag <- ifelse(prob < alpha/2, 1, ifelse(prob <= 1-alpha/2, 0, -1))
    p_value <- 2 * pmin(prob, 1-prob)
  }
  else if (alternative == "greater") {
    flag <- ifelse(prob < alpha, 1, 0)
    p_value <- prob
  }
  else if (alternative == "less") {
    flag <- ifelse(1 - prob < alpha, -1, 0)
    p_value <- 1 - prob
  }
  else {
    stop("Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
  }

  result <- data.frame(flag = factor(flag), p = p_value, stat = stat, Std.Error = se.gamma)
  colnames(result) <- c("flag", "p value", "stat", "Std.Error")

  if (missing(parm)) {
    attr(result, "provider size") <- n.prov
    return(result)
  }
  else {
    if (is.integer(parm)) {  #avoid "integer" class
      parm <- as.numeric(parm)
    }
    if (class(parm) == class(data[, ProvID.char])) {
      attr(result, "provider size") <- n.prov[names(n.prov) %in% parm]
      result <- result[row.names(result) %in% parm, ]
      return(result)
    } else {
      stop("Argument 'parm' includes invalid elements!")
    }
  }
}
