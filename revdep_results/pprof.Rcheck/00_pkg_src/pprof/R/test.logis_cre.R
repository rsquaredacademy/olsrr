#' Conduct hypothesis testing for provider effects from a fitted `logis_cre` object
#'
#' Conduct hypothesis tests on provider effects and identify outlying providers for a correlated random effect logistic model.
#'
#' @param fit a model fitted from \code{logis_cre}.
#' @param parm specifies a subset of providers for which confidence intervals are to be given.
#' By default, all providers are included. The class of `parm` should match the class of the provider IDs.
#' @param level the confidence level during the hypothesis test, meaning a significance level of \eqn{1 - \text{level}}.
#' The default value is 0.95.
#' @param null a number defining the null hypothesis for the provider effects.
#' The default value is 0.
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
#' data(ExampleDataBinary)
#' outcome <- ExampleDataBinary$Y
#' covar <- ExampleDataBinary$Z
#' ProvID <- ExampleDataBinary$ProvID
#' data <- data.frame(outcome, ProvID, covar)
#' outcome.char <- colnames(data)[1]
#' ProvID.char <- colnames(data)[2]
#' wb.char <- c("z1", "z2")
#' other.char <- c("z3", "z4", "z5")
#' fit_cre <- logis_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
#' wb.char = wb.char, other.char = other.char)
#' test(fit_cre)
#'
#' @importFrom stats pnorm qnorm
#'
#' @exportS3Method test logis_cre

test.logis_cre <- function(fit, parm, level = 0.95, null = 0, alternative = "two.sided", ...) {
  model <- attributes(fit)$model

  alpha <- 1 - level

  data <- fit$data_include
  Y.char <- fit$char_list$Y.char
  ProvID.char <- fit$char_list$ProvID.char
  Z.char <- fit$char_list$Z.char
  n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)

  postVar <- matrix(attr(ranef(model, condVar = TRUE)[[ProvID.char]], "postVar")[1,1,], ncol = 1)
  rownames(postVar) <- rownames(fit$coefficient$RE)
  colnames(postVar) <- "PostVar"
  PostSE <- sqrt(postVar)

  Z_score <- (fit$coefficient$RE - null)/PostSE
  p <- pnorm(Z_score, lower.tail=F)

  if (alternative == "two.sided") {
    p_value <- 2 * pmin(p, 1-p)
    flag <- ifelse(p < alpha/2, 1, ifelse(p > 1 - alpha/2, -1, 0))
  }
  else if (alternative == "greater") {
    p_value <- p
    flag <- ifelse(p < alpha, 1, 0)
  }
  else if (alternative == "less") {
    p_value <- 1 - p
    flag <- ifelse(1 - p < alpha, -1, 0)
  }
  else {
    stop("Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
  }

  result <- data.frame(flag = factor(flag), p = p_value, stat = Z_score, Std.Error = PostSE)
  colnames(result) <- c("flag", "p value", "stat", "Std.Error")

  if (missing(parm)) {
    attr(result, "provider size") <- n.prov
    return(result)
  }
  else {
    if (is.numeric(parm)) {  #avoid "integer" class
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
