#' Correlation test for normality
#'
#' @description
#' Correlation between observed residuals and expected residuals under normality.
#'
#' @param model An object of class \code{lm}.
#'
#' @return Correlation between fitted regression model residuals and expected
#' values of residuals.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_test_correlation(model)
#'
#' @importFrom stats qnorm
#'
#' @family residual diagnostics
#'
#' @export
#'
ols_test_correlation <- function(model) {

  check_model(model)
  check_weights_diagnostic(model, "ols_test_correlation")
  corrout(model)

}

corrout <- function(model) {

  n      <- model_rows(model)
  stderr <- summary(model)$sigma
  h      <- ka(seq_len(n), stderr = stderr, n = n)
  out    <- sort(model_residuals_used(model))

  cor(h, out)

}

ka <- function(k, stderr, n) {
  stderr * qnorm((k - 0.375) / (n + 0.25))
}


#' Test for normality
#'
#' Test for detecting violation of normality assumption.
#'
#' @param y A numeric vector or an object of class \code{lm}.
#' @param ... Other arguments.
#'
#' @details
#' The tests differ in the sample sizes they support. \code{shapiro.test()}
#' requires between 3 and 5000 observations, \code{nortest::lillie.test()}
#' requires more than 4, and \code{nortest::ad.test()} and
#' \code{nortest::cvm.test()} require at least 8. Tests whose sample-size
#' requirement is not met are skipped: their statistic and p-value are returned
#' as \code{NA} and a single warning lists the skipped tests and the reason. If
#' the sample is too small for every test, the function stops with an error.
#'
#' The Kolmogorov-Smirnov test is the Lilliefors test,
#' \code{nortest::lillie.test()}. A plain \code{ks.test()} against a normal
#' distribution whose mean and standard deviation are estimated from the same
#' data is conservative: the estimation is not accounted for in the null
#' distribution, so the p-value is too large. The Lilliefors correction uses
#' the null distribution of the statistic with estimated parameters.
#'
#' The Cramér–von Mises test is \code{nortest::cvm.test()}, the test of
#' composite normality, which compares the data with a normal distribution
#' whose mean and standard deviation are estimated from the data. It is
#' deterministic. For very small p-values the test reports \code{7.37e-10}
#' with a warning instead of a more precise value.
#'
#' @return \code{ols_test_normality} returns an object of class \code{"ols_test_normality"}.
#' An object of class \code{"ols_test_normality"} is a list containing the
#' following components:
#'
#' \item{kolmogorv}{Lilliefors (Kolmogorov-Smirnov) statistic}
#' \item{shapiro}{shapiro wilk statistic}
#' \item{cramer}{cramer von mises statistic}
#' \item{anderson}{anderson darling statistic}
#'
#' @references
#' Stephens, M. A. (1986). Tests based on EDF statistics. In R. B. D'Agostino and M. A. Stephens (Eds.), \emph{Goodness-of-Fit Techniques} (pp. 97--193). New York: Marcel Dekker.
#'
#' Thode, H. C. (2002). \emph{Testing for Normality}. New York: Marcel Dekker.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_test_normality(model)
#'
#' @importFrom stats shapiro.test
#' @importFrom nortest ad.test lillie.test
#'
#' @family residual diagnostics
#'
#' @export
#'
ols_test_normality <- function(y, ...) UseMethod("ols_test_normality")

# Sample-size ranges supported by the normality tests. nortest::lillie.test()
# requires more than 4 observations, nortest::cvm.test() at least 8, and
# shapiro.test() between 3 and 5000.
normality_limits <- list(
  shapiro   = list(label = "Shapiro-Wilk", min = 3L, max = 5000L),
  kolmogorv = list(
    label = "Kolmogorov-Smirnov (Lilliefors)", min = 5L, max = Inf
  ),
  cramer    = list(label = "Cramer-von Mises", min = 8L, max = Inf),
  anderson  = list(label = "Anderson-Darling", min = 8L, max = Inf)
)

normality_skip_reason <- function(test, n) {
  lim <- normality_limits[[test]]

  if (n >= lim$min && n <= lim$max) {
    return(NULL)
  }

  if (is.finite(lim$max)) {
    paste0(
      lim$label, " test skipped: it requires between ", lim$min, " and ",
      lim$max, " observations (n = ", n, ")."
    )
  } else {
    paste0(
      lim$label, " test skipped: it requires at least ", lim$min,
      " observations (n = ", n, ")."
    )
  }
}

skipped_normality_test <- function(label, data_name) {
  structure(
    list(
      statistic = NA_real_,
      p.value   = NA_real_,
      method    = paste(label, "test (skipped)"),
      data.name = data_name
    ),
    class = "htest"
  )
}

#' @export
#'
ols_test_normality.default <- function(y, ...) {

  if (!is.numeric(y)) {
    stop("y must be numeric")
  }

  dname <- deparse(substitute(y))
  y     <- y[!is.na(y)]
  n     <- length(y)

  skipped <- vapply(
    names(normality_limits),
    function(test) !is.null(normality_skip_reason(test, n)),
    logical(1)
  )

  if (all(skipped)) {
    stop(
      paste0(
        "Normality tests cannot be performed: at least 3 observations are ",
        "required (n = ", n, ")."
      ),
      call. = FALSE
    )
  }

  if (any(skipped)) {
    warning(
      paste0(
        vapply(
          names(normality_limits)[skipped],
          function(test) normality_skip_reason(test, n),
          character(1)
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  if (skipped[["kolmogorv"]]) {
    ks <- skipped_normality_test("Kolmogorov-Smirnov (Lilliefors)", dname)
  } else {
    ks <- lillie.test(y)
  }

  if (skipped[["shapiro"]]) {
    sw <- skipped_normality_test("Shapiro-Wilk", dname)
  } else {
    sw <- shapiro.test(y)
  }

  if (skipped[["cramer"]]) {
    cvm <- skipped_normality_test("Cramer-von Mises", dname)
  } else {
    cvm <- nortest::cvm.test(y)
  }

  if (skipped[["anderson"]]) {
    ad <- skipped_normality_test("Anderson-Darling", dname)
  } else {
    ad <- ad.test(y)
  }

  result <- list(kolmogorv = ks, shapiro = sw, cramer = cvm, anderson = ad)

  class(result) <- "ols_test_normality"
  return(result)
}

#' @export
#' @rdname ols_test_normality
#'
ols_test_normality.lm <- function(y, ...) {

  if (!inherits(y, "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  ols_test_normality.default(residuals(y))

}

#' @export
#'
print.ols_test_normality <- function(x, ...) {
  print_norm_test(x)
}