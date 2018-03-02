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
#' @section Deprecated Function:
#' \code{ols_corr_test()} has been deprecated. Instead use \code{ols_test_correlation()}.
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

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  corrout(model)

}

#' @export
#' @rdname ols_test_correlation
#' @usage NULL
#'
ols_corr_test <- function(model) {
  .Deprecated("ols_test_correlation()")
}


corrout <- function(model) {

  n <- model_rows(model)

  stderr <-
    model %>%
    summary() %>%
    extract2(6)

  h <-
    n %>%
    seq_len() %>%
    ka(stderr = stderr, n = n)

  out <-
    model %>%
    residuals() %>%
    sort()

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
#' @return \code{ols_test_normality} returns an object of class \code{"ols_test_normality"}.
#' An object of class \code{"ols_test_normality"} is a list containing the
#' following components:
#'
#' \item{kolmogorv}{kolmogorv smirnov statistic}
#' \item{shapiro}{shapiro wilk statistic}
#' \item{cramer}{cramer von mises statistic}
#' \item{anderson}{anderson darling statistic}
#'
#' @section Deprecated Function:
#' \code{ols_norm_test()} has been deprecated. Instead use \code{ols_test_normality()}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_test_normality(model)
#'
#' @importFrom stats ks.test shapiro.test
#' @importFrom goftest cvm.test
#' @importFrom nortest ad.test
#'
#' @family residual diagnostics
#'
#' @export
#'
ols_test_normality <- function(y, ...) UseMethod("ols_test_normality")

#' @export
#'
ols_test_normality.default <- function(y, ...) {

  if (!is.numeric(y)) {
    stop("y must be numeric")
  }

  ks  <- ks.test(y, "pnorm", mean(y), sd(y))
  sw  <- shapiro.test(y)
  cvm <- cvm.test(y)
  ad  <- ad.test(y)

  result <- list(kolmogorv = ks,
                 shapiro   = sw,
                 cramer    = cvm,
                 anderson  = ad)

  class(result) <- "ols_test_normality"
  return(result)
}

#' @export
#' @rdname ols_test_normality
#'
ols_test_normality.lm <- function(y, ...) {

  if (!all(class(y) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  ols_test_normality.default(residuals(y))

}

#' @export
#'
print.ols_test_normality <- function(x, ...) {
  print_norm_test(x)
}

#' @export
#' @rdname ols_test_normality
#' @usage NULL
#'
ols_norm_test <- function(y, ...) {
  .Deprecated("ols_test_normality()")
}
