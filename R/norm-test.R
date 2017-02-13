#' @importFrom stats qnorm
#' @title Correlation Test For Normality
#' @description Correlation between observed residuals and expected residuals under normality.
#' @param model an object of class \code{lm}
#' @return correlation between fitted regression model residuals and expected
#' values of residuals
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' corr_test(model)
#' @export
#'
corr_test <- function(model) {

		if (!all(class(model) == 'lm')) {
      stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    return(corrout(model))

}

#' @importFrom stats ks.test shapiro.test
#' @importFrom goftest cvm.test
#' @importFrom nortest ad.test
#' @title Test for normality
#' @description Test for detecting violation of normality assumption.
#' @param y a numeric vector
#' @param ... other arguments
#' @return \code{norm_test} returns an object of class \code{"norm_test"}.
#' An object of class \code{"norm_test"} is a list containing the
#' following components:
#'
#' \item{kolmogorv}{kolmogorv smirnov statistic}
#' \item{shapiro}{shapiro wilk statistic}
#' \item{cramer}{cramer von mises statistic}
#' \item{anderson}{anderson darling statistic}
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' norm_test(model)
#' @export
#'
norm_test <- function(y, ...) UseMethod('norm_test')

#' @export
#'
norm_test.default <- function(y, ...) {

	if (!is.numeric(y)) {
		stop('y must be numeric')
	}

	ks  <- ks.test(y, "pnorm", mean(y), sd(y))
	sw  <- shapiro.test(y)
	cvm <- cvm.test(y)
	ad  <- ad.test(y)

	result <- list(kolmogorv = ks,
								 shapiro   = sw,
								 cramer    = cvm,
								 anderson  = ad)

	class(result) <- 'norm_test'
	return(result)
}

#' @export
#'
norm_test.lm <- function(y, ...) {
	if (!all(class(y) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }
	norm_test.default(residuals(y))
}

#' @export
#'
print.norm_test <- function(x, ...) {
	print_norm_test(x)
}
