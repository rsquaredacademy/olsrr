#' @importFrom stats qnorm
#' @title Correlation Test
#' @description Correlation Test
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return correlation between fitted regression model residuals and expected
#' values of residuals
#' @export
#'
corr_test <- function(model) {

		if (!all(class(model) == 'lm')) {
      stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    return(corout(model))

}

#' @importFrom stats ks.test shapiro.test
#' @importFrom goftest cvm.test
#' @importFrom nortest ad.test
#' @title Normality Test
#' @description Normality Test
#' @param y a numeric vector
#' @param ... other arguments
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return \code{norm_test} returns an object of class \code{"norm_test"}.
#' An object of class \code{"norm_test"} is a list containing the
#' following components:
#'
#' \item{kolmogorv}{kolmogorv smirnov statistic}
#' \item{shapiro}{shapiro wilk statistic}
#' \item{cramer}{cramer von mises statistic}
#' \item{anderson}{anderson darling statistic}
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
	cvm <- cvm.test(y, "pnorm", mean(y), sd(y))
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
