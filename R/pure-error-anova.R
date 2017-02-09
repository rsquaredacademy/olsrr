#' @importFrom stats coefficients
#' @importFrom dplyr group_by_ select_ summarise_each funs
#' @title Lack  of Fit F Test
#' @description Lack of Fit F Test
#' @param model an object of class \code{lm}
#' @param ... other parameters
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return \code{pure_error_anova} returns an object of class
#' \code{"pure_error_anova"}. An object of class \code{"pure_error_anova"} is a
#' list containing the following components:
#'
#' \item{lackoffit}{f statistic}
#' \item{pure_error}{pure error}
#' \item{rss}{regression sum of squares}
#' \item{ess}{error sum of squares}
#' \item{total}{total sum of squares}
#' \item{rms}{}
#' \item{ems}{p-value of \code{fstat}}
#' \item{lms}{degrees of freedom}
#' \item{pms}{name(s) of \code{variable}}
#' \item{rf}{name of \code{group_var}}
#' \item{lf}{f statistic}
#' \item{pr}{p-value of \code{fstat}}
#' \item{pl}{degrees of freedom}
#' \item{mpred}{name(s) of \code{variable}}
#' \item{df_rss}{name of \code{group_var}}
#' \item{df_ess}{f statistic}
#' \item{df_lof}{p-value of \code{fstat}}
#' \item{df_error}{degrees of freedom}
#' \item{final}{name(s) of \code{variable}}
#' \item{resp}{name of \code{group_var}}
#' \item{preds}{name of \code{group_var}}
#' @export
#'
pure_error_anova <- function(model, ...) UseMethod('pure_error_anova')

#' @export
#'
pure_error_anova.default <- function(model, ...) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	ln <- length(coefficients(model))
	if (ln > 2) {
		stop("Lack of fit F test is available only for simple linear regression.", call. = FALSE)
	}

	k <- peanova(model)
	result <- list(lackoffit  = k$lackoffit,
		             pure_error = k$pure_error,
		             rss        = k$rss,
		             ess        = k$ess,
		             total      = k$total,
		             rms        = k$rms,
		             ems        = k$ems,
		             lms        = k$lms,
		             pms        = k$pms,
		             rf         = k$rf,
		             lf         = k$lf,
		             pr         = k$pr,
		             pl         = k$pl,
		             mpred      = k$mpred,
		             df_rss     = k$df_rss,
		             df_ess     = k$df_ess,
		             df_lof     = k$df_lof,
		             df_error   = k$df_error,
		             final      = k$final,
		             resp       = k$resp,
		             preds      = k$preds)

	class(result) <- 'pure_error_anova'

	return(result)

}

#' @export
#'
print.pure_error_anova <- function(x, ...) {
	print_pure_error_anova(x)
}
