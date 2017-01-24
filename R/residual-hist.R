#' @importFrom stats dnorm sd
#' @title Residual Histogram
#' @description Residual Histogram
#' @param model an object of class \code{lm}
#' @return \code{hist_resid} returns a list containing the
#' following components:
#'
#' \item{residuals}{f cook's d statistic}
#' \item{breaks}{f cook's d statistic}
#' \item{counts}{threshold for outliers}
#' \item{density}{f cook's d statistic}
#' \item{mids}{threshold for outliers}
#' \item{xname}{f cook's d statistic}
#' \item{equidist}{f cook's d statistic}
#' \item{norm}{f cook's d statistic}
#' @export
#'
hist_resid <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	k <- histdata(model)
	h <- hist(k$resid, xlim = c(k$minx, k$maxx), border = 'blue',
		main = 'Residual Histogram', xlab = 'Residuals')
	l <- histn(k$resid, h)
	lines(l$xfit, l$yfit, col = "blue", lwd = 2)

	z <- list(residuals = resid,
						breaks    = h$breaks,
						counts    = h$counts,
						density   = h$density,
						mids      = h$mids,
						xname     = h$xname,
						equidist  = h$equidist,
						norm      = yfit)

}
