#' @importFrom stats dnorm sd
#' @importFrom graphics hist
#' @title Residual Histogram
#' @description Histogram of residuals for detecting violation of normality assumption.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_resid_hist(model)
#' @export
#'
ols_resid_hist <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	x <- NULL
	y <- NULL
	k <- histdata(model)
	h <- hist(k$resid, plot = FALSE)
	l <- histn(k$resid, h)
	d2 <- tibble(x = l$xfit, y = l$yfit)
	d <- tibble(x = k$resid)
	p <- ggplot(d, aes(x = x)) +
	    geom_histogram(bins = 6, color = 'black', fill = '#ADD8E6') +
	    geom_line(data = d2, aes(x = x, y = y), color = '#0000A0', size = 1.2) +
	    xlab('Residuals') + ggtitle('Residual Histogram')
	print(p)

}


# hist_resid <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	k <- histdata(model)
# 	h <- hist(k$resid, xlim = c(k$minx, k$maxx), border = 'blue',
# 		main = 'Residual Histogram', xlab = 'Residuals')
# 	l <- histn(k$resid, h)
# 	lines(l$xfit, l$yfit, col = "blue", lwd = 2)
#
# 	z <- list(residuals = resid,
# 						breaks    = h$breaks,
# 						counts    = h$counts,
# 						density   = h$density,
# 						mids      = h$mids,
# 						xname     = h$xname,
# 						equidist  = h$equidist,
# 						norm      = l$yfit)
#
# }
