#' @importFrom ggplot2 geom_linerange
#' @title Cooks' D Chart
#' @description Chart of Cook's distance to detect observations that strongly influence fitted values of the model.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_cooksd_chart(model)
#' @export
#'
ols_cooksd_chart <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	obs <- NULL
	ckd <- NULL
	k <- cdchart(model)
	d <- k$d
	p <- ggplot(d, aes(x = obs, y = ckd, ymin = min(ckd), ymax = ckd))
	p <- p + geom_linerange(colour = 'blue')
	p <- p + geom_point(shape = 1, colour = 'blue')
	p <- p + geom_hline(yintercept = k$ts, colour = 'red')
	p <- p + xlab('Observation') + ylab("Cook's D") + ggtitle("Cook's D Chart")
	print(p)

}