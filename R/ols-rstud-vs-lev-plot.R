#' @importFrom dplyr filter
#' @importFrom ggplot2 geom_vline
#' @title Studentized Residuals vs Leverage Plot 
#' @description Graph for detecting influential observations
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_rsdlev_plot(model)
#' @export
#'
ols_rsdlev_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	Observation <- NULL
	leverage <- NULL
	g <- rstudlev(model)
	d <- g$levrstud
	p <- ggplot(d, aes(leverage, rstudent))
	p <- p + geom_point(shape = 1, aes(colour = Observation))
	p <- p + scale_color_manual(values = c("blue", "red", "green", "violet"))
	p <- p + xlim(g$minx, g$maxx) + ylim(g$miny, g$maxy)
	p <- p + xlab('Leverage') + ylab('RStudent')
	p <- p + ggtitle(paste("Outlier and Leverage Diagnostics for", g$nam[1]))
	p <- p + geom_hline(yintercept = c(2, -2), colour = 'maroon')
	p <- p + geom_vline(xintercept = g$lev_thrsh, colour = 'maroon')
	print(p)

}