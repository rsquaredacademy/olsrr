#' @importFrom stats residuals
#' @importFrom ggplot2 geom_boxplot theme element_blank
#' @title Residual Box Plot
#' @description Box plot of residuals from a linear regression model
#' @param model an object of class \code{lm}
#' @export
#'
box_resid <- function(model) {

	resid <- NULL
	d <- tibble(resid = residuals(model))
	p <- ggplot(d, aes(x = factor(0), y = resid)) +
	    geom_boxplot(outlier.color = 'green', outlier.size = 3,
	        fill = 'grey80', colour = '#3366FF') +
	    xlab(' ') + ylab('Residuals') + ggtitle('Residual Box Plot') +
			theme(axis.text.x = element_blank())

	print(p)

}
