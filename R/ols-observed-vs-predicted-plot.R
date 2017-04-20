#' @importFrom stats fitted.values
#' @importFrom ggplot2 geom_abline geom_segment
#' @title Actual vs Fitted Values Plot
#' @description Plot of actual vs fitted values to assess the fit of the model.
#' @param model an object of class \code{lm}
#' @details  Ideally, all your points should be close to a regressed diagonal line. Draw such a diagonal line 
#' within your graph and check out where the points lie. If your model had a high R Square, all the points would 
#' be close to this diagonal line. The lower the R Square, the weaker the Goodness of fit of your model, the more 
#' foggy or dispersed your points are from this diagonal line.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_ovsp_plot(model)
#' @export
#'
ols_ovsp_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	oname <- model %>%
    model.frame() %>%
    names() %>%
    `[`(1)

	x <- NULL
	y <- NULL
	d <- obspred(model)
	p <- ggplot(d, aes(x = x, y = y))
	p <- p + geom_point(color = 'blue', shape = 1)
	p <- p + ylab('Fitted Value') + xlab(paste(oname))
	p <- p + ggtitle(paste("Actual vs Fitted for", oname))
	p <- p + geom_abline(intercept = 0, slope = 1, color = 'blue')
	p <- p + geom_segment(data = d, aes(x = min(x), y = min(y), xend = max(x), yend = max(y)),
		colour = 'red')
	print(p)
}