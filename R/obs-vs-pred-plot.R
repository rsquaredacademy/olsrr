#' @importFrom stats fitted.values
#' @importFrom ggplot2 geom_abline geom_segment
#' @title Observed vs Predicted Plot
#' @description Plot of observed vs fitted values to assess the fit of the model.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ovsp_plot(model)
#' @export
#'
ovsp_plot <- function(model) {

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
	p <- p + xlab('Predicted Value') + ylab(paste(oname))
	p <- p + ggtitle(paste("Observed by Predicted for", oname))
	p <- p + geom_abline(intercept = 0, slope = 1, color = 'blue')
	p <- p + geom_segment(data = d, aes(x = min(x), y = min(y), xend = max(x), yend = max(y)),
		colour = 'red')
	print(p)
}


# ovsp_plot <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	x     <- fitted.values(model)
# 	y     <- unlist(model.frame(model)[1])
# 	oname <- names(model.frame(model))[1]
#
# 	plot(x, y, col = "blue", xlab = "Predicted Value", ylab = paste(oname),
# 	     main = paste("Observed by Predicted for", oname))
# 	segments(min(x), min(y), max(x), max(y))
# 	abline(coef = c(0, 1))
#
# }
