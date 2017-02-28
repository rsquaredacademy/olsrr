#' @title Residual vs Fitted Plot
#' @description Plot to detect non-linearity, unequal error variances, and outliers.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_rvsp_plot(model)
#' @export
#'
ols_rvsp_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	predicted <- NULL
	resid <- NULL

  d <- rvspdata(model)
	p <- ggplot(d, aes(x = predicted, y = resid))
	p <- p + geom_point(shape = 1, colour = 'blue')
	p <- p + xlab('Fitted Value') + ylab('Residual')
	p <- p + ggtitle('Residual vs Fitted Values')
	p <- p + geom_hline(yintercept = 0, colour = 'red')
	print(p)

}