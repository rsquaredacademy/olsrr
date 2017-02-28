#' @importFrom stats cooks.distance
#' @importFrom ggplot2 geom_bar coord_flip ylim geom_hline
#' @title Cooks' D Bar Plot
#' @description Bar Plot of Cook's distance to detect observations that strongly influence fitted values of the model.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_cooksd_barplot(model)
#' @export
#'
ols_cooksd_barplot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	obs <- NULL
	cd <- NULL
	Observation <- NULL
	k <- cdplot(model)
	d <- k$ckd
	p <- ggplot(d, aes(x = obs, y = cd)) + geom_bar(width = 0.5, stat = 'identity', aes(fill = Observation))
	p <- p + scale_fill_manual(values = c('blue', 'red'))
	p <- p + coord_flip()
	p <- p + ylim(0, k$maxx)
	p <- p + ylab("Cook's D") + xlab('Observation') + ggtitle("Cook's D Bar Plot")
	p <- p + geom_hline(yintercept = 0)
	p <- p + geom_hline(yintercept = k$ts, colour = 'red')
	print(p)

}