#' @importFrom ggplot2 scale_fill_manual
#' @title Studentized Residual Plot
#' @description Graph for identifying outliers
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_srsd_plot(model)
#' @export
#'
ols_srsd_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	obs <- NULL
	dsr <- NULL
	Observation <- NULL
	g <- srdata(model)
	d <- g$dsr
	p <- ggplot(d, aes(x = obs, y = dsr))
	p <- p + geom_bar(width = 0.5, stat = 'identity', aes(fill = Observation))
	p <- p + scale_fill_manual(values = c('blue', 'red'))
	p <- p + ylim(g$cminx, g$cmaxx)
	p <- p + coord_flip()
	p <- p + xlab('Observation') + ylab('Deleted Studentized Residuals')
	p <- p + ggtitle('Studentized Residuals')
	p <- p + geom_hline(yintercept = c(g$cminx, g$cmaxx), color = 'red')
	p <- p + geom_hline(yintercept = c(0, g$nseq, g$pseq))
	print(p)

}