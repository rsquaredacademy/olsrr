#' @importFrom stats fitted rstudent
#' @importFrom dplyr mutate
#' @title Deleted Studentized Residual vs Fitted Values Plot
#' @description Plot for detecting outliers.
#' @param model an object of class \code{lm}
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_dsrvsp_plot(model)
#' @export
#'
ols_dsrvsp_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	dsr <- NULL
	pred <- NULL
	Observation <- NULL
	k <- dpred(model)
	d <- k$ds
	p <- ggplot(d, aes(x = pred, y = dsr))
	p <- p + geom_point(aes(colour = Observation))
	p <- p + scale_color_manual(values = c('blue', 'red'))
	p <- p + ylim(k$cminx, k$cmaxx)
	p <- p + xlab('Predicted Value') + ylab('Deleted Studentized Residual')
	p <- p + ggtitle("Deleted Studentized Residual vs Predicted Values")
	p <- p + geom_hline(yintercept = c(-2, 2), colour = 'red')
	print(p)

}