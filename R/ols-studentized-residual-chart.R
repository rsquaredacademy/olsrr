#' @importFrom stats rstandard
#' @title Studentized Residual Chart
#' @description Chart for identifying outliers 
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_srsd_chart(model)
#' @export
#'
ols_srsd_chart <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	    obs <- NULL
	  sdres <- rstandard(model)
	outlier <- sdres[abs(sdres) > 2]
	      d <- data.frame(obs = seq_len(length(sdres)), sdres = sdres)

	p <- ggplot(d, aes(x = obs, y = sdres, ymin = 0, ymax = sdres))
	p <- p + geom_linerange(colour = 'blue')
	p <- p + geom_point(shape = 1, colour = 'blue')
	p <- p + geom_hline(yintercept = 0, colour = 'gray')
	p <- p + geom_hline(yintercept = c(2, -2), colour = 'red')
	p <- p + xlab('Observation') + ylab('Studentized Residuals')
	p <- p + ggtitle('Studentized Residuals Chart')
	print(p)

	z <- list(studresid = sdres,
						threshold = 2,
						outliers  = d$obs)
}