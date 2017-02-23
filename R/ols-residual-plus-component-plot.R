#' @title Residual Plus Component Plot
#' @description The residual plus component plot indicates whether any non-linearity is present
#' in the relationship between response and predictor variables and can suggest possible transformations
#' for linearizing the data.
#' @param model an object of class \code{lm}
#' @references Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition). 
#' Chicago, IL., McGraw Hill/Irwin.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_rpc_plot(model)
#' @export
#'
ols_rpc_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	pl <- cpout(model)
	 x <- NULL
	 y <- NULL
	 myplots <- list()
	for (i in seq_len(pl$lmc)) {

			k <- cpdata(pl$data, pl$mc, pl$e, i)
			p <- eval(substitute(ggplot(k, aes(x = x, y = y)) +
				geom_point(colour = 'blue', size = 2) + xlab(pl$nam[i]) +
				ylab(paste0("Residual + Component (", pl$indvar, ")")) +
				stat_smooth(method="lm", se=FALSE), list(i = i)))

			print(p)
			myplots[[i]] <- p

	}

	do.call(grid.arrange, c(myplots, list(ncol = 2)))

}


# cplusr_plot <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	e      <- residuals(model)
# 	mc     <- model$coefficients[-1]
# 	data   <- model.frame(model)[-1]
# 	lmc    <- length(mc)
# 	nam    <- names(data)
# 	indvar <- names(model.frame(model))[1]
#
# 	for (i in seq_len(lmc)) {
#
# 	    x <- data[i]
# 	    y <- (mc[i] * data[i]) + e
# 	    plot(x[[1]], y[[1]], xlab = nam[i], col = "blue",
# 	         ylab = paste0("Component + Residual (", indvar, ")"))
# 	    abline(lm(y[[1]] ~ x[[1]]), col = "red")
#
# 	}
#
# }
