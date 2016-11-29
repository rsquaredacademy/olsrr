ovsp_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	x     <- fitted.values(model)
	y     <- unlist(model.frame(model)[1])
	oname <- names(model.frame(model))[1]

	plot(x, y, col = "blue", xlab = "Predicted Value", ylab = paste(oname), 
	     main = paste("Observed by Predicted for", oname))
	segments(min(x), min(y), max(x), max(y))
	abline(coef = c(0, 1))

}


