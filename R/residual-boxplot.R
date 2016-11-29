resid_boxplot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	resid <- residuals(model)
	boxplot(resid, xlab = 'Residuals',
	        main = "Residual Box Plot")

}