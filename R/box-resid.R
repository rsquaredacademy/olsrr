box_resid <- function(model) {

	resid <- residuals(model)
	boxplot(resid, xlab = 'Residuals',
	        main = "Residual Box Plot")

}