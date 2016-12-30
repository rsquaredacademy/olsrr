#' @importFrom stats residuals
#' @importFrom graphics boxplot
#' @title Residual Box Plot
#' @description Box plot of residuals from a linear regression model
#' @param model an object of class \code{lm}
#' @export
#'
box_resid <- function(model) {
	resid <- residuals(model)
	boxplot(resid, xlab = 'Residuals',
	        main = "Residual Box Plot")
}
