#' @title Residual vs Predictor Plot
#' @description Residual vs Predictor Plot
#' @param model an object of class \code{lm}
#' @export
#'
rvsp_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	resid     <- residuals(model)
	predicted <- fitted(model)

	plot(predicted, resid, col = "blue",
	     xlab = "Predicted Value", ylab = "Residual",
	     main = "Residual vs Predicted Values")
	abline(h = 0, col = "gray")


}
