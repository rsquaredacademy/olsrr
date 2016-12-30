#' @importFrom stats qqnorm qqline
#' @title Residual QQ Plot
#' @description Residual QQ Plot
#' @param model an object of class \code{lm}
#' @export
#'
qqresid <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	resid <- residuals(model)
	qqnorm(resid)
	qqline(resid)

}
