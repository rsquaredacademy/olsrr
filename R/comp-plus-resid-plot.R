#' @title Component Plus Residual Plot
#' @description Component plus residual plot
#' @param model an object of class \code{lm}
#' @export
#'
cplusr_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	e      <- residuals(model)
	mc     <- model$coefficients[-1]
	data   <- model.frame(model)[-1]
	lmc    <- length(mc)
	nam    <- names(data)
	indvar <- names(model.frame(model))[1]

	for (i in seq_len(lmc)) {

	    x <- data[i]
	    y <- (mc[i] * data[i]) + e
	    plot(x[[1]], y[[1]], xlab = nam[i], col = "blue",
	         ylab = paste0("Component + Residual (", indvar, ")"))
	    abline(lm(y[[1]] ~ x[[1]]), col = "red")

	}

}
