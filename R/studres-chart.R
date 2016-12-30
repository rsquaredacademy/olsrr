#' @importFrom graphics text
#' @importFrom stats rstandard
#' @title Studentized Residual Chart
#' @description Studentized Residual Chart
#' @param model an object of class \code{lm}
#' @return \code{studres_chart} returns a list containing the
#' following components:
#'
#' \item{studresid}{f cook's d statistic}
#' \item{threshold}{threshold for outliers}
#' \item{outliers}{f cook's d statistic}
#' @export
#'
studres_chart <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	sdres   <- rstandard(model)
	outlier <- sdres[abs(sdres) > 2]
	obs     <- as.numeric(names(outlier))

	plot(seq_len(length(sdres)), sdres, type = "h", lwd = 1, col = "blue",
	     xlab = "Observation", ylab = "Studentized Residuals",
	     main = "Studentized Residuals Chart")
	points(sdres, col = "blue")
	abline(h = 0, col = "black")
	abline(h = c(2, -2), col = "gray")
	text(jitter(obs, factor = 2), as.vector(outlier), obs, cex = 0.8)

	z <- list(studresid = sdres,
						threshold = 2,
						outliers  = obs)
}
