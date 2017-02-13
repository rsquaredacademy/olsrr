#' @importFrom ggplot2 geom_linerange
#' @title Cooks' D Chart
#' @description Chart of Cook's distance to detect observations that strongly influence fitted values of the model.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' cooksd_chart(model)
#' @export
#'
cooksd_chart <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	obs <- NULL
	ckd <- NULL
	k <- cdchart(model)
	d <- k$d
	p <- ggplot(d, aes(x = obs, y = ckd, ymin = min(ckd), ymax = ckd))
	p <- p + geom_linerange(colour = 'blue')
	p <- p + geom_point(shape = 1, colour = 'blue')
	p <- p + geom_hline(yintercept = k$ts, colour = 'red')
	p <- p + xlab('Observation') + ylab("Cook's D") + ggtitle("Cook's D Chart")
	print(p)

}


# cooksd_chart <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	ckd <- cooks.distance(model)
# 	ts  <- 4 / length(ckd)
#
# 	plot(seq_len(length(ckd)), ckd, type = "h", lwd = 1, col = "blue",
# 	     xlab = "Observation", ylab = "Cook's D",
# 	     main = paste("Cook's D for", names(model.frame(model))[1]))
# 	points(ckd, col = "blue")
# 	abline(h = ts, col = "gray")
#
# 	outlier <- ckd[ckd > ts]
# 	obs     <- as.numeric(names(outlier))
#
# 	text(jitter(obs), as.vector(outlier), obs, cex = 0.8)
#
# 	z <- list(cooks_d   = ckd,
# 						threshold = round(ts, 4),
# 						outliers  = obs)
#
# }
