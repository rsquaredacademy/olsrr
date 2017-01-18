#' @importFrom ggplot2 geom_linerange
#' @title Cooks' d Chart
#' @description Cooks' d Chart
#' @param model an object of class \code{lm}
#' @return \code{cooksd_chart} returns a list containing the
#' following components:
#'
#' \item{cooks_d}{f cook's d statistic}
#' \item{threshold}{threshold for outliers}
#' \item{outliers}{residual points that are outliers}
#' @export
#'
cooksd_chart <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	k <- cdchart(model)
	p <- ggplot(k$d, aes(obs, ckd, ymin = min(ckd), ymax = ckd))
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
