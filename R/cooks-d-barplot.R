#' @importFrom stats cooks.distance
#' @importFrom ggplot2 geom_bar coord_flip ylim geom_hline
#' @title Cooks' d Bar Plot
#' @description Cooks' d Bar Plot
#' @param model an object of class \code{lm}
#' @return \code{cooksd_bplot} returns a list containing the
#' following components:
#'
#' \item{cooks_d}{f cook's d statistic}
#' \item{threshold}{threshold for outliers}
#' \item{normal}{normal residual points}
#' \item{outlier}{residual points that are outliers}
#' @export
#'
cooksd_bplot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	obs <- NULL
	cd <- NULL
	Observation <- NULL
	k <- cdplot(model)
	d <- k$ckd
	p <- ggplot(d, aes(x = obs, y = cd)) + geom_bar(width = 0.5, stat = 'identity', aes(fill = Observation))
	p <- p + coord_flip()
	p <- p + ylim(0, k$maxx)
	p <- p + ylab("Cook's D") + xlab('Observation') + ggtitle("Cook's D Bar Plot")
	p <- p + geom_hline(yintercept = 0)
	p <- p + geom_hline(yintercept = k$ts, colour = 'red')
	print(p)

}


# cooksd_bplot <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	cooksd <- cooks.distance(model)
# 	n      <- length(cooksd)
# 	ckd    <- data.frame(obs = seq_len(n), cd = cooksd)
# 	ts     <- 4 / length(ckd$cd)
#
# 	ckd$color <- ifelse(ckd$cd >= ts, c("outlier"), c("normal"))
# 	# ckd <- ckd %>%
# 	#     mutate(color = ifelse((cd >= ts), "outlier", "normal"))
#
# 	ckd$color1 <- factor(ckd$color)
# 	ckd$color2 <- ordered(ckd$color1, levels = c("normal", "outlier"))
# 	color      <- c(NA, "blue")
# 	maxx       <- max(ckd$cd) + 0.1
# 	ln         <- length(ckd$cd)
#
# 	barplot(ckd$cd, col = color[ckd$color2], axes = T, xlim = c(0, maxx),
# 	        width = 0.5, space = 1, horiz = T, names.arg = seq_len(ln),
# 	        cex.names = 0.5, main = "Cook's D",
# 	        xlab = "Cook's D",
# 	        ylab = "Observation")
# 	abline(v = 0)
# 	abline(v = ts, lwd = 0.1, col = "gray")
#
# 	z <- list(cooks_d   = ckd$cd,
# 					  threshold = round(ts, 4),
# 					  normal    = ckd$obs[ckd$color == "normal"],
# 					  outlier   = ckd$obs[ckd$color == "outlier"])
# }
