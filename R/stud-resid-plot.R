#' @importFrom ggplot2 scale_fill_manual
#' @title Studentized Residual Plot
#' @description Studentized Residual Plot
#' @param model an object of class \code{lm}
#' @return \code{srplot} returns a list containing the
#' following components:
#'
#' \item{dstudresid}{f cook's d statistic}
#' \item{threshold}{threshold for outliers}
#' \item{normal}{threshold for outliers}
#' \item{outlier}{f cook's d statistic}
#' @export
#'
srplot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	obs <- NULL
	dsr <- NULL
	Observation <- NULL
	g <- srdata(model)
	d <- g$dsr
	p <- ggplot(d, aes(x = obs, y = dsr))
	p <- p + geom_bar(width = 0.5, stat = 'identity', aes(fill = Observation))
	p <- p + scale_fill_manual(values = c('blue', 'red'))
	p <- p + ylim(g$cminx, g$cmaxx)
	p <- p + coord_flip()
	p <- p + xlab('Observation') + ylab('Deleted Studentized Residuals')
	p <- p + ggtitle('Studentized Residuals')
	p <- p + geom_hline(yintercept = c(g$cminx, g$cmaxx), color = 'red')
	p <- p + geom_hline(yintercept = c(0, g$nseq, g$pseq))
	print(p)

}

# srplot <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	dstud <- unname(rstudent(model))
# 	n     <- length(dstud)
# 	dsr   <- data.frame(obs = seq_len(n), dsr = dstud)
#
# 	dsr <- dsr %>%
#         mutate(color = ifelse((abs(dsr) >= 3), "outlier", "normal"))
#
# 	dsr$color1 <- factor(dsr$color)
# 	dsr$color2 <- ordered(dsr$color1, levels = c("normal", "outlier"))
# 	color      <- c(NA, "red")
# 	minx       <- min(dsr$dsr) - 1
# 	maxx       <- max(dsr$dsr) + 1
# 	ln         <- length(dsr$dsr)
#
# 	barplot(dsr$dsr, col = color[dsr$color2],
# 	        axes = T, xlim = c(minx, maxx), width = 0.5,
# 	        space = 1, horiz = T, names.arg = seq_len(ln),
# 	        cex.names = 0.5, main = "Studentized Residuals",
# 	        xlab = "Deleted Studentized Residuals",
# 	        ylab = "Observation")
# 	abline(v = 0)
# 	abline(v = ceiling(minx))
# 	abline(v = floor(maxx))
#
# 	cminx <- ceiling(minx)
# 	cmaxx <- floor(maxx)
# 	nseq  <- seq_len(abs(0 + cminx + 1)) * -1
# 	pseq  <- seq_len(0 + cmaxx - 1)
# 	abline(v = nseq, lwd = 0.1, col = "gray")
# 	abline(v = pseq, lwd = 0.1, col = "gray")
#
# 	z <- list(dstudresid = dsr$dsr,
# 		        threshold  = 3,
# 		        normal     = dsr$obs[dsr$color == "normal"],
# 		        outlier    = dsr$obs[dsr$color == "outlier"])
#
# }
