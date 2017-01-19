#' @importFrom stats fitted rstudent
#' @importFrom dplyr mutate
#' @title Deleted Studentized Residual vs Predictor Plot
#' @description Deleted Studentized Residual vs Predictor Plot
#' @param model an object of class \code{lm}
#' @return \code{dsrvsp_plot} returns a list containing the
#' following components:
#'
#' \item{fitted}{fitted values of the regression model}
#' \item{dstudresid}{deleted studentized residuals}
#' \item{threshold}{threshold for outliers}
#' \item{normal}{normal residual points}
#' \item{outliers}{residual points that are outliers}
#' @export
#'
dsrvsp_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	k <- dpred(model)
	p <- ggplot(k$ds, aes(x = pred, y = dsr))
	p <- p + geom_point(aes(colour = Observation))
	p <- p + scale_color_manual(values = c('blue', 'red'))
	p <- p + ylim(k$cminx, k$cmaxx)
	p <- p + xlab('Predicted Value') + ylab('Deleted Studentized Residual')
	p <- p + ggtitle("Deleted Studentized Residual vs Predicted Values")
	p <- p + geom_hline(yintercept = c(-2, 2), colour = 'red')
	print(p)

}


# dsrvsp_plot <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	pred    <- fitted(model)
# 	dsresid <- unname(rstudent(model))
# 	n       <- length(dsresid)
#
# 	dsr   <- data.frame(obs = seq_len(n), dsr = dsresid)
#
# 	dsr <- dsr %>%
#     mutate(color = ifelse((abs(dsr) >= 2), "outlier", "normal"))
#
# 	dsr$color1 <- factor(dsr$color)
# 	dsr$color2 <- ordered(dsr$color1, levels = c("normal", "outlier"))
# 	color      <- c("blue", "red")
# 	minx       <- min(dsr$dsr) - 1
# 	maxx       <- max(dsr$dsr) + 1
# 	cminx   <- ifelse(minx < -2, minx, -2.5)
# 	cmaxx   <- ifelse(maxx > 2, maxx, 2.5)
#
# 	plot(pred, dsr$dsr,
# 		   col = color[dsr$color2],
# 	     xlab = "Predicted Value",
# 	     ylab = "RStudent",
# 	     ylim = c(cminx, cmaxx),
# 	     main = "Deleted Studentized Residual vs Predicted Values")
#
# 	abline(h  = c(-2, 2),
# 		    col = "gray")
#
# 	z <- list(fitted     = pred,
# 				    dstudresid = dsresid,
# 				    threshold  = 2,
# 				    normal     = dsr$obs[dsr$color == "normal"],
# 		        outlier    = dsr$obs[dsr$color == "outlier"])
# }
