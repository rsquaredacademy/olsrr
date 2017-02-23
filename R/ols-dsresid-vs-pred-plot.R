#' @importFrom stats fitted rstudent
#' @importFrom dplyr mutate
#' @title Deleted Studentized Residual vs Fitted Values Plot
#' @description Deleted Studentized Residual vs Fitted Values Plot
#' @param model an object of class \code{lm}
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_dsrvsp_plot(model)
#' @export
#'
ols_dsrvsp_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	dsr <- NULL
	pred <- NULL
	Observation <- NULL
	k <- dpred(model)
	d <- k$ds
	p <- ggplot(d, aes(x = pred, y = dsr))
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
