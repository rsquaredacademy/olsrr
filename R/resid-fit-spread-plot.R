#' @importFrom stats ecdf
#' @importFrom gridExtra grid.arrange
#' @title Residual Fit Spread Plot
#' @description Residual Fit Spread Plot
#' @param model an object of class \code{lm}
#' @return \code{rfs_plot} returns a list containing the
#' following components:
#'
#' \item{residuals}{f cook's d statistic}
#' \item{fitted_values}{f cook's d statistic}
#' \item{avg_fitted}{threshold for outliers}
#' \item{dev_fitted}{f cook's d statistic}
#' \item{ecdf_avg_fitted}{threshold for outliers}
#' \item{ecdf_resid}{f cook's d statistic}
#' @export
#'
rfs_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	predicted  <- fitted.values(model)
	pred_m     <- mean(predicted)
	pred_s     <- predicted - pred_m
	percentile <- ecdf(pred_s)
	ymin1       <- min(pred_s) + (0.25 * min(pred_s))
	ymax1       <- max(pred_s) + (0.25 * max(pred_s))

	d1 <- data.frame(x = percentile(pred_s), y = pred_s)

	p1 <- ggplot(d1, aes(x = x, y = y))
	p1 <- p1 + geom_point(shape = 1, color = 'blue')
	p1 <- p1 + xlim(c(-0.2, 1.2)) + ylim(c(ymin1, ymax1))
	p1 <- p1 + xlab('Proportion Less') + ylab('')
	p1 <- p1 + ggtitle('Fit - Mean')



	resid     <- residuals(model)
	residtile <- ecdf(resid)
	ymin2      <- min(resid) + (0.25 * min(resid))
	ymax2      <- max(resid) + (0.25 * max(resid))
	d2         <- data.frame(x = residtile(resid), y = resid)

	p2 <- ggplot(d2, aes(x = x, y = y))
	p2 <- p2 + geom_point(color = 'blue', shape = 1)
	p2 <- p2 + ylim(c(ymin2, ymax2)) + xlim(c(-0.2, 1.2))
	p2 <- p2  + xlab('Proportion Less') + ylab('')
	p2 <- p2 + ggtitle('Residual')

	grid.arrange(p1, p2, ncol = 2)

}


#' @importFrom stats ecdf
#' @importFrom graphics axis
#' @title Fit Mean Plot
#' @description Fit Mean Plot
#' @param model an object of class \code{lm}
#' @return \code{fm_plot} returns a list containing the
#' following components:
#'
#' \item{fitted_values}{f cook's d statistic}
#' \item{avg_fitted}{threshold for outliers}
#' \item{dev_fitted}{f cook's d statistic}
#' \item{ecdf_avg_fitted}{threshold for outliers}
#' @export
#'
fm_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	predicted  <- fitted.values(model)
	pred_m     <- mean(predicted)
	pred_s     <- predicted - pred_m
	percentile <- ecdf(pred_s)
	ymin       <- min(pred_s) + (0.25 * min(pred_s))
	ymax       <- max(pred_s) + (0.25 * max(pred_s))

	d <- data.frame(x = percentile(pred_s), y = pred_s)

	p <- ggplot(d, aes(x = x, y = y))
	p <- p + geom_point(shape = 1, color = 'blue')
	p <- p + xlim(c(-0.2, 1.2)) + ylim(c(ymin, ymax))
	p <- p + xlab('Proportion Less') + ylab('Fit - Mean')
	p <- p + ggtitle('Residual Fit Spread Plot')
	print(p)

	# z <- list(fitted_values   = predicted,
	# 	        avg_fitted      = pred_m,
	# 	        dev_fitted      = pred_s,
	# 	        ecdf_avg_fitted = percentile)

}


#' @title Residual Spread Plot
#' @description Residual Spread Plot
#' @param model an object of class \code{lm}
#' @return \code{rsd_plot} returns a list containing the
#' following components:
#'
#' \item{residuals}{f cook's d statistic}
#' \item{ecdf_resid}{threshold for outliers}
#' @export
#'
rsd_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	resid     <- residuals(model)
	residtile <- ecdf(resid)
	ymin      <- min(resid) + (0.25 * min(resid))
	ymax      <- max(resid) + (0.25 * max(resid))
	d         <- data.frame(x = residtile(resid), y = resid)

	p <- ggplot(d, aes(x = x, y = y))
	p <- p + geom_point(color = 'blue', shape = 1)
	p <- p + ylim(c(ymin, ymax)) + xlim(c(-0.2, 1.2))
	p <- p + ylab('Residual') + xlab('Proportion Less')
	p <- p + ggtitle('Residual Fit Spread Plot')
	print(p)

	# z <- list(residuals  = resid,
	# 	        ecdf_resid = residtile)

}


# rsd_plot <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	resid     <- residuals(model)
# 	residtile <- ecdf(resid)
# 	ymin      <- min(resid) + (0.25 * min(resid))
# 	ymax      <- max(resid) + (0.25 * max(resid))
# 	d         <- data.frame(x = residtile(resid), y = resid)
#
# 	p <- ggplot(d, aes(x = x, y = y))
# 	p <- p + geom_point(color = 'blue', shape = 1)
# 	p <- p + ylim(c(ymin, ymax)) + xlim(c(-0.2, 1.2))
# 	p <- p + ylab('Residual') + xlab('Proportion Less')
# 	p <- p + ggtitle('Residual Fit Spread Plot')
#
# 	plot(residtile(resid), resid,
# 		   ylim   = c(ymin, ymax),
# 		   col    = "blue",
# 		   xaxt   = "n",
# 		   xlim   = c(-0.2, 1.2),
# 		   ylab   = "Residual",
# 		   xlab   = "Proportion Less",
# 		   main   = "Residual Fit Spread Plot",
# 		   yaxt   = "n")
#
# 	axis(side   = 1,
# 		   at     = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
# 	     labels = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
#
# 	z <- list(residuals  = resid,
# 		        ecdf_resid = residtile)
#
# }


# fm_plot <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	predicted  <- fitted.values(model)
# 	pred_m     <- mean(predicted)
# 	pred_s     <- predicted - pred_m
# 	percentile <- ecdf(pred_s)
# 	ymin       <- min(pred_s) + (0.25 * min(pred_s))
# 	ymax       <- max(pred_s) + (0.25 * max(pred_s))
#
#
# 	plot(percentile(pred_s), pred_s,
# 		   ylim   = c(ymin, ymax),
# 		   col    = "blue",
# 		   xaxt   = "n",
# 		   xlim   = c(-0.2, 1.2),
# 		   ylab   = "Fit - Mean",
# 		   xlab   = "Proportion Less",
# 		   main   = "Residual Fit Spread Plot")
#
# 	axis(side   = 1,
# 		   at     = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
# 	     labels = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
#
# 	z <- list(fitted_values   = predicted,
# 		        avg_fitted      = pred_m,
# 		        dev_fitted      = pred_s,
# 		        ecdf_avg_fitted = percentile)
#
# }


# rfs_plot <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	predicted  <- fitted.values(model)
# 	resid      <- residuals(model)
# 	pred_m     <- mean(predicted)
# 	pred_s     <- predicted - pred_m
# 	percentile <- ecdf(pred_s)
# 	residtile  <- ecdf(resid)
# 	ymin       <- min(pred_s) + (0.25 * min(pred_s))
# 	ymax       <- max(pred_s) + (0.25 * max(pred_s))
# 	op         <- par(no.readonly = TRUE)
# 	on.exit(par(op))
#
# 	par(mfrow   = c(1, 2))
#
# 	plot(percentile(pred_s), pred_s,
# 			 ylim   = c(ymin, ymax),
# 			 col    = "blue",
# 			 xaxt   = "n",
# 	     xlim   = c(-0.2, 1.2),
# 	     ylab   = "",
# 	     xlab   = "",
# 	     main   = "Fit - Mean")
#
# 	axis(side   = 1,
# 		   at     = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
# 	     labels = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
#
# 	mtext("Proportion Less",
# 		    side  = 1,
# 		    line  = 3,
# 		    at    = 1.4)
#
# 	plot(residtile(resid), resid,
# 		   ylim   = c(ymin, ymax),
# 		   col    = "blue",
# 		   xaxt   = "n",
# 	     xlim   = c(-0.2, 1.2),
# 	     ylab   = "",
# 	     xlab   = "",
# 	     main   = "Residual",
# 	     yaxt   = "n")
#
# 	axis(side   = 1,
# 		   at     = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0),
# 	     labels = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
#
# 	z <- list(residuals       = resid,
# 						fitted_values   = predicted,
# 		        avg_fitted      = pred_m,
# 		        dev_fitted      = pred_s,
# 		        ecdf_avg_fitted = percentile,
# 		        ecdf_resid      = residtile)
# }
