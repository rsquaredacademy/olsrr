#' @importFrom stats model.frame residuals lm as.formula
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab stat_smooth
#' @importFrom magrittr %>%
#' @title Added Variable Plot
#' @description Added variable plot provides information about the marginal importance of a 
#' predictor variable, given the other predictor variables already in 
#' the model. It shows the marginal importance of the variable in reducing the
#' residual variability. 
#' @param model an object of class \code{lm}
#' @references Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition). 
#' Chicago, IL., McGraw Hill/Irwin.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' addvar_plot(model)
#' @export
#'
addvar_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	# data   <- model.frame(model)
	     m1 <- tibble::as_data_frame(model.frame(model))
	     m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
	   data <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
	 xnames <- colnames(data)
	     nl <- ncol(data)
	   dat2 <- data[-1]
	myplots <- list()

	for(i in 2:nl) {

			x <- advarx(dat2, i)
			y <- advary(data, i)
			d <- tibble(x, y)
			p <- eval(substitute(ggplot(d, aes(x = x, y = y)) +
				geom_point(colour = 'blue', size = 2) +
				xlab(paste(xnames[i], " | Others")) +
				ylab(paste(xnames[1], " | Others")) +
				stat_smooth(method="lm", se=FALSE), list(i = i)))

			print(p)
			j <- i - 1
			myplots[[j]] <- p

	}

	do.call(grid.arrange, c(myplots, list(ncol = 2)))

}

# addvar_plot <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	data   <- model.frame(model)
# 	xnames <- colnames(data)
# 	nl     <- ncol(data)
# 	dat2   <- data[-1]
#
# 	for(i in 2:nl) {
#
# 	    j    <- i - 1
# 	    dat  <- data[-i]
# 	    k    <- names(dat)
# 	    fla  <- as.formula(paste(k[1], "~ ."))
# 	    y    <- residuals(lm(fla, data = dat))
# 	    k2   <- names(dat2)
# 	    n    <- length(k2)
# 	    fla2 <- as.formula(paste(k2[j], "~ ."))
# 	    x    <- residuals(lm(fla2, data = dat2))
#
# 	    plot(x, y, col = "blue",
# 	         xlab = paste(xnames[i], " | Others"),
# 	         ylab = paste(xnames[1], " | Others"))
# 	    abdat <- data.frame(y, x)
# 	    abline(lm(y ~ x, data = abdat), col = "gray")
# 	}
#
# }
