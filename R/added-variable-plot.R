#' @importFrom stats model.frame residuals lm as.formula
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab stat_smooth
#' @importFrom magrittr %>%
#' @title Added Variable Plot
#' @description Show the effect of adding another variable to a model already
#' having one or more independent variables.
#' @param model an object of class \code{lm}
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' addvar_plot(model)
#' @export
#'
addvar_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	data   <- model.frame(model)
	xnames <- colnames(data)
	nl     <- ncol(data)
	dat2   <- data[-1]

	for(i in 2:nl) {

			x <- advarx(dat2, i)
			y <- advary(data, i)
			d <- tibble(x, y)
			p <- ggplot(d, aes(x = x, y = y)) +
				geom_point(colour = 'blue', size = 2) +
				xlab(paste(xnames[i], " | Others")) +
				ylab(paste(xnames[1], " | Others")) +
				stat_smooth(method="lm", se=FALSE)

			print(p)

	}

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
