#' @importFrom stats model.frame residuals lm as.formula
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab stat_smooth
#' @importFrom magrittr %>%
#' @title Added Variable Plot
#' @description Show the effect of adding another variable to a model already
#' having one or more independent variables.
#' @param model an object of class \code{lm}
#' @details The added variable plot was introduced by Mosteller and Tukey (1977). It enables
#' us to visualize the regression coefficient of a new variable being considered to
#' be included in a model. The plot can be constructed for each predictor variable.
#'
#' Let us assume we want to test the effect of adding/removing variable *X* from a
#' model. Let the response variable of the model be *Y*
#'
#' Steps to construct an added variable plot:
#'
#' - Regress *Y* on all variables other than *X* and store the residuals (*Y* residuals).
#' - Regress *X* on all the other variables inlcuded in the model (*X* residuals).
#' - Construct a scatter plot of *Y* residuals and *X* residuals.
#'
#' What do the *Y* and *X* residuals represent? The *Y* residuals represent the part
#' of **Y** not explained by all the variables other than X. The *X* residuals
#' represent the part of **X** not explained by other variables. The slope of the line
#' fitted to the points in the added variable plot is equal to the regression
#' coefficient when **Y** is regressed on all variables including **X**.
#'
#' A strong linear relationship in the added variable plot indicates the increased
#' importance of the contribution of **X** to the model already containing the
#' other predictors.
#' @references Regression Analysis by Example, 5th Edition, Samprit Chatterjee
#' and Ali S Hadi, 2012, John Wiley & Sons
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
