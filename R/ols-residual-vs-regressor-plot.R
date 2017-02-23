#' @title Residual vs Regressors Plot
#' @description Graph to determine whether we should add a new predictor to the model already containing other predictors.
#' The residuals from the model is regressed on the new predictor and if the plot shows non random pattern, 
#' you should consider adding the new predictor to the model.
#' @param model an object of class \code{lm}
#' @param variable new predictor to be added to the \code{model}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_rvsr_plot(model, mtcars$drat)
#' @export
#'
ols_rvsr_plot <- function(model, variable) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	x <- NULL
	y <- NULL
	d <- rvsrdata(model)

				v <- l(deparse(substitute(variable)))
				k <- data.frame(x = variable, y = model$residuals)
				p <- ggplot(k, aes(x = x, y = y))
				p <- p + geom_point(shape = 1, colour = 'blue')
				p <- p + xlab(paste(v)) + ylab('Residual')
				p <- p + ggtitle(paste("Residual vs", v))
				p <- p + geom_hline(yintercept = 0, colour = 'red')
				print(p)

}

# rvsr_plot <- function(model) {

# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }

# 	x <- NULL
# 	y <- NULL
# 	d <- rvsrdata(model)

# 		for (i in seq_len(d$np)) {
# 				k <- data.frame(x = unlist(d$dat[i]), y = model$residuals)
# 				p <- ggplot(k, aes(x = x, y = y))
# 				p <- p + geom_point(shape = 1, colour = 'blue')
# 				p <- p + xlab(paste(d$pnames[i])) + ylab('Residual')
# 				p <- p + ggtitle(paste("Residual vs", d$pnames[i]))
# 				p <- p + geom_hline(yintercept = 0, colour = 'red')
# 				print(p)
# 		}

# }


# rvsr_plot <- function(model, panel = TRUE) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
#
# 	np     <- length(model$coefficients) - 1
# 	dat    <- model.frame(model)[-1]
# 	pnames <- names(model$coefficients)[-1]
# 	dnames <- colnames(model.frame(model))[1]
#
# 	if (panel) {
#
# 		ncols  <- 2
# 		nrows  <- ceiling(np / 2 )
# 		op     <- par(no.readonly = TRUE)
#
# 		on.exit(par(op))
# 		par(mfrow=c(nrows,ncols), oma = c(0, 0, 2, 0))
# 		for (i in seq_len(np)) {
# 		    plot(unlist(dat[i]), model$residuals, col = "blue",
# 		         xlab = paste(pnames[i]), ylab = "Residual")
# 		    abline(h = 0)
# 		}
# 		mtext(paste("Residual by Regressors for", dnames), outer = T)
#
# 	} else {
#
# 		for (i in seq_len(np)) {
# 		    plot(unlist(dat[i]), model$residuals, col = "blue",
# 		         xlab = paste(pnames[i]), ylab = "Residual",
# 		         main = paste("Residual vs", pnames[i]))
# 		    abline(h = 0)
# 		}
#
# 	}
#
# }
