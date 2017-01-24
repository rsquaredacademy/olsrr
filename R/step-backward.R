#' @title Stepwise Backward Regression
#' @description Stepwise Backward Regression
#' @param model an object of class \code{lm}
#' @param ... other inputs
#' @return \code{step_backward} returns an object of class \code{"step_backward"}.
#' An object of class \code{"step_backward"} is a list containing the
#' following components:
#'
#' \item{steps}{f statistic}
#' \item{removed}{p value of \code{score}}
#' \item{rsquare}{degrees of freedom}
#' \item{aic}{fitted values of the regression model}
#' \item{sbc}{name of explanatory variables of fitted regression model}
#' \item{sbic}{response variable}
#' \item{adjr}{predictors}
#' \item{rmse}{predictors}
#' \item{mallows_cp}{predictors}
#' \item{indvar}{predictors}
#' @export
#'
step_backward <- function(model, ...) UseMethod('step_backward')

#' @export
#'
step_backward.default <- function(model, prem = 0.3, details = FALSE, ...) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS regression model.', call. = FALSE)
    }

    if ((prem < 0) | (prem > 1)) {
      stop('prem must be between 0 and 1.', call. = FALSE)
    }

    if (!is.logical(details)) {
      stop('details must be either TRUE or FALSE', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    message("We are eliminating variables based on p value...")
	l        <- model.frame(model)
	nam      <- names(l)
	response <- nam[1]
	preds    <- nam[-1]
    cterms   <- preds
	ilp      <- length(preds)
	end      <- FALSE
	step     <- 0
	rpred    <- c()
	rsq      <- c()
    adjrsq   <- c()
    aic      <- c()
    sbic     <- c()
    sbc      <- c()
    cp       <- c()
    rmse     <- c()

	while (!end) {

		m     <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
		pvals <- m$pvalues[-1]
		maxp  <- which(pvals == max(pvals))

		if (pvals[maxp] > prem) {

			step   <- step + 1
			rpred  <- c(rpred, preds[maxp])
			preds  <- preds[-maxp]
			lp     <- length(rpred)
			fr     <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
            rsq    <- c(rsq, fr$rsq)
            adjrsq <- c(adjrsq, fr$adjr)
            aic    <- c(aic, aic(fr$model))
            sbc    <- c(sbc, sbc(fr$model))
            sbic   <- c(sbic, sbic(fr$model, model))
            cp     <- c(cp, mallow_cp(fr$model, model))
            rmse   <- c(rmse, sqrt(fr$ems))

            if (details == TRUE) {

            	cat(paste("Backward Elimination: Step", step, "\n\n"), paste("Variable", rpred[lp], "Removed"), "\n\n")
                m <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
                print(m)
                cat("\n\n")

            }

		} else {

			end <- TRUE
			message(paste("No more variables satisfy the condition of prem:", prem))
		}

	}

	out <- list(steps      = step,
                removed    = rpred,
                rsquare    = rsq,
                aic        = aic,
                sbc        = sbc,
                sbic       = sbic,
                adjr       = adjrsq,
                rmse       = rmse,
                mallows_cp = cp,
                indvar     = cterms)

	class(out) <- 'step_backward'

	return(out)

}

#' @export
#'
print.step_backward <- function(x, ...) {
    print_step_backward(x)
}



#' @export
#'
plot.step_backward <- function(x, ...) {

    y        <- seq_len(x$steps)
    rmax     <- max(x$rsquare)
    rstep    <- which(x$rsquare == rmax)
    adjrmax  <- max(x$adjr)
    adjrstep <- which(x$adjr == adjrmax)
    cpdiff   <- x$mallows_cp - y
    cpdifmin <- min(cpdiff)
    cpdifi   <- which(cpdiff == cpdifmin)
    cpval    <- x$mallows_cp[cpdifi]
    aicmin   <- min(x$aic)
    aicstep  <- which(x$aic == aicmin)
    sbicmin  <- min(x$sbic)
    sbicstep <- which(x$sbic == sbicmin)
    sbcmin   <- min(x$sbc)
    sbcstep  <- which(x$sbc == sbcmin)

    d1 <- tibble(a = y, b = x$rsquare) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
    
    d2 <- tibble(a = y, b = x$adjr) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('Adj. R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d3 <- tibble(a = y, b = x$mallows_cp) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('C(p)') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d4 <- tibble(a = y, b = x$aic) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('AIC') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d5 <- tibble(a = y, b = x$sbic) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBIC') +
    theme(
        axis.ticks = element_blank())

    d6 <- tibble(a = y, b = x$sbc) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBC') +
    theme(
        axis.ticks = element_blank())

    grid.arrange(d1, d2, d3, d4, d5, d6, ncol = 2, top = 'Stepwise Backward Regression')

}


# plot.step_backward <- function(x, ...) {
#
#     y        <- seq_len(x$steps)
#     rmax     <- max(x$rsquare)
#     rstep    <- which(x$rsquare == rmax)
#     adjrmax  <- max(x$adjr)
#     adjrstep <- which(x$adjr == adjrmax)
#     cpdiff   <- x$mallows_cp - y
#     cpdifmin <- min(cpdiff)
#     cpdifi   <- which(cpdiff == cpdifmin)
#     cpval    <- x$mallows_cp[cpdifi]
#     aicmin   <- min(x$aic)
#     aicstep  <- which(x$aic == aicmin)
#     sbicmin  <- min(x$sbic)
#     sbicstep <- which(x$sbic == sbicmin)
#     sbcmin   <- min(x$sbc)
#     sbcstep  <- which(x$sbc == sbcmin)
#
#     op <- par(no.readonly = TRUE)
#     on.exit(par(op))
#
#     m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
#     layout(mat = m,heights = c(2, 2))
#
#     plot(y, x$rsquare, type = 'b', col = 'blue', xlab = '', ylab = '',
#      main = 'R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
#     points(rstep, rmax, pch = 2, col = "red", cex = 2.5)
#
#     plot(y, x$adjr, type = 'b', col = 'blue', xlab = '', ylab = '',
#         main = 'Adj. R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
#     points(adjrstep, adjrmax, pch = 2, col = "red", cex = 2.5)
#
#     plot(y, x$mallows_cp, type = 'b', col = 'blue', xlab = '', ylab = '',
#         main = 'C(p)', cex.main = 1, axes = FALSE, frame.plot = T)
#     points(cpdifi, cpval, pch = 2, col = "red", cex = 2.5)
#
#     plot(y, x$aic, type = 'b', col = 'blue', xlab = 'Step', ylab = '',
#         main = 'AIC', cex.main = 1, yaxt = 'n')
#     points(aicstep, aicmin, pch = 2, col = "red", cex = 2.5)
#
#     plot(y, x$sbic, type = 'b', col = 'blue', xlab = 'Step', ylab = '',
#         main = 'SBIC', cex.main = 1, yaxt = 'n')
#     points(sbicstep, sbicmin, pch = 2, col = "red", cex = 2.5)
#
#     plot(y, x$sbc, type = 'b', col = 'blue', xlab = 'Step', ylab = '',
#         main = 'SBC', cex.main = 1, yaxt = 'n')
#     points(sbcstep, sbcmin, pch = 2, col = "red", cex = 2.5)
#
#
# }
