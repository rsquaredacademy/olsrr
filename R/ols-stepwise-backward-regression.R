#' @title Stepwise Backward Regression
#' @description Build regression model from a set of candidate predictor variables by removing predictors based on 
#' p values, in a stepwise manner until there is no variable left to remove any more.
#' @param model an object of class \code{lm}; the model should include all candidate predictor variables
#' @param prem p value; variables with p more than \code{prem} will be removed from the model
#' @param details logical; if \code{TRUE}, will print the regression result at each step
#' @param x an object of class \code{ols_step_backward}
#' @param ... other inputs
#' @return \code{ols_step_backward} returns an object of class \code{"ols_step_backward"}.
#' An object of class \code{"ols_step_backward"} is a list containing the
#' following components:
#'
#' \item{steps}{total number of steps}
#' \item{removed}{variables removed from the model}
#' \item{rsquare}{coefficient of determination}
#' \item{aic}{akaike information criteria}
#' \item{sbc}{bayesian information criteria}
#' \item{sbic}{sawa's bayesian information criteria}
#' \item{adjr}{adjusted r-square}
#' \item{rmse}{root mean square error}
#' \item{mallows_cp}{mallow's Cp}
#' \item{indvar}{predictors}
#'
#' @references Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @examples
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward(model)
#'
#' # stepwise backward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_backward(model)
#' plot(k)
#'
#' @export
#'
ols_step_backward <- function(model, ...) UseMethod('ols_step_backward')

#' @export
#' @rdname ols_step_backward
#'
ols_step_backward.default <- function(model, prem = 0.3, details = FALSE, ...) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS regression model.', call. = FALSE)
    }

    if ((prem < 0) | (prem > 1)) {
      stop('p value for removing variables from the model must be between 0 and 1.', call. = FALSE)
    }

    if (!is.logical(details)) {
      stop('details must be either TRUE or FALSE', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    message("We are eliminating variables based on p value...")
	l        <- mod_sel_data(model)
	nam      <- colnames(attr(model$terms, 'factors'))
    response <- names(model$model)[1]
    preds    <- nam
    # nam      <- names(l)
	# response <- nam[1]
	# preds    <- nam[-1]
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

		m     <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
		pvals <- m$pvalues[-1]
		maxp  <- which(pvals == max(pvals))

        suppressWarnings(

    		if (pvals[maxp] > prem) {

    			step   <- step + 1
    			rpred  <- c(rpred, preds[maxp])
    			preds  <- preds[-maxp]
    			lp     <- length(rpred)
    			fr     <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
                rsq    <- c(rsq, fr$rsq)
                adjrsq <- c(adjrsq, fr$adjr)
                aic    <- c(aic, ols_aic(fr$model))
                sbc    <- c(sbc, ols_sbc(fr$model))
                sbic   <- c(sbic, ols_sbic(fr$model, model))
                cp     <- c(cp, ols_mallows_cp(fr$model, model))
                rmse   <- c(rmse, sqrt(fr$ems))

                if (details == TRUE) {

                	cat(paste("Backward Elimination: Step", step, "\n\n"), paste("Variable", rpred[lp], "Removed"), "\n\n")
                    m <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
                    print(m)
                    cat("\n\n")

                }

    		} else {

    			end <- TRUE
    			message(paste("No more variables satisfy the condition of prem:", prem))
    		}
        )

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

	class(out) <- 'ols_step_backward'

	return(out)

}

#' @export
#'
print.ols_step_backward <- function(x, ...) {
    if (x$steps > 0) {
      print_step_backward(x)    
    } else {
      print('No variables have been removed from the model.')
    }
    
}



#' @export
#' @rdname ols_step_backward
#'
plot.ols_step_backward <- function(x, model = NA, ...) {

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
           a <- NULL
           b <- NULL

    d1 <- tibble(a = y, b = x$rsquare)
    p1 <- ggplot(d1, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d2 <- tibble(a = y, b = x$adjr)
    p2 <- ggplot(d2, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('Adj. R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d3 <- tibble(a = y, b = x$mallows_cp)
    p3 <- ggplot(d3, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('C(p)') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d4 <- tibble(a = y, b = x$aic)
    p4 <- ggplot(d4, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('AIC') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d5 <- tibble(a = y, b = x$sbic)
    p5 <- ggplot(d5, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBIC') +
    theme(
        axis.ticks = element_blank())

    d6 <- tibble(a = y, b = x$sbc)
    p6 <- ggplot(d6, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBC') +
    theme(
        axis.ticks = element_blank())

    grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = 'Stepwise Backward Regression')

  result <- list(rsquare_plot = p1, adj_rsquare_plot = p2, mallows_cp_plot = p3,
                aic_plot = p4, sbic_plot = p5, sbc_plot = p6)
  invisible(result)

}


