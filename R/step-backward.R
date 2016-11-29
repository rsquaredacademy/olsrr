step_backward <- function(model, prem = 0.3, details = FALSE) UseMethod('step_backward')

step_backward.default <- function(model, prem = 0.3, details = FALSE) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS regression model.', call. = FALSE)
    }

    if ((prem < 0) | (prem > 1)) {
      stop('penter must be between 0 and 1.', call. = FALSE)
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
			message(paste("No more variables satisfy the condition of prem:", penter))
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


print.step_backward <- function(data) {

    print_step_backward(data)

}



# plot method
plot.step_backward <- function(data) {

    x        <- seq_len(data$steps)
    rmax     <- max(data$rsquare)
    rstep    <- which(data$rsquare == rmax)
    adjrmax  <- max(data$adjr)
    adjrstep <- which(data$adjr == adjrmax)
    cpdiff   <- data$mallows_cp - x
    cpdifmin <- min(cpdiff)
    cpdifi   <- which(cpdiff == cpdifmin)
    cpval    <- data$mallows_cp[cpdifi]
    aicmin   <- min(data$aic)
    aicstep  <- which(data$aic == aicmin)
    sbicmin  <- min(data$sbic)
    sbicstep <- which(data$sbic == sbicmin)
    sbcmin   <- min(data$sbc)
    sbcstep  <- which(data$sbc == sbcmin)

    op <- par(no.readonly = TRUE)
    on.exit(par(op))

    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
    layout(mat = m,heights = c(2, 2))

    plot(x, data$rsquare, type = 'b', col = 'blue', xlab = '', ylab = '',
     main = 'R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
    points(rstep, rmax, pch = 2, col = "red", cex = 2.5)

    plot(x, data$adjr, type = 'b', col = 'blue', xlab = '', ylab = '', 
        main = 'Adj. R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
    points(adjrstep, adjrmax, pch = 2, col = "red", cex = 2.5)

    plot(x, data$mallows_cp, type = 'b', col = 'blue', xlab = '', ylab = '', 
        main = 'C(p)', cex.main = 1, axes = FALSE, frame.plot = T)
    points(cpdifi, cpval, pch = 2, col = "red", cex = 2.5)

    plot(x, data$aic, type = 'b', col = 'blue', xlab = 'Step', ylab = '', 
        main = 'AIC', cex.main = 1, yaxt = 'n')
    points(aicstep, aicmin, pch = 2, col = "red", cex = 2.5)

    plot(x, data$sbic, type = 'b', col = 'blue', xlab = 'Step', ylab = '', 
        main = 'SBIC', cex.main = 1, yaxt = 'n')
    points(sbicstep, sbicmin, pch = 2, col = "red", cex = 2.5)

    plot(x, data$sbc, type = 'b', col = 'blue', xlab = 'Step', ylab = '', 
        main = 'SBC', cex.main = 1, yaxt = 'n')
    points(sbcstep, sbcmin, pch = 2, col = "red", cex = 2.5)
   
    
}














