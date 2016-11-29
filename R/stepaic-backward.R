stepaic_backward <- function(model, details = FALSE) UseMethod('stepaic_backward')

stepaic_backward.default <- function(model, details = FALSE) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }
    
    if (!is.logical(details)) {
      stop('details must be either TRUE or FALSE', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    l        <- model.frame(model)
    nam      <- names(l)
    response <- nam[1]
    preds    <- nam[-1]
    aic_f    <- round(aic(model), 3)
    mi       <- regress(paste(response, '~', paste(preds, collapse = ' + ')), data = l)
    rss_f    <- mi$rss
    laic     <- aic_f
    lrss     <- rss_f
    less     <- mi$ess
    lrsq     <- mi$rsq
    larsq    <- mi$adjr

    if (details == TRUE) {
        cat(' Step 0: AIC =', aic_f, '\n', paste(response, '~', paste(preds, collapse = ' + '), '\n\n'))
    }

    ilp   <- length(preds)
    end   <- FALSE
    step  <- 0
    rpred <- c()
    aics  <- c()
    ess   <- c()
    rss   <- c()
    rsq   <- c()
    arsq  <- c()

        for (i in seq_len(ilp)) {
            
            predictors <- preds[-i]
            m          <- regress(paste(response, '~', paste(predictors, collapse = ' + ')), data = l)
            aics[i]    <- round(aic(m$model), 3)
            ess[i]     <- round(m$ess, 3)
            rss[i]     <- round(rss_f - m$rss, 3)
            rsq[i]     <- round(m$rsq, 3)
            arsq[i]    <- round(m$adjr, 3)
        }

        da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
        da2 <- arrange(da, rss)

        if(details == TRUE) {
            
            w1 <- max(nchar('Predictor'), nchar(predictors))
            w2 <- 2
            w3 <- max(nchar('AIC'), nchar(format(aics, nsmall = 3)))
            w4 <- max(nchar('Sum Sq'), nchar(format(rss, nsmall = 3)))
            w5 <- max(nchar('RSS'), nchar(format(ess, nsmall = 3)))
            w6 <- max(nchar('R-Sq'), nchar(format(rsq, nsmall = 3)))
            w7 <- max(nchar('Adj. R-Sq'), nchar(format(arsq, nsmall = 3)))
            w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
            ln <- length(aics)
            
            cat(rep("-", w), sep = "", '\n')
            cat(fl('Variable', w1), fs(), fc('DF', w2), fs(), fc('AIC', w3), fs(),
                fc('Sum Sq', w4), fs(), fc('RSS', w5), fs(), fc('R-Sq', w6), fs(),
                fc('Adj. R-Sq', w7), '\n')
            cat(rep("-", w), sep = "", '\n')

            for (i in seq_len(ln)) {
                cat(fl(da2[i, 1], w1), fs(), fc(1, w2), fs(), fg(da2[i, 2], w3), fs(),
                    fg(da2[i, 4], w4), fs(), fg(da2[i, 3], w5), fs(), fg(da2[i, 5], w6), fs(),
                    fg(da2[i, 6], w7), '\n')
            }

            cat(rep("-", w), sep = "", '\n\n')
        }
    
    while (!end) {

        minc <- which(aics == min(aics))

        if (aics[minc] < aic_f) {
            
            rpred <- c(rpred, preds[minc])
            preds <- preds[-minc]
            ilp   <- length(preds)
            step  <- step + 1
            aic_f <- round(aics[minc], 3)
            mi    <- regress(paste(response, '~', paste(preds, collapse = ' + ')), data = l)
            rss_f <- mi$rss
            laic  <- c(laic, aic_f)
            lrss  <- c(lrss, rss_f)
            less  <- c(less, mi$ess)
            lrsq  <- c(lrsq, mi$rsq)
            larsq <- c(larsq, mi$adjr)
            aics  <- c()
            ess   <- c()
            rss   <- c()
            rsq   <- c()
            arsq  <- c()

            for (i in seq_len(ilp)) {
                    
                    predictors <- preds[-i]
                    m          <- regress(paste(response, '~', paste(predictors, collapse = ' + ')), data = l)
                    aics[i]    <- round(aic(m$model), 3)
                    ess[i]     <- round(m$ess, 3)
                    rss[i]     <- round(rss_f - m$rss, 3)
                    rsq[i]     <- round(m$rsq, 3)
                    arsq[i]    <- round(m$adjr, 3)
                }
            
            
            if (details == TRUE) {
                cat(' Step', step, ': AIC =', aic_f, '\n', paste(response, '~', paste(preds, collapse = ' + '), '\n\n'))
                

                da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
                da2 <- arrange(da, rss)
                w1  <- max(nchar('Predictor'), nchar(predictors))
                w2  <- 2
                w3  <- max(nchar('AIC'), nchar(format(aics, nsmall = 3)))
                w4  <- max(nchar('Sum Sq'), nchar(format(rss, nsmall = 3)))
                w5  <- max(nchar('RSS'), nchar(format(ess, nsmall = 3)))
                w6  <- max(nchar('R-Sq'), nchar(format(rsq, nsmall = 3)))
                w7  <- max(nchar('Adj. R-Sq'), nchar(format(arsq, nsmall = 3)))
                w   <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
                ln  <- length(aics)
                    
                cat(rep("-", w), sep = "", '\n')
                cat(fl('Variable', w1), fs(), fc('DF', w2), fs(), fc('AIC', w3), fs(),
                    fc('Sum Sq', w4), fs(), fc('RSS', w5), fs(), fc('R-Sq', w6), fs(),
                    fc('Adj. R-Sq', w7), '\n')
                cat(rep("-", w), sep = "", '\n')

                for (i in seq_len(ln)) {
                    cat(fl(da2[i, 1], w1), fs(), fc(1, w2), fs(), fg(da2[i, 2], w3), fs(),
                        fg(da2[i, 4], w4), fs(), fg(da2[i, 3], w5), fs(), fg(da2[i, 5], w6), fs(),
                        fg(da2[i, 6], w7), '\n')
                }
                
                cat(rep("-", w), sep = "", '\n\n')
                
            }
            
        } else {

            end <- TRUE

            if (details == TRUE) {
                message(paste("No more variables to be removed."))    
            }
            
        }

    }
    
    out <- list(steps      = step, 
                predictors = rpred, 
                aics       = laic, 
                ess        = less, 
                rss        = lrss, 
                rsq        = lrsq, 
                arsq       = larsq)

    class(out) <- 'stepaic_backward'
    
    return(out)


}


print.stepaic_backward <- function(data) {

    print_stepaic_backward(data)

}


plot.stepaic_backward <- function(data) {

    x          <- c(0, seq_len(data$steps))
    xloc       <- x - 0.1
    yloc       <- data$aics - 0.2
    xmin       <- min(x) - 0.4
    xmax       <- max(x) + 1
    ymin       <- min(data$aics) - 1
    ymax       <- max(data$aics) + 1
    predictors <- c('Full Model', data$predictors)

    plot(x, data$aics, 
        type = "b", 
        col  = "blue", 
        xlab = "Steps", 
        ylab = "AIC",
        xlim = c(xmin, xmax), 
        ylim = c(ymin, ymax),
        main = "Step AIC: Backward Elimination")

    text(xloc, yloc, predictors, 
        col = "red", 
        cex = 0.9)


}












    
