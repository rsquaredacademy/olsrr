stepaic_forward <- function(model, details = FALSE) UseMethod('stepaic_forward')

stepaic_forward.default <- function(model, details = FALSE) {

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
    all_pred <- nam[-1]
    mlen_p   <- length(all_pred)
    preds    <- c()
    step     <- 1
    aics     <- c()
    ess      <- c()
    rss      <- c()
    rsq      <- c()
    arsq     <- c()
    mo       <- lm(paste(response, '~', 1), data = l)
    aic1     <- aic(mo)

    if (details == TRUE) {
        cat(' Step 0: AIC =', aic1, '\n', paste(response, '~', 1, '\n\n'))
    }

    for (i in seq_len(mlen_p)) {
        predictors <- all_pred[i]
        k          <- regress(paste(response, '~', paste(predictors, collapse = ' + ')), data = l)
        aics[i]    <- aic(k$model)  
        ess[i]     <- k$ess
        rss[i]     <- k$rss
        rsq[i]     <- k$rsq
        arsq[i]    <- k$adjr
    }

    da  <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
    da2 <- arrange(da, desc(rss))

    if(details == TRUE) {
        
        w1 <- max(nchar('Predictor'), nchar(all_pred))
        w2 <- 2
        w3 <- max(nchar('AIC'), nchar(aics))
        w4 <- max(nchar('Sum Sq'), nchar(rss))
        w5 <- max(nchar('RSS'), nchar(ess))
        w6 <- max(nchar('R-Sq'), nchar(rsq))
        w7 <- max(nchar('Adj. R-Sq'), nchar(arsq))
        w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
        ln <- length(aics)

        cat(rep("-", w), sep = "", '\n')
        cat(fl('Variable', w1), fs(), fc('DF', w2), fs(), fc('AIC', w3), fs(),
            fc('Sum Sq', w4), fs(), fc('RSS', w5), fs(), fc('R-Sq', w6), fs(),
            fc('Adj. R-Sq', w7), '\n')
        cat(rep("-", w), sep = "", '\n')

        for (i in seq_len(ln)) {
            cat(fl(da2[i, 1], w1), fs(), fg(1, w2), fs(), fg(da2[i, 2], w3), fs(),
            fg(da2[i, 4], w4), fs(), fg(da2[i, 3], w5), fs(), fg(da2[i, 5], w6), fs(),
            fg(da2[i, 6], w7), '\n')
        }

        cat(rep("-", w), sep = "", '\n')
    }      
    
    minc     <- which(aics == min(aics))
    laic     <- aics[minc]
    less     <- ess[minc]
    lrss     <- rss[minc]
    lrsq     <- rsq[minc]
    larsq    <- arsq[minc]
    preds    <- all_pred[minc]
    lpreds   <- length(preds)
    all_pred <- all_pred[-minc]
    len_p    <- length(all_pred)
    step     <- 1
    
    while (step < mlen_p) {
        
        aics <- c()
        ess  <- c()
        rss  <- c()
        rsst <- c()
        rsq  <- c()
        arsq <- c()
        mo   <- regress(paste(response, '~', paste(preds, collapse = ' + ')), data = l)
        aic1 <- aic(mo$model)

        if (details == TRUE) {
            cat('\n\n', 'Step', step, ': AIC =', aic1, '\n', paste(response, '~', paste(preds, collapse = ' + '), '\n\n'))
        }

        for (i in seq_len(len_p)) {
            predictors <- c(preds, all_pred[i])
            k          <- regress(paste(response, '~', paste(predictors, collapse = ' + ')), data = l)
            aics[i]    <- aic(k$model)
            ess[i]     <- k$ess
            rsst[i]    <- k$rss
            rss[i]     <- round(k$rss - mo$rss, 3)
            rsq[i]     <- k$rsq
            arsq[i]    <- k$adjr
        }

        if (details == TRUE) {

            da  <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
            da2 <- arrange(da, desc(rss))
            w1  <- max(nchar('Predictor'), nchar(predictors))
            w2  <- 2
            w3  <- max(nchar('AIC'), nchar(aics))
            w4  <- max(nchar('Sum Sq'), nchar(rss))
            w5  <- max(nchar('RSS'), nchar(ess))
            w6  <- max(nchar('R-Sq'), nchar(rsq))
            w7  <- max(nchar('Adj. R-Sq'), nchar(arsq))
            w   <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
            ln  <- length(aics)

            cat(rep("-", w), sep = "", '\n')
            cat(fl('Variable', w1), fs(), fc('DF', w2), fs(), fc('AIC', w3), fs(),
                fc('Sum Sq', w4), fs(), fc('RSS', w5), fs(), fc('R-Sq', w6), fs(),
                fc('Adj. R-Sq', w7), '\n')
            cat(rep("-", w), sep = "", '\n')

            for (i in seq_len(ln)) {
                cat(fl(da2[i, 1], w1), fs(), fg(1, w2), fs(), fg(da2[i, 2], w3), fs(),
                fg(da2[i, 4], w4), fs(), fg(da2[i, 3], w5), fs(), fg(da2[i, 5], w6), fs(),
                fg(da2[i, 6], w7), '\n')
            }

            cat(rep("-", w), sep = "", '\n')

        }

        minaic <- which(aics == min(aics))
        
        if (aics[minaic] < laic[lpreds]) {
            
            preds    <- c(preds, all_pred[minaic])
            minc     <- aics[minaic]
            mess     <- ess[minaic]
            mrss     <- round(rsst[minaic], 3) 
            mrsq     <- rsq[minaic]
            marsq    <- arsq[minaic]                    
            laic     <- c(laic, minc)
            less     <- c(less, mess)
            lrss     <- c(lrss, mrss)
            lrsq     <- c(lrsq, mrsq)
            larsq    <- c(larsq, marsq)
            lpreds   <- length(preds)
            all_pred <- all_pred[-minaic]
            len_p    <- length(all_pred)
            step     <- step + 1
            
        } else {
            
            if (details == TRUE) {
                message("No more variables to be added.\n\n")
            }
            
            break
            
        }
        
        
    }
    
    if(details == TRUE) {

        fi <- regress(paste(response, '~', paste(preds, collapse = ' + ')), data = l)
        print(fi)

    }

    out <- list(steps = step, 
                predictors = preds, 
                aics = laic, 
                ess = less, 
                rss = lrss,  
                rsq = lrsq, 
                arsq = larsq)

    class(out) <- 'stepaic_forward'
    
    return(out)


}


print.stepaic_forward <- function(data) {

    print_stepaic_forward(data)

}


plot.stepaic_forward <- function(data, ...) {

    x    <- seq_len(data$steps)
    xloc <- x - 0.1
    yloc <- data$aics - 0.2
    xmin <- min(x) - 1
    xmax <- max(x) + 1
    ymin <- min(data$aics) - 1
    ymax <- max(data$aics) + 1

    plot(x, data$aics, 
         type = "b", 
         col  = "blue", 
         xlab = "Steps", 
         ylab = "AIC",
         xlim = c(xmin, xmax), 
         ylim = c(ymin, ymax),
         main = "Step AIC: Variable Selection")
    
    text(xloc, yloc, data$predictors, 
         col  = "red", 
         cex  = 0.9)

}


