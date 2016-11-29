stepaic_both <- function(model, details = FALSE) UseMethod('stepaic_both')

stepaic_both.default <- function(model, details = FALSE) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    if (!is.logical(details)) {
      stop('details must be either TRUE or FALSE', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    l          <- model.frame(model)
    nam        <- names(l)
    response   <- nam[1]
    predictors <- nam[-1]
    mlen_p     <- length(predictors)
    tech       <- c('addition', 'removal')
    mo         <- lm(paste(response, '~', 1), data = l)
    aic_c      <- aic(mo)

    if (details == TRUE) {
        cat(' Step 0: AIC =', aic_c, '\n', paste(response, '~', 1, '\n\n'))
    }

    step      <- 0
    all_step  <- 0
    preds     <- c()
    var_index <- c()
    method    <- c()
    laic      <- c()
    less      <- c()
    lrss      <- c()
    lrsq      <- c()
    larsq     <- c()

    while (step < mlen_p) {
        
        aics <- c()
        ess  <- c()
        rss  <- c()
        rsq  <- c()
        arsq <- c()
        lpds <- length(predictors)

        for (i in seq_len(lpds)) {
            predn   <- c(preds, predictors[i])
            m       <- regress(paste(response, '~', paste(predn, collapse = ' + ')), data = l)
            aics[i] <- aic(m$model)
            ess[i]  <- m$ess
            rss[i]  <- m$rss
            rsq[i]  <- m$rsq
            arsq[i] <- m$adjr

        }

        da  <- data.frame(predictors = predictors, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
        da2 <- arrange(da, desc(rss))

        if (details == TRUE) {

                w1 <- max(nchar('Predictor'), nchar(predictors))
                w2 <- 2
                w3 <- max(nchar('AIC'), nchar(aics))
                w4 <- max(nchar('Sum Sq'), nchar(rss))
                w5 <- max(nchar('RSS'), nchar(ess))
                w6 <- max(nchar('R-Sq'), nchar(rsq))
                w7 <- max(nchar('Adj. R-Sq'), nchar(arsq))
                w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
                ln <- length(aics)

                cat(format('Addition', width = w, justify = 'centre'), '\n')
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
                
                cat(rep("-", w), sep = "", '\n')
        }

        minc <- which(aics == min(aics))

        if (aics[minc] < aic_c) {

            aic_c      <- aics[minc]
            preds      <- c(preds, predictors[minc])
            predictors <- predictors[-minc]
            lpds       <- length(predictors)
            method     <- c(method, tech[1])
            lpreds     <- length(preds)
            var_index  <- c(var_index, preds[lpreds])
            step       <- step + 1
            all_step   <- all_step + 1
            maic       <- aics[minc]
            mess       <- ess[minc]
            mrss       <- rss[minc]
            mrsq       <- rsq[minc]
            marsq      <- arsq[minc]
            laic       <- c(laic, maic)
            less       <- c(less, mess)
            lrss       <- c(lrss, mrss)
            lrsq       <- c(lrsq, mrsq)
            larsq      <- c(larsq, marsq)

            if (details == TRUE) {
                cat('\n\n', 'Step', all_step, ': AIC =', maic, '\n', paste(response, '~', paste(preds, collapse = ' + ')), '\n\n')
            }

            aics <- c()
            ess  <- c()
            rss  <- c()
            rsq  <- c()
            arsq <- c()

            if (lpreds > 1) {

                for (i in seq_len(lpreds)) {

                    preda   <- preds[-i]
                    m       <- regress(paste(response, '~', paste(preda, collapse = ' + ')), data = l)
                    aics[i] <- aic(m$model)
                    ess[i]  <- m$ess
                    rss[i]  <- m$rss
                    rsq[i]  <- m$rsq
                    arsq[i] <- m$adjr

                }

                da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
                da2 <- arrange(da, rss)

                if (details == TRUE) {

                        w1 <- max(nchar('Predictor'), nchar(predictors))
                        w2 <- 2
                        w3 <- max(nchar('AIC'), nchar(aics))
                        w4 <- max(nchar('Sum Sq'), nchar(rss))
                        w5 <- max(nchar('RSS'), nchar(ess))
                        w6 <- max(nchar('R-Sq'), nchar(rsq))
                        w7 <- max(nchar('Adj. R-Sq'), nchar(arsq))
                        w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
                        ln <- length(aics)

                        cat(format('Removal', width = w, justify = 'centre'), '\n')
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

                minc2 <- which(aics == min(aics))
                aic_c <- aics[minc2]

                if (aics[minc2] < laic[lpreds]) {

                    maic      <- aics[minc2]
                    mess      <- ess[minc2]
                    mrss      <- rss[minc2]
                    mrsq      <- rsq[minc2]
                    marsq     <- arsq[minc2]
                    laic      <- c(laic, maic)
                    less      <- c(less, mess)
                    lrss      <- c(lrss, mrss)
                    lrsq      <- c(lrsq, mrsq)
                    larsq     <- c(larsq, marsq)
                    var_index <- c(var_index, preds[minc2])
                    method    <- c(method, tech[2])
                    all_step  <- all_step + 1
                    preds     <- preds[-minc2]
                    lpreds    <- length(preds)

                    if (details == TRUE) {
                        cat('\n\n', 'Step', all_step, ': AIC =', maic, '\n', paste(response, '~', paste(preds, collapse = ' + ')), '\n\n')
                    }

                }


            } else {

                preds    <- preds
                all_step <- all_step
                    
            }

        } else {
            
            message(paste("No more variables to be added or removed."))
            break

        }

    }

    out <- list(predictors = var_index, 
                method     = method, 
                aic        = round(laic, 3), 
                ess        = round(less, 3),
                rss        = round(lrss, 3), 
                rsq        = round(lrsq, 3), 
                arsq       = round(larsq, 3), 
                steps      = all_step)

    class(out) <- 'stepaic_both'

    return(out)


}


print.stepaic_both <- function(data) {

    print_stepaic_both(data)

}


plot.stepaic_both <- function(data, ...) {

    x    <- seq_len(length(data$aic))
    xloc <- x - 0.1
    yloc <- data$aic - 0.2
    xmin <- min(x) - 0.4
    xmax <- max(x) + 1
    ymin <- min(data$aic) - 1
    ymax <- max(data$aic) + 1

    plot(x, data$aic, 
        type = "b", 
        col  = "blue", 
        xlab = "Steps", 
        ylab = "AIC",
        xlim = c(xmin, xmax), 
        ylim = c(ymin, ymax),
        main = "Step AIC: Stepwise Regression")

    text(xloc, yloc, data$predictors, 
        col = "red", 
        cex = 0.9)


}