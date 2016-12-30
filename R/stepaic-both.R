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
            
            if (lpreds > 1) {
                
                aics <- c()
                ess  <- c()
                rss  <- c()
                rsq  <- c()
                arsq <- c()
                
                for (i in seq_len(lpreds)) {
                    
                    preda   <- preds[-i]
                    m       <- regress(paste(response, '~', paste(preda, collapse = ' + ')), data = l)
                    aics[i] <- aic(m$model)
                    ess[i]  <- m$ess
                    rss[i]  <- m$rss
                    rsq[i]  <- m$rsq
                    arsq[i] <- m$adjr
                    
                }
                
                
                minc2 <- which(aics == min(aics))
                
                
                if (aics[minc2] < laic[lpreds]) {
                    
                    aic_c     <- aics[minc2]
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