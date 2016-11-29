best_subset <- function(model) UseMethod('best_subset')

best_subset.default <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    n     <- length(model$coefficients) - 1
    r     <- seq_len(n)
    combs <- list()

    for (i in seq_len(n)) {
        combs[[i]] <- combn(n, r[i])
    }

    lc        <- length(combs)
    nam       <- names(model$coefficients)[-1]
    varnames  <- names(model.frame(model))
    predicts  <- varnames[-1]
    len_preds <- length(predicts)
    gap       <- len_preds - 1
    space     <- sum(nchar(predicts)) + gap
    data      <- model.frame(model)
    colas     <- c()

    for(i in seq_len(lc)) {
        colas[i] <- ncol(combs[[i]])
    }

    response <- varnames[1]
    p        <- colas
    t        <- cumsum(colas)
    q        <- c(1, t[-lc] + 1)
    mcount   <- 0
    rsq      <- list()
    adjr     <- list()
    cp       <- list()
    aic      <- list()
    sbic     <- list()
    sbc      <- list()
    mse      <- list()
    gmsep    <- list()
    jp       <- list()
    pc       <- list()
    sp       <- list()
    press    <- list()
    predrsq  <- list()
    preds    <- list()
    lpreds   <- c()

    for (i in seq_len(lc)) {
        for (j in seq_len(colas[i])) {
                predictors        <- nam[combs[[i]][, j]]
                lp                <- length(predictors)
                out               <- regress(paste(response, '~', paste(predictors, collapse = ' + ')), data = data)
                mcount            <- mcount + 1
                lpreds[mcount]    <- lp
                rsq[[mcount]]     <- out$rsq
                adjr[[mcount]]    <- out$adjr
                cp[[mcount]]      <- mallow_cp(out$model, model)
                aic[[mcount]]     <- aic(out$model)
                sbic[[mcount]]    <- sbic(out$model, model)
                sbc[[mcount]]     <- sbc(out$model)
                gmsep[[mcount]]   <- gmsep(out$model)
                jp[[mcount]]      <- jp(out$model)
                pc[[mcount]]      <- pc(out$model)
                sp[[mcount]]      <- sp(out$model)
                predrsq[[mcount]] <- pred_rsq(out$model)
                preds[[mcount]]   <- paste(predictors, collapse = " ")
        }
    }

    ui <- data.frame(n = lpreds, 
                     predictors       = unlist(preds), 
                     rsquare          = unlist(rsq), 
                     adjr             = unlist(adjr), 
                     predrsq          = unlist(predrsq), 
                     cp               = unlist(cp), 
                     aic              = unlist(aic), 
                     sbic             = unlist(sbic), 
                     sbc              = unlist(sbc), 
                     gmsep            = unlist(gmsep), 
                     jp               = unlist(jp), 
                     pc               = unlist(pc), 
                     sp               = unlist(sp), 
                     stringsAsFactors = F)

    sorted <- c()
    
    for (i in seq_len(lc)) {
        temp   <- ui[q[i]:t[i], ]
        temp   <- arrange(temp, desc(rsquare))
        sorted <- rbind(sorted, temp[1, ])
    }

    mindex <- seq_len(nrow(sorted))
    sorted <- cbind(mindex, sorted)

    class(sorted) <- 'best_subset'

    return(sorted)
}


print.best_subset <- function(data) {

    print_best_subset(data)

}


plot.best_subset <- function(data, ...) {

    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
    layout(mat = m, heights = c(2, 2))

    plot(data$mindex, data$rsquare, type = 'b', col = 'blue', xlab = '', ylab = '',
     main = 'R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
    
    plot(data$mindex, data$adjr, type = 'b', col = 'blue', xlab = '', ylab = '', 
        main = 'Adj. R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
    
    plot(data$mindex, data$cp, type = 'b', col = 'blue', xlab = '', ylab = '', 
        main = 'C(p)', cex.main = 1, axes = FALSE, frame.plot = T)
    
    plot(data$mindex, data$aic, type = 'b', col = 'blue', xlab = 'Predictors', ylab = '', 
        main = 'AIC', cex.main = 1, yaxt = 'n')
    
    plot(data$mindex, data$sbic, type = 'b', col = 'blue', xlab = 'Predictors', ylab = '', 
        main = 'SBIC', cex.main = 1, yaxt = 'n')
    
    plot(data$mindex, data$sbc, type = 'b', col = 'blue', xlab = 'Predictors', ylab = '', 
        main = 'SBC', cex.main = 1, yaxt = 'n')
    
}












