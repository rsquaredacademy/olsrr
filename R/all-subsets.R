all_subset <- function(model) UseMethod('all_subset')

all_subset.default <- function(model) {

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
    adjrsq   <- list()
    predrsq  <- list()
    cp       <- list()
    aic      <- list()
    sbic     <- list()
    sbc      <- list()
    gmsep    <- list()
    jp       <- list()
    pc       <- list()
    sp       <- list()
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
                adjrsq[[mcount]]  <- out$adjr
                predrsq[[mcount]] <- pred_rsq(out$model)
                cp[[mcount]]      <- mallow_cp(out$model, model)
                aic[[mcount]]     <- aic(out$model)
                sbic[[mcount]]    <- sbic(out$model, model)
                sbc[[mcount]]     <- sbc(out$model)
                gmsep[[mcount]]   <- gmsep(out$model)
                jp[[mcount]]      <- jp(out$model)
                pc[[mcount]]      <- pc(out$model)
                sp[[mcount]]      <- sp(out$model)
                preds[[mcount]]   <- paste(predictors, collapse = " ")

        }
    }

    ui <- data.frame(n               = lpreds, 
                    predictors       = unlist(preds), 
                    rsquare          = round(unlist(rsq), 3), 
                    adjr             = round(unlist(adjrsq), 3), 
                    predrsq          = round(unlist(predrsq), 3), 
                    cp               = round(unlist(cp), 3), 
                    aic              = round(unlist(aic), 3), 
                    sbic             = round(unlist(sbic), 3), 
                    sbc              = round(unlist(sbc), 3),
                    gmsep            = round(unlist(gmsep), 3), 
                    jp               = round(unlist(jp), 3), 
                    pc               = round(unlist(pc), 3), 
                    sp               = round(unlist(sp), 3), 
                    stringsAsFactors = F)

    sorted <- c()

    for (i in seq_len(lc)) {
        temp   <- ui[q[i]:t[i], ]
        temp   <- arrange(temp, desc(rsquare))
        sorted <- rbind(sorted, temp)
    }

    mindex <- seq_len(nrow(sorted))
    sorted <- cbind(mindex, sorted)

    class(sorted) <- c('all_subset')

    return(sorted)
}


print.all_subset <- function(data, ...) {

    n <- max(data$mindex)
    k <- data.frame(matrix(unlist(data), nrow = n), stringsAsFactors = F)[, c(1:5, 7)]
    names(k) <- c('Index', 'N', 'Predictors', 'R-Square', 'Adj. R-Square', "Mallow's Cp")
    print(k)

}


plot.all_subset <- function(data, ...) {

    maxs  <- tapply(data$rsquare, data$n, max)
    lmaxs <- seq_len(length(maxs))
    index <- c()

    suppressWarnings(
        for (i in lmaxs) {
            index[i] <- which(data$rsquare == maxs[i])
        }
    )

    maxs1  <- tapply(data$adjr, data$n, max)
    lmaxs1 <- seq_len(length(maxs1))
    index1 <- c()

    suppressWarnings(
        for (i in lmaxs1) {
            index1[i] <- which(data$adjr == maxs1[i])
        }
    )

    cps   <- abs(data$n - data$cp)
    mcps  <- tapply(cps, data$n, min)
    lmcps <- seq_len(length(mcps))
    imcps <- c()

    for (i in lmcps) {
        imcps[i] <- which(cps == mcps[i])
    }

    maxs2  <- tapply(data$aic, data$n, min)
    lmaxs2 <- seq_len(length(maxs2))
    index2 <- c()

    for (i in lmaxs2) {
        index2[i] <- which(data$aic == maxs2[i])
    }

    maxs3  <- tapply(data$sbic, data$n, min)
    lmaxs3 <- seq_len(length(maxs3))
    index3 <- c()

    for (i in lmaxs3) {
        index3[i] <- which(data$sbic == maxs3[i])
    }

    maxs4  <- tapply(data$sbc, data$n, min)
    lmaxs4 <- seq_len(length(maxs4))
    index4 <- c()

    for (i in lmaxs4) {
        index4[i] <- which(data$sbc == maxs4[i])
    }

    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(mfrow = c(2, 3))

    plot(data$n, data$rsquare, col = "#0000A0", xlab = '', ylab = '', cex = 1.5,
     main = 'R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
    abline(v = lmaxs, col = "gray")
    points(lmaxs, maxs, pch = 2, col = "red", cex = 2)
    text(x = lmaxs + 0.1, y = maxs, labels = index)

    plot(data$n, data$rsquare, col = "#0000A0", xlab = '', ylab = '', cex = 1.5,
     main = 'R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
    abline(v = lmaxs1, col = "gray")
    points(lmaxs1, maxs1, pch = 2, col = "red", cex = 2)
    text(x = lmaxs1 + 0.1, y = maxs1, labels = index1)

    plot(data$n, data$cp, col = "#0000A0", ylim = c(-20, max(cps)), xlab = '', ylab = '', cex = 1.5,
        main = 'C(p)', cex.main = 1, axes = FALSE, frame.plot = T)
    abline(v = lmcps, col = "gray")
    points(lmcps, data$cp[imcps], pch = 2, col = "red", cex = 2)
    text(x = lmcps + 0.1, y = data$cp[imcps], labels = imcps)

    plot(data$n, data$aic, col = "#0000A0", xlab = 'Step', ylab = '', cex = 1.5,
     main = 'AIC', cex.main = 1, yaxt = 'n')
    abline(v = lmaxs2, col = "gray")
    points(lmaxs2, maxs2, pch = 2, col = "red", cex = 2)
    text(x = lmaxs2 + 0.1, y = maxs2, labels = index2)

    plot(data$n, data$sbic, col = "#0000A0", xlab = 'Step', ylab = '', cex = 1.5,
     main = 'SBIC', cex.main = 1, yaxt = 'n')
    abline(v = lmaxs3, col = "gray")
    points(lmaxs3, maxs3, pch = 2, col = "red", cex = 2)
    text(x = lmaxs3 + 0.1, y = maxs3, labels = index3)

    plot(data$n, data$sbc, col = "#0000A0", xlab = 'Step', ylab = '', cex = 1.5,
     main = 'SBC', cex.main = 1, yaxt = 'n')
    abline(v = lmaxs4, col = "gray")
    points(lmaxs4, maxs4, pch = 2, col = "red", cex = 2)
    text(x = lmaxs4 + 0.1, y = maxs4, labels = index4)

}






















