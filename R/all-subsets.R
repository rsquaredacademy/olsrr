#' @importFrom ggplot2 ggtitle scale_shape_manual scale_size_manual scale_color_manual ggtitle geom_text
#' @importFrom utils combn
#' @importFrom purrr map_int
#' @title All Subsets Regression
#' @description Performs all subset regression for a linear regression model.
#' @param model an object of class \code{lm}
#' @param ... other arguments
#' @return \code{all_subset} returns an object of class \code{"all_subset"}.
#' An object of class \code{"all_subset"} is a data frame containing the
#' following components:
#'
#' \item{n}{model number}
#' \item{predictors}{predictors in the model}
#' \item{rsquare}{rsquare of the model}
#' \item{adjr}{adjusted rsquare of the model}
#' \item{predrsq}{predicted rsquare of the model}
#' \item{cp}{Mallow's Cp}
#' \item{aic}{Akaike Information Criteria}
#' \item{sbic}{Sawa Bayesian Information Criteria}
#' \item{sbc}{Schwarz Bayes Information Criteria}
#' \item{gmsep}{estimated MSE of prediction, assuming multivariate normality}
#' \item{jp}{final prediction error}
#' \item{pc}{Amemiya Prediction Criteria}
#' \item{sp}{Hocking's Sp}
#'
#' @examples
#' model <- lm(mpg ~ disp + hp, data = mtcars)
#' all_subset(model)
#'
#' @export
#'
all_subset <- function(model, ...) UseMethod('all_subset')

#' @export
#'
all_subset.default <- function(model, ...) {

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
    data      <- mod_sel_data(model)
    colas     <- combs %>% map_int(ncol)
    # colas     <- c()
    # for(i in seq_len(lc)) {
    #     colas[i] <- ncol(combs[[i]])
    # }

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
                    rsquare          = round(unlist(rsq), 5),
                    adjr             = round(unlist(adjrsq), 5),
                    predrsq          = round(unlist(predrsq), 5),
                    cp               = round(unlist(cp), 5),
                    aic              = round(unlist(aic), 5),
                    sbic             = round(unlist(sbic), 5),
                    sbc              = round(unlist(sbc), 5),
                    gmsep            = round(unlist(gmsep), 5),
                    jp               = round(unlist(jp), 5),
                    pc               = round(unlist(pc), 5),
                    sp               = round(unlist(sp), 5),
                    stringsAsFactors = F)

    sorted <- c()

    for (i in seq_len(lc)) {
        temp   <- ui[q[i]:t[i], ]
        temp   <- temp[order(temp$rsquare, decreasing = TRUE), ]
        # temp   <- arrange(temp, desc(rsquare))
        sorted <- rbind(sorted, temp)
    }

    mindex <- seq_len(nrow(sorted))
    sorted <- cbind(mindex, sorted)

    class(sorted) <- c('all_subset')

    return(sorted)
}


#' @export
#'
print.all_subset <- function(x, ...) {

    n <- max(x$mindex)
    k <- data.frame(matrix(unlist(x), nrow = n), stringsAsFactors = F)[, c(1:5, 7)]
    names(k) <- c('Index', 'N', 'Predictors', 'R-Square', 'Adj. R-Square', "Mallow's Cp")
    print(k)

}

#' @export
#'
plot.all_subset <- function(x, ...) {

    maxs  <- tapply(x$rsquare, x$n, max)
    lmaxs <- seq_len(length(maxs))
    index <- c()

    suppressWarnings(
        for (i in lmaxs) {
            index[i] <- which(x$rsquare == maxs[i])
        }
    )

    maxs1  <- tapply(x$adjr, x$n, max)
    lmaxs1 <- seq_len(length(maxs1))
    index1 <- c()

    suppressWarnings(
        for (i in lmaxs1) {
            index1[i] <- which(x$adjr == maxs1[i])
        }
    )

    cps   <- abs(x$n - x$cp)
    mcps  <- tapply(cps, x$n, min)
    lmcps <- seq_len(length(mcps))
    imcps <- c()

    for (i in lmcps) {
        imcps[i] <- which(cps == mcps[i])
    }

    maxs2  <- tapply(x$aic, x$n, min)
    lmaxs2 <- seq_len(length(maxs2))
    index2 <- c()

    for (i in lmaxs2) {
        index2[i] <- which(x$aic == maxs2[i])
    }

    maxs3  <- tapply(x$sbic, x$n, min)
    lmaxs3 <- seq_len(length(maxs3))
    index3 <- c()

    for (i in lmaxs3) {
        index3[i] <- which(x$sbic == maxs3[i])
    }

    maxs4  <- tapply(x$sbc, x$n, min)
    lmaxs4 <- seq_len(length(maxs4))
    index4 <- c()

    for (i in lmaxs4) {
        index4[i] <- which(x$sbc == maxs4[i])
    }

    shape <- NULL
    size <- NULL
    tx <- NULL
    y <- NULL
    k <- NULL

    d1 <- data.frame(x = x$n, y = x$rsquare)
    d2 <- data.frame(x = lmaxs, y = maxs, tx = index, shape = 6, size = 4)
    p1 <- ggplot(d1, aes(x = x, y = y)) +
      geom_point(color = 'blue', size = 2) +
      xlab('') + ylab('') + ggtitle('R-Square') +
      geom_point(data = d2, aes(x = x, y = y, shape = factor(shape),
        color = factor(shape), size = factor(size))) +
      scale_shape_manual(values = c(2), guide = FALSE) +
      scale_size_manual(values = c(4), guide = FALSE) +
      scale_color_manual(values = c('red'), guide = FALSE) +
      geom_text(data = d2, aes(label = tx), hjust = 0, nudge_x = 0.1)

    d3 <- data.frame(x = k$n, y = k$adjr)
    d4 <- data.frame(x = lmaxs1, y = maxs1, tx = index1, shape = 6, size = 4)
    p2 <- ggplot(d3, aes(x = x, y = y)) +
      geom_point(color = 'blue', size = 2) +
      xlab('') + ylab('') + ggtitle('Adj R-Square') +
      geom_point(data = d4, aes(x = x, y = y, shape = factor(shape),
        color = factor(shape), size = factor(size))) +
      scale_shape_manual(values = c(2), guide = FALSE) +
      scale_size_manual(values = c(4), guide = FALSE) +
      scale_color_manual(values = c('red'), guide = FALSE) +
      geom_text(data = d4, aes(label = tx), hjust = 0, nudge_x = 0.1)

    d5 <- data.frame(x = k$n, y = k$cp)
    d6 <- data.frame(x = lmcps, y = x$cp[imcps], tx = imcps,  shape = 6, size = 4)
    p3 <- ggplot(d5, aes(x = x, y = y)) +
      geom_point(color = 'blue', size = 2) +
      xlab('') + ylab('') + ggtitle('C(p)') +
      geom_point(data = d6, aes(x = x, y = y, shape = factor(shape),
        color = factor(shape), size = factor(size))) +
      scale_shape_manual(values = c(2), guide = FALSE) +
      scale_size_manual(values = c(4), guide = FALSE) +
      scale_color_manual(values = c('red'), guide = FALSE) +
      geom_text(data = d6, aes(label = tx), hjust = 0, nudge_x = 0.1)

    d7 <- data.frame(x = k$n, y = k$aic)
    d8 <- data.frame(x = lmaxs2, y = maxs2, tx = index2, shape = 6, size = 4)
    p4 <- ggplot(d7, aes(x = x, y = y)) +
      geom_point(color = 'blue', size = 2) +
      xlab('') + ylab('') + ggtitle('AIC') +
      geom_point(data = d8, aes(x = x, y = y, shape = factor(shape),
        color = factor(shape), size = factor(size))) +
      scale_shape_manual(values = c(2), guide = FALSE) +
      scale_size_manual(values = c(4), guide = FALSE) +
      scale_color_manual(values = c('red'), guide = FALSE) +
      geom_text(data = d8, aes(label = tx), hjust = 0, nudge_x = 0.1)

    d9 <- data.frame(x = k$n, y = k$sbic)
    d10 <- data.frame(x = lmaxs3, y = maxs3, tx = index3, shape = 6, size = 4)
    p5 <- ggplot(d9, aes(x = x, y = y)) +
      geom_point(color = 'blue', size = 2) +
      xlab('Number of Predictors') + ylab('') + ggtitle('SBIC') +
      geom_point(data = d10, aes(x = x, y = y, shape = factor(shape),
        color = factor(shape), size = factor(size))) +
      scale_shape_manual(values = c(2), guide = FALSE) +
      scale_size_manual(values = c(4), guide = FALSE) +
      scale_color_manual(values = c('red'), guide = FALSE) +
      geom_text(data = d10, aes(label = tx), hjust = 0, nudge_x = 0.1)

    d11 <- data.frame(x = k$n, y = k$sbc)
    d12 <- data.frame(x = lmaxs4, y = maxs4, tx = index4, shape = 6, size = 4)
    p6 <- ggplot(d11, aes(x = x, y = y)) +
      geom_point(color = 'blue', size = 2) +
      xlab('Number of Predictors') + ylab('') + ggtitle('SBC') +
      geom_point(data = d12, aes(x = x, y = y, shape = factor(shape),
        color = factor(shape), size = factor(size))) +
      scale_shape_manual(values = c(2), guide = FALSE) +
      scale_size_manual(values = c(4), guide = FALSE) +
      scale_color_manual(values = c('red'), guide = FALSE) +
      geom_text(data = d12, aes(label = tx), hjust = 0, nudge_x = 0.1)

    grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = 'All Subset Regression')

}



# plot.all_subset <- function(x, ...) {
#
#     maxs  <- tapply(x$rsquare, x$n, max)
#     lmaxs <- seq_len(length(maxs))
#     index <- c()
#
#     suppressWarnings(
#         for (i in lmaxs) {
#             index[i] <- which(x$rsquare == maxs[i])
#         }
#     )
#
#     maxs1  <- tapply(x$adjr, x$n, max)
#     lmaxs1 <- seq_len(length(maxs1))
#     index1 <- c()
#
#     suppressWarnings(
#         for (i in lmaxs1) {
#             index1[i] <- which(x$adjr == maxs1[i])
#         }
#     )
#
#     cps   <- abs(x$n - x$cp)
#     mcps  <- tapply(cps, x$n, min)
#     lmcps <- seq_len(length(mcps))
#     imcps <- c()
#
#     for (i in lmcps) {
#         imcps[i] <- which(cps == mcps[i])
#     }
#
#     maxs2  <- tapply(x$aic, x$n, min)
#     lmaxs2 <- seq_len(length(maxs2))
#     index2 <- c()
#
#     for (i in lmaxs2) {
#         index2[i] <- which(x$aic == maxs2[i])
#     }
#
#     maxs3  <- tapply(x$sbic, x$n, min)
#     lmaxs3 <- seq_len(length(maxs3))
#     index3 <- c()
#
#     for (i in lmaxs3) {
#         index3[i] <- which(x$sbic == maxs3[i])
#     }
#
#     maxs4  <- tapply(x$sbc, x$n, min)
#     lmaxs4 <- seq_len(length(maxs4))
#     index4 <- c()
#
#     for (i in lmaxs4) {
#         index4[i] <- which(x$sbc == maxs4[i])
#     }
#
#     op <- par(no.readonly = TRUE)
#     on.exit(par(op))
#     par(mfrow = c(2, 3))
#
#     plot(x$n, x$rsquare, col = "#0000A0", xlab = '', ylab = '', cex = 1.5,
#      main = 'R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
#     abline(v = lmaxs, col = "gray")
#     points(lmaxs, maxs, pch = 2, col = "red", cex = 2)
#     text(x = lmaxs + 0.1, y = maxs, labels = index)
#
#     plot(x$n, x$adjr, col = "#0000A0", xlab = '', ylab = '', cex = 1.5,
#      main = 'R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
#     abline(v = lmaxs1, col = "gray")
#     points(lmaxs1, maxs1, pch = 2, col = "red", cex = 2)
#     text(x = lmaxs1 + 0.1, y = maxs1, labels = index1)
#
#     plot(x$n, x$cp, col = "#0000A0", ylim = c(-20, max(cps)), xlab = '', ylab = '', cex = 1.5,
#         main = 'C(p)', cex.main = 1, axes = FALSE, frame.plot = T)
#     abline(v = lmcps, col = "gray")
#     points(lmcps, x$cp[imcps], pch = 2, col = "red", cex = 2)
#     text(x = lmcps + 0.1, y = x$cp[imcps], labels = imcps)
#
#     plot(x$n, x$aic, col = "#0000A0", xlab = 'Step', ylab = '', cex = 1.5,
#      main = 'AIC', cex.main = 1, yaxt = 'n')
#     abline(v = lmaxs2, col = "gray")
#     points(lmaxs2, maxs2, pch = 2, col = "red", cex = 2)
#     text(x = lmaxs2 + 0.1, y = maxs2, labels = index2)
#
#     plot(x$n, x$sbic, col = "#0000A0", xlab = 'Step', ylab = '', cex = 1.5,
#      main = 'SBIC', cex.main = 1, yaxt = 'n')
#     abline(v = lmaxs3, col = "gray")
#     points(lmaxs3, maxs3, pch = 2, col = "red", cex = 2)
#     text(x = lmaxs3 + 0.1, y = maxs3, labels = index3)
#
#     plot(x$n, x$sbc, col = "#0000A0", xlab = 'Step', ylab = '', cex = 1.5,
#      main = 'SBC', cex.main = 1, yaxt = 'n')
#     abline(v = lmaxs4, col = "gray")
#     points(lmaxs4, maxs4, pch = 2, col = "red", cex = 2)
#     text(x = lmaxs4 + 0.1, y = maxs4, labels = index4)
#
# }
