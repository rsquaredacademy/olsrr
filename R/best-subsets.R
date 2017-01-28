#' @importFrom ggplot2 geom_line theme element_blank
#' @title Best Subsets Regression
#' @description Select the subset of predictors that do the best at meeting some
#' well-defined objective criterion, such as having the largest R2 value or the
#' smallest MSE.
#' @param model an object of class \code{lm}
#' @param ... other inputs
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return \code{best_subset} returns an object of class \code{"best_subset"}.
#' An object of class \code{"best_subset"} is a data frame containing the
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
#' @export
#'
best_subset <- function(model, ...) UseMethod('best_subset')

#' @export
#'
best_subset.default <- function(model, ...) {

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
        temp   <- temp[order(temp$rsquare, decreasing = TRUE), ]
        # temp   <- arrange(temp, desc(rsquare))
        sorted <- rbind(sorted, temp[1, ])
    }

    mindex <- seq_len(nrow(sorted))
    sorted <- cbind(mindex, sorted)

    class(sorted) <- 'best_subset'

    return(sorted)
}

#' @export
#'
print.best_subset <- function(x, ...) {
    print_best_subset(x)
}

#' @export
#'
plot.best_subset <- function(x, model = NA, ...) {

    # op <- par(no.readonly = TRUE)
    # on.exit(par(op))
    # m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
    # layout(mat = m, heights = c(2, 2))
    a <- NULL
    b <- NULL

    d1 <- tibble(a = x$mindex, b = x$rsquare)
    p1 <- ggplot(d1, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
    # plot(x$mindex, x$rsquare, type = 'b', col = 'blue', xlab = '', ylab = '',
    #  main = 'R-Square', cex.main = 1, axes = FALSE, frame.plot = T)

    d2 <- tibble(a = x$mindex, b = x$adjr)
    p2 <- ggplot(d2, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('Adj. R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
    # plot(x$mindex, x$adjr, type = 'b', col = 'blue', xlab = '', ylab = '',
    #     main = 'Adj. R-Square', cex.main = 1, axes = FALSE, frame.plot = T)

    d3 <- tibble(a = x$mindex, b = x$cp)
    p3 <- ggplot(d3, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('C(p)') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
    # plot(x$mindex, x$cp, type = 'b', col = 'blue', xlab = '', ylab = '',
    #     main = 'C(p)', cex.main = 1, axes = FALSE, frame.plot = T)

    d4 <- tibble(a = x$mindex, b = x$aic)
    p4 <- ggplot(d4, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('AIC') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
    # plot(x$mindex, x$aic, type = 'b', col = 'blue', xlab = 'Predictors', ylab = '',
    #     main = 'AIC', cex.main = 1, yaxt = 'n')

    d5 <- tibble(a = x$mindex, b = x$sbic)
    p5 <- ggplot(d5, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBIC') +
    theme(
        axis.ticks = element_blank())
    # plot(x$mindex, x$sbic, type = 'b', col = 'blue', xlab = 'Predictors', ylab = '',
    #     main = 'SBIC', cex.main = 1, yaxt = 'n')

    d6 <- tibble(a = x$mindex, b = x$sbc)
    p6 <- ggplot(d6, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBC') +
    theme(
        axis.ticks = element_blank())
    # plot(x$mindex, x$sbc, type = 'b', col = 'blue', xlab = 'Predictors', ylab = '',
    #     main = 'SBC', cex.main = 1, yaxt = 'n')

    grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = 'Best Subsets Regression')
}
