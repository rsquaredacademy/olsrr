#' @title Stepwise Regression
#' @description Stepwise Regression
#' @param model an object of class \code{lm}
#' @param ... other arguments
#' @return \code{stepwise} returns an object of class \code{"stepwise"}.
#' An object of class \code{"stepwise"} is a list containing the
#' following components:
#'
#' \item{orders}{f statistic}
#' \item{method}{f statistic}
#' \item{steps}{f statistic}
#' \item{predictors}{p value of \code{score}}
#' \item{rsquare}{degrees of freedom}
#' \item{aic}{fitted values of the regression model}
#' \item{sbc}{name of explanatory variables of fitted regression model}
#' \item{sbic}{response variable}
#' \item{adjr}{predictors}
#' \item{rmse}{predictors}
#' \item{mallows_cp}{predictors}
#' \item{indvar}{predictors}
#' @export
#'
stepwise <- function(model, ...)  UseMethod('stepwise')

#' @export
#'
stepwise.default <- function(model, pent = 0.1, prem = 0.3, details = FALSE, ...) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    if ((pent < 0) | (pent > 1)) {
      stop('pent must be between 0 and 1.', call. = FALSE)
    }

    if ((prem < 0) | (prem > 1)) {
      stop('pent must be between 0 and 1.', call. = FALSE)
    }

    if (!is.logical(details)) {
      stop('details must be either TRUE or FALSE', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    message("We are selecting variables based on p value...")
    l        <- model.frame(model)
    df       <- nrow(l) - 2
    tenter   <- qt(1 - (pent) / 2, df)
    trem     <- qt(1 - (prem) / 2, df)
    n        <- ncol(l)
    nam      <- names(l)
    response <- nam[1]
    all_pred <- nam[-1]
    cterms   <- all_pred
    mlen_p   <- length(all_pred)
    preds    <- c()
    step     <- 1
    ppos     <- step + 1
    pvals    <- c()
    tvals    <- c()
    rsq      <- c()
    cp       <- c()
    f        <- c()
    fp       <- c()

    for (i in seq_len(mlen_p)) {
        predictors <- all_pred[i]
        m          <- regress(paste(response, '~', paste(predictors, collapse = ' + ')), l)
        pvals[i]   <- m$pvalues[ppos]
        tvals[i]   <- m$tvalues[ppos]
    }

    minp   <- which(pvals == min(pvals))
    tvals  <- abs(tvals)
    maxt   <- which(tvals == max(tvals))
    preds  <- all_pred[maxt]
    lpreds <- length(preds)
    fr     <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
    rsq    <- fr$rsq
    adjrsq <- fr$adjr
    cp     <- mallow_cp(fr$model, model)
    aic    <- aic(fr$model)
    sbc    <- sbc(fr$model)
    sbic   <- sbic(fr$model, model)
    rmse   <- sqrt(fr$ems)
    message(paste(lpreds, "variable(s) added...."))


    if (details == TRUE) {

        cat("Variable Selection Procedure\n", paste("Dependent Variable:", response), "\n\n",
        paste("Stepwise Selection: Step", step), "\n\n", paste("Variable", preds[lpreds], "Entered"), "\n\n")
        m <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
        print(m)
        cat("\n\n")

    }

    all_step  <- 1
    tech      <- c("addition", "removal")
    var_index <- preds
    method    <- tech[1]

    while (step < mlen_p) {

        all_pred <- all_pred[-maxt]
        len_p    <- length(all_pred)
        step     <- step + 1
        ppos     <- ppos + length(maxt)
        pvals    <- c()
        tvals    <- c()

        for (i in seq_len(len_p)) {
            predictors <- c(preds, all_pred[i])
            m          <- regress(paste(response, '~', paste(predictors, collapse = ' + ')), l)
            pvals[i]   <- m$pvalues[ppos]
            tvals[i]   <- m$tvalues[ppos]
        }

        minp  <- which(pvals == min(pvals))
        tvals <- abs(tvals)
        maxt  <- which(tvals == max(tvals))

        if (tvals[maxt] >= tenter) {

            message(paste(length(maxt), "variable(s) added..."))
            preds     <- c(preds, all_pred[maxt])
            var_index <- c(var_index, all_pred[maxt])
            method    <- c(method, tech[1])
            lpreds    <- length(preds)
            all_step  <- all_step + 1
            fr        <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
            rsq       <- c(rsq, fr$rsq)
            adjrsq    <- c(adjrsq, fr$adjr)
            aic       <- c(aic, aic(fr$model))
            sbc       <- c(sbc, sbc(fr$model))
            sbic      <- c(sbic, sbic(fr$model, model))
            cp        <- c(cp, mallow_cp(fr$model, model))
            rmse      <- c(rmse, sqrt(fr$ems))

            if (details == TRUE) {

                cat(paste("Stepwise Selection: Step", all_step, "\n\n"), paste("Variable", preds[lpreds], "Entered"), "\n\n")
                m <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
                print(m)
                cat("\n\n")

            }

            m2      <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
            tvals_r <- abs(m2$tvalues[-1])
            mint    <- which(tvals_r == min(tvals_r))
            if (tvals_r[mint] < trem) {

                message(paste(length(mint), "variable(s) removed...."))
            	var_index <- c(var_index, preds[mint])
                lvar      <- length(var_index)
                method    <- c(method, tech[2])
            	preds     <- preds[-mint]
            	all_step  <- all_step + 1
                ppos      <- ppos - length(mint)
                fr        <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
                rsq       <- c(rsq, fr$rsq)
                adjrsq    <- c(adjrsq, fr$adjr)
                aic       <- c(aic, aic(fr$model))
                sbc       <- c(sbc, sbc(fr$model))
                sbic      <- c(sbic, sbic(fr$model, model))
                cp        <- c(cp, mallow_cp(fr$model, model))
                rmse      <- c(rmse, sqrt(fr$ems))

                if (details == TRUE) {

                    cat(paste("Stepwise Selection: Step", all_step, "\n\n"), paste("Variable", var_index[lvar], "removed"), "\n\n")
                    m <- regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
                    print(m)
                    cat("\n\n")

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

    out <- list(orders = var_index,
                method = method,
                steps = all_step,
                predictors = preds,
                rsquare = rsq,
                aic = aic,
                sbc = sbc,
                sbic = sbic,
                adjr = adjrsq,
                rmse = rmse,
                mallows_cp = cp,
                indvar = cterms)

    class(out) <- 'stepwise'

    return(out)
}

#' @export
#'
print.stepwise <- function(x, ...) {
    print_stepwise(x)
}

#' @export
#'
plot.stepwise <- function(x, ...) {

    y        <- seq_len(x$steps)
    rmax     <- max(x$rsquare)
    rstep    <- which(x$rsquare == rmax)
    adjrmax  <- max(x$adjr)
    adjrstep <- which(x$adjr == adjrmax)
    cpdiff   <- abs(x$mallows_cp - y)
    cpdifmin <- min(cpdiff)
    cpdifi   <- which(cpdiff == cpdifmin)
    cpval    <- x$mallows_cp[cpdifi]
    aicmin   <- min(x$aic)
    aicstep  <- which(x$aic == aicmin)
    sbicmin  <- min(x$sbic)
    sbicstep <- which(x$sbic == sbicmin)
    sbcmin   <- min(x$sbc)
    sbcstep  <- which(x$sbc == sbcmin)

    d1 <- tibble(a = y, b = x$rsquare) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d2 <- tibble(a = y, b = x$adjr) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('Adj. R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d3 <- tibble(a = y, b = x$mallows_cp) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('C(p)') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d4 <- tibble(a = y, b = x$aic) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('AIC') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d5 <- tibble(a = y, b = x$sbic) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBIC') +
    theme(
        axis.ticks = element_blank())

    d6 <- tibble(a = y, b = x$sbc) %>%
    ggplot(., aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBC') +
    theme(
        axis.ticks = element_blank())

    grid.arrange(d1, d2, d3, d4, d5, d6, ncol = 2, top = 'Stepwise Regression')

}

# plot.stepwise <- function(x, ...) {
#
#   y        <- seq_len(x$steps)
#   rmax     <- max(x$rsquare)
#   rstep    <- which(x$rsquare == rmax)
#   adjrmax  <- max(x$adjr)
#   adjrstep <- which(x$adjr == adjrmax)
#   cpdiff   <- abs(x$mallows_cp - y)
#   cpdifmin <- min(cpdiff)
#   cpdifi   <- which(cpdiff == cpdifmin)
#   cpval    <- x$mallows_cp[cpdifi]
#   aicmin   <- min(x$aic)
#   aicstep  <- which(x$aic == aicmin)
#   sbicmin  <- min(x$sbic)
#   sbicstep <- which(x$sbic == sbicmin)
#   sbcmin   <- min(x$sbc)
#   sbcstep  <- which(x$sbc == sbcmin)
#
#   op <- par(no.readonly = TRUE)
#   on.exit(par(op))
#   m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
#   layout(mat = m,heights = c(2, 2))
#
#   plot(y, x$rsquare, type = 'b', col = 'blue', xlab = '', ylab = '',
#   main = 'R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
#   points(rstep, rmax, pch = 2, col = "red", cex = 2.5)
#
#   plot(y, x$adjr, type = 'b', col = 'blue', xlab = '', ylab = '',
#   main = 'Adj. R-Square', cex.main = 1, axes = FALSE, frame.plot = T)
#   points(adjrstep, adjrmax, pch = 2, col = "red", cex = 2.5)
#
#   plot(y, x$mallows_cp, type = 'b', col = 'blue', xlab = '', ylab = '',
#   main = 'C(p)', cex.main = 1, axes = FALSE, frame.plot = T)
#   points(cpdifi, cpval, pch = 2, col = "red", cex = 2.5)
#
#   plot(y, x$aic, type = 'b', col = 'blue', xlab = 'Step', ylab = '',
#   main = 'AIC', cex.main = 1, yaxt = 'n')
#   points(aicstep, aicmin, pch = 2, col = "red", cex = 2.5)
#
#   plot(y, x$sbic, type = 'b', col = 'blue', xlab = 'Step', ylab = '',
#   main = 'SBIC', cex.main = 1, yaxt = 'n')
#   points(sbicstep, sbicmin, pch = 2, col = "red", cex = 2.5)
#
#   plot(y, x$sbc, type = 'b', col = 'blue', xlab = 'Step', ylab = '',
#   main = 'SBC', cex.main = 1, yaxt = 'n')
#   points(sbcstep, sbcmin, pch = 2, col = "red", cex = 2.5)
#
#
# }
