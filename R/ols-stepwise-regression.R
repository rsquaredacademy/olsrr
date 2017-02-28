#' @title Stepwise Regression
#' @description Build regression model from a set of candidate predictor variables by entering and removing predictors based on 
#' p values, in a stepwise manner until there is no variable left to enter or remove any more.
#' @param model an object of class \code{lm}
#' @param x an object of class \code{ols_stepwise}
#' @param ... other arguments
#' @return \code{ols_stepwise} returns an object of class \code{"ols_stepwise"}.
#' An object of class \code{"ols_stepwise"} is a list containing the
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
#'
#' @references Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @examples
#' # stepwise regression
#' model <- lm(y ~ ., data = surgical)
#' ols_stepwise(model)
#'
#' # stepwise regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_stepwise(model)
#' plot(k)
#'
#' @export
#'
ols_stepwise <- function(model, ...)  UseMethod('ols_stepwise')

#' @export
#'
ols_stepwise.default <- function(model, pent = 0.1, prem = 0.3, details = FALSE, ...) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    if ((pent < 0) | (pent > 1)) {
      stop('p value for entering variables into the model must be between 0 and 1.', call. = FALSE)
    }

    if ((prem < 0) | (prem > 1)) {
      stop('p value for removing variables from the model must be between 0 and 1.', call. = FALSE)
    }

    if (!is.logical(details)) {
      stop('details must be either TRUE or FALSE', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    message("We are selecting variables based on p value...")
    l        <- mod_sel_data(model)
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
        m          <- ols_regress(paste(response, '~', paste(predictors, collapse = ' + ')), l)
        pvals[i]   <- m$pvalues[ppos]
        tvals[i]   <- m$tvalues[ppos]
    }

    minp   <- which(pvals == min(pvals))
    tvals  <- abs(tvals)
    maxt   <- which(tvals == max(tvals))
    preds  <- all_pred[maxt]
    lpreds <- length(preds)
    fr     <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
    rsq    <- fr$rsq
    adjrsq <- fr$adjr
    cp     <- ols_mallows_cp(fr$model, model)
    aic    <- ols_aic(fr$model)
    sbc    <- ols_sbc(fr$model)
    sbic   <- ols_sbic(fr$model, model)
    rmse   <- sqrt(fr$ems)
    message(paste(lpreds, "variable(s) added...."))


    if (details == TRUE) {

        cat("Variable Selection Procedure\n", paste("Dependent Variable:", response), "\n\n",
        paste("Stepwise Selection: Step", step), "\n\n", paste("Variable", preds[lpreds], "Entered"), "\n\n")
        m <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
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
            m          <- ols_regress(paste(response, '~', paste(predictors, collapse = ' + ')), l)
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
            fr        <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
            rsq       <- c(rsq, fr$rsq)
            adjrsq    <- c(adjrsq, fr$adjr)
            aic       <- c(aic, ols_aic(fr$model))
            sbc       <- c(sbc, ols_sbc(fr$model))
            sbic      <- c(sbic, ols_sbic(fr$model, model))
            cp        <- c(cp, ols_mallows_cp(fr$model, model))
            rmse      <- c(rmse, sqrt(fr$ems))

            if (details == TRUE) {

                cat(paste("Stepwise Selection: Step", all_step, "\n\n"), paste("Variable", preds[lpreds], "Entered"), "\n\n")
                m <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
                print(m)
                cat("\n\n")

            }

            m2      <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
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
                fr        <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
                rsq       <- c(rsq, fr$rsq)
                adjrsq    <- c(adjrsq, fr$adjr)
                aic       <- c(aic, ols_aic(fr$model))
                sbc       <- c(sbc, ols_sbc(fr$model))
                sbic      <- c(sbic, ols_sbic(fr$model, model))
                cp        <- c(cp, ols_mallows_cp(fr$model, model))
                rmse      <- c(rmse, sqrt(fr$ems))

                if (details == TRUE) {

                    cat(paste("Stepwise Selection: Step", all_step, "\n\n"), paste("Variable", var_index[lvar], "removed"), "\n\n")
                    m <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), l)
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

    class(out) <- 'ols_stepwise'

    return(out)
}

#' @export
#'
print.ols_stepwise <- function(x, ...) {
    if (x$steps > 0) {
      print_stepwise(x)
    } else {
      print('No variables have been added to or removed from the model.')
    }
    
}

#' @export
#' @rdname ols_stepwise
#'
plot.ols_stepwise <- function(x, model = NA, ...) {

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
           a <- NULL
           b <- NULL

    d1 <- tibble(a = y, b = x$rsquare)
    p1 <- ggplot(d1, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d2 <- tibble(a = y, b = x$adjr)
    p2 <- ggplot(d2, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('Adj. R-Square') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d3 <- tibble(a = y, b = x$mallows_cp)
    p3 <- ggplot(d3, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('C(p)') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d4 <- tibble(a = y, b = x$aic)
    p4 <- ggplot(d4, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('AIC') +
    theme(
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

    d5 <- tibble(a = y, b = x$sbic)
    p5 <- ggplot(d5, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBIC') +
    theme(
        axis.ticks = element_blank())

    d6 <- tibble(a = y, b = x$sbc)
    p6 <- ggplot(d6, aes(x = a, y = b)) +
    geom_line(color = 'blue') +
    geom_point(color = 'blue', shape = 1, size = 2) +
    xlab('') + ylab('') + ggtitle('SBC') +
    theme(
        axis.ticks = element_blank())

    grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = 'Stepwise Regression')

}

