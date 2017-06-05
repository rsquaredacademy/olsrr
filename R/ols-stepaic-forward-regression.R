#' @importFrom dplyr desc
#' @title Stepwise AIC Forward Regression
#' @description Build regression model from a set of candidate predictor variables by entering predictors based on 
#' Akaike Information Criteria, in a stepwise manner until there is no variable left to enter any more.
#' @param model an object of class \code{lm}
#' @param details logical; if \code{TRUE}, will print the regression result at each step
#' @param x an object of class \code{ols_stepaic_forward}
#' @param ... other arguments
#' @return \code{ols_stepaic_forward} returns an object of class \code{"ols_stepaic_forward"}.
#' An object of class \code{"ols_stepaic_forward"} is a list containing the
#' following components:
#'
#' \item{steps}{total number of steps}
#' \item{predictors}{variables added to the model}
#' \item{aics}{akaike information criteria}
#' \item{ess}{error sum of squares}
#' \item{rss}{regression sum of squares}
#' \item{rsq}{rsquare}
#' \item{arsq}{adjusted rsquare}
#' @references Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#' @examples
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_stepaic_forward(model)
#'
#' # stepwise forward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_stepaic_forward(model)
#' plot(k)
#'
#' @export
#'
ols_stepaic_forward <- function(model, ...) UseMethod('ols_stepaic_forward')

#' @export
#' @rdname ols_stepaic_forward
#'
ols_stepaic_forward.default <- function(model, details = FALSE, ...) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    if (!is.logical(details)) {
      stop('details must be either TRUE or FALSE', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    l        <- mod_sel_data(model)
    nam      <- colnames(attr(model$terms, 'factors'))
    response <- names(model$model)[1]
    all_pred <- nam
    # nam      <- names(l)
    # response <- nam[1]
    # all_pred <- nam[-1]
    mlen_p   <- length(all_pred)
    preds    <- c()
    step     <- 1
    aics     <- c()
    ess      <- c()
    rss      <- c()
    rsq      <- c()
    arsq     <- c()
    mo       <- lm(paste(response, '~', 1), data = l)
    aic1     <- ols_aic(mo)

    if (details == TRUE) {
        cat(' Step 0: AIC =', aic1, '\n', paste(response, '~', 1, '\n\n'))
    }

    for (i in seq_len(mlen_p)) {
        predictors <- all_pred[i]
        k          <- ols_regress(paste(response, '~', paste(predictors, collapse = ' + ')), data = l)
        aics[i]    <- ols_aic(k$model)
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
        mo   <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), data = l)
        aic1 <- ols_aic(mo$model)

        if (details == TRUE) {
            cat('\n\n', 'Step', step, ': AIC =', aic1, '\n', paste(response, '~', paste(preds, collapse = ' + '), '\n\n'))
        }

        for (i in seq_len(len_p)) {
            predictors <- c(preds, all_pred[i])
            k          <- ols_regress(paste(response, '~', paste(predictors, collapse = ' + ')), data = l)
            aics[i]    <- ols_aic(k$model)
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

        fi <- ols_regress(paste(response, '~', paste(preds, collapse = ' + ')), data = l)
        print(fi)

    }

    out <- list(steps = step,
                predictors = preds,
                aics = laic,
                ess = less,
                rss = lrss,
                rsq = lrsq,
                arsq = larsq)

    class(out) <- 'ols_stepaic_forward'

    return(out)


}

#' @export
#'
print.ols_stepaic_forward <- function(x, ...) {
    if (x$steps > 0) {
      print_stepaic_forward(x)
    } else {
      print('No variables have been added to the model.')
    }
}

#' @rdname ols_stepaic_forward
#' @export
#'
plot.ols_stepaic_forward <- function(x, ...) {

             y <- seq_len(x$steps)
          xloc <- y - 0.1
          yloc <- x$aics - 0.2
          xmin <- min(y) - 1
          xmax <- max(y) + 1
          ymin <- min(x$aics) - 1
          ymax <- max(x$aics) + 1
    predictors <- x$predictors
             a <- NULL
             b <- NULL
            tx <- NULL

    d2 <- tibble(x = xloc, y = yloc, tx = predictors)
    d <- tibble(a = y, b = x$aics)
    p <- ggplot(d, aes(x = a, y = b)) +
      geom_line(color = 'blue') +
      geom_point(color = 'blue', shape = 1, size = 2) +
      xlim(c(xmin, xmax)) + ylim(c(ymin, ymax)) +
      xlab('Step') + ylab('AIC') + ggtitle('Stepwise AIC Forward Selection') +
      geom_text(data = d2, aes(x = x, y = y, label = tx), hjust = 0, nudge_x = 0.1)

    print(p)
}

