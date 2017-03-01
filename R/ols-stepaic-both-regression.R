#' @title Stepwise AIC Regression
#' @description Build regression model from a set of candidate predictor variables by entering and removing predictors based on 
#' Akaike Information Criteria, in a stepwise manner until there is no variable left to enter or remove any more.
#' @param model an object of class \code{lm}
#' @param x an object of class \code{ols_stepaic_both}
#' @param details logical; if TRUE details of variable selection will be printed on screen
#' @param ... other arguments
#' @return \code{ols_stepaic_both} returns an object of class \code{"ols_stepaic_both"}.
#' An object of class \code{"ols_stepaic_both"} is a list containing the
#' following components:
#'
#' \item{predictors}{variables retained in the model}
#' \item{method}{addition/deletion}
#' \item{aics}{akaike information criteria}
#' \item{ess}{error sum of squares}
#' \item{rss}{regression sum of squares}
#' \item{rsq}{rsquare}
#' \item{arsq}{adjusted rsquare}
#' \item{steps}{total number of steps}
#' @references Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#' @examples
#' # stepwise regression
#' model <- lm(y ~ ., data = surgical)
#' ols_stepaic_both(model)
#'
#' # stepwise regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_stepaic_both(model)
#' plot(k)
#'
#' @export
#'
ols_stepaic_both <- function(model, details = FALSE) UseMethod('ols_stepaic_both')

#' @export
#'
ols_stepaic_both.default <- function(model, details = FALSE) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    if (!is.logical(details)) {
        stop('details must be either TRUE or FALSE', call. = FALSE)
    }

    if (length(model$coefficients) < 3) {
        stop('Please specify a model with at least 2 predictors.', call. = FALSE)
    }

    l          <- mod_sel_data(model)
    nam        <- names(l)
    response   <- nam[1]
    predictors <- nam[-1]
    mlen_p     <- length(predictors)
    tech       <- c('addition', 'removal')
    mo         <- lm(paste(response, '~', 1), data = l)
    aic_c      <- ols_aic(mo)

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
            m       <- ols_regress(paste(response, '~', paste(predn, collapse = ' + ')), data = l)
            aics[i] <- ols_aic(m$model)
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
                    m       <- ols_regress(paste(response, '~', paste(preda, collapse = ' + ')), data = l)
                    aics[i] <- ols_aic(m$model)
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

    class(out) <- 'ols_stepaic_both'

    return(out)

}

#' @export
#'
print.ols_stepaic_both <- function(x, ...) {
    if (x$steps > 0) {
      print_stepaic_both(x)
    } else {
      print('No variables have been added to or removed from the model.')
    }
}

#' @rdname ols_stepaic_both
#' @export
#'
plot.ols_stepaic_both <- function(x, ...) {

             y <- seq_len(length(x$aic))
          xloc <- y - 0.1
          yloc <- x$aic - 0.2
          xmin <- min(y) - 0.4
          xmax <- max(y) + 1
          ymin <- min(x$aic) - 1
          ymax <- max(x$aic) + 1
    predictors <- x$predictors
             a <- NULL
             b <- NULL
            tx <- NULL

    d2 <- tibble(x = xloc, y = yloc, tx = predictors)
    d <- tibble(a = y, b = x$aic)
    p <- ggplot(d, aes(x = a, y = b)) +
      geom_line(color = 'blue') +
      geom_point(color = 'blue', shape = 1, size = 2) +
      xlim(c(xmin, xmax)) + ylim(c(ymin, ymax)) +
      xlab('Step') + ylab('AIC') + ggtitle('Stepwise AIC Forward Selection') +
      geom_text(data = d2, aes(x = x, y = y, label = tx), hjust = 0, nudge_x = 0.1)

    print(p)

}