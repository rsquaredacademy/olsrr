#' Stepwise AIC backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on akaike information criterion, in a stepwise
#' manner until there is no variable left to remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_backward_aic}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#'
#' @return \code{ols_step_backward_aic} returns an object of class \code{"ols_step_backward_aic"}.
#' An object of class \code{"ols_step_backward_aic"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#' \item{others}{list; info used for plotting and printing}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward_aic(model)
#'
#' # stepwise backward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_backward_aic(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' @importFrom ggplot2 geom_text
#' @importFrom utils tail
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_backward_aic <- function(model, ...) UseMethod("ols_step_backward_aic")

#' @export
#' @rdname ols_step_backward_aic
#'
ols_step_backward_aic.default <- function(model, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- TRUE
  }

  check_model(model)
  check_logic(details)
  check_npredictors(model, 3)

  response <- names(model$model)[1]
  l        <- model$model
  nam      <- coeff_names(model)
  preds    <- nam
  aic_f    <- ols_aic(model)

  mi    <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
  rss_f <- mi$rss
  laic  <- aic_f
  lrss  <- rss_f
  less  <- mi$ess
  lrsq  <- mi$rsq
  larsq <- mi$adjr

  if (progress) {
    cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
    cat(rep("-", 27), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")
  }

  if (details) {
    cat(" Step 0: AIC =", aic_f, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))
  }

  ilp   <- length(preds)
  end   <- FALSE
  step  <- 0
  rpred <- c()
  aics  <- c()
  ess   <- c()
  rss   <- c()
  rsq   <- c()
  arsq  <- c()

  for (i in seq_len(ilp)) {

    predictors <- preds[-i]

    m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)

    aics[i] <- ols_aic(m$model)
    ess[i]  <- m$ess
    rss[i]  <- rss_f - m$rss
    rsq[i]  <- m$rsq
    arsq[i] <- m$adjr
  }

  da <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
  da2 <- da[order(da$rss), ]

  if (details) {
    w1 <- max(nchar("Predictor"), nchar(predictors))
    w2 <- 2
    w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
    w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
    w5 <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
    w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
    w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
    w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
    ln <- length(aics)

    cat(rep("-", w), sep = "", "\n")
    cat(
      fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
      fc("Sum Sq", w4), fs(), fc("ESS", w5), fs(), fc("R-Sq", w6), fs(),
      fc("Adj. R-Sq", w7), "\n"
    )
    cat(rep("-", w), sep = "", "\n")

    for (i in seq_len(ln)) {
      cat(
        fl(da2[i, 1], w1), fs(), fc(1, w2), fs(), fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
        fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(), fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
        fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(), fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
      )
    }

    cat(rep("-", w), sep = "", "\n\n")
  }

  if (progress) {
    cat("\n")
    cat("Variables Removed:", "\n\n")
  }

  while (!end) {
    minc <- which(aics == min(aics))

    if (aics[minc] < aic_f) {

      rpred <- c(rpred, preds[minc])
      preds <- preds[-minc]
      ilp   <- length(preds)
      step  <- step + 1
      aic_f <- aics[minc]

      mi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")),
                        data = l)

      rss_f <- mi$rss
      laic  <- c(laic, aic_f)
      lrss  <- c(lrss, rss_f)
      less  <- c(less, mi$ess)
      lrsq  <- c(lrsq, mi$rsq)
      larsq <- c(larsq, mi$adjr)
      aics  <- c()
      ess   <- c()
      rss   <- c()
      rsq   <- c()
      arsq  <- c()

      if (progress) {
        if (interactive()) {
          cat("x", tail(rpred, n = 1), "\n")
        } else {
          cat(paste("-", tail(rpred, n = 1)), "\n")
        }
      }

      for (i in seq_len(ilp)) {

        predictors <- preds[-i]

        m <- ols_regress(paste(response, "~",
                               paste(predictors, collapse = " + ")), data = l)

        aics[i] <- ols_aic(m$model)
        ess[i]  <- m$ess
        rss[i]  <- rss_f - m$rss
        rsq[i]  <- m$rsq
        arsq[i] <- m$adjr
      }


      if (details) {
        cat("\n\n", " Step", step, ": AIC =", aic_f, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))


        da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
        da2 <- da[order(da$rss), ]
        w1  <- max(nchar("Predictor"), nchar(predictors))
        w2  <- 2
        w3  <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
        w4  <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
        w5  <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
        w6  <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
        w7  <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
        w   <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
        ln  <- length(aics)

        cat(rep("-", w), sep = "", "\n")
        cat(
          fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
          fc("Sum Sq", w4), fs(), fc("RSS", w5), fs(), fc("R-Sq", w6), fs(),
          fc("Adj. R-Sq", w7), "\n"
        )
        cat(rep("-", w), sep = "", "\n")

        for (i in seq_len(ln)) {
          cat(
            fl(da2[i, 1], w1), fs(), fc(1, w2), fs(), fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
            fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(), fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
            fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(), fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
          )
        }

        cat(rep("-", w), sep = "", "\n\n")
      }
    } else {
      end <- TRUE
      if (progress) {
        cat("\n")
        cat("No more variables to be removed.")
      }
    }
  }


  if (details) {
    cat("\n\n")
    cat("Variables Removed:", "\n\n")
    for (i in seq_len(length(rpred))) {
      if (interactive()) {
        cat("x", rpred[i], "\n")
      } else {
        cat(paste("-", rpred[i]), "\n")
      }
    }
  }

  if (progress) {
    cat("\n\n")
    cat("Final Model Output", "\n")
    cat(rep("-", 18), sep = "", "\n\n")

    fi <- ols_regress(
      paste(response, "~", paste(preds, collapse = " + ")),
      data = l
    )
    print(fi)
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  metrics     <- data.frame(step     = seq_len(step),
                            variable = rpred,
                            r2       = tail(lrsq,  n = step),
                            adj_r2   = tail(larsq, n = step),
                            aic      = tail(laic,  n = step),
                            rss      = tail(lrss,  n = step),
                            ess      = tail(less,  n = step))

  out <- list(metrics = metrics,
              model   = final_model,
              others  = list(model = model))

  class(out) <- "ols_step_backward_aic"

  return(out)
}

#' @export
#'
print.ols_step_backward_aic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_stepaic_backward(x)
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_backward_aic
#' @export
#'
plot.ols_step_backward_aic <- function(x, print_plot = TRUE, ...) {

  step <- NULL
  aic  <- NULL
  tx    <- NULL
  a     <- NULL
  b     <- NULL

  mi <- ols_regress(x$others$model)
  predictors <- c("Full Model", x$metrics$variable)
  aic  <- c(mi$aic, x$metrics$aic)

     y <- c(0, seq_len(length(x$metrics$step)))
  xloc <- y - 0.1
  yloc <- aic - 0.2
  xmin <- min(y) - 0.4
  xmax <- max(y) + 1
  ymin <- min(aic) - 1
  ymax <- max(aic) + 1 

  d2 <- data.frame(x = xloc, y = yloc, tx = predictors)
  d  <- data.frame(a = y, b = aic)

  p <-
    ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + xlab("Step") + ylab("AIC") +
    ggtitle("Stepwise AIC Backward Elimination") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), hjust = 0, nudge_x = 0.1)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}