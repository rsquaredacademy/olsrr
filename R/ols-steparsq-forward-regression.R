#' Stepwise Adjusted R-Squared forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on adjusted R-squared, in a stepwise
#' manner until there is no variable left to enter any more, or a maximum step has been reached.
#'
#' @param model An object of class \code{lm}.
#' @param max_steps Integer; if not \code{NULL}, will specify the maximum number of steps.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_forward_arsq}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#' @return \code{ols_step_forward_arsq} returns an object of class \code{"ols_step_forward_arsq"}.
#' An object of class \code{"ols_step_forward_arsq"} is a list containing the
#' following components:
#'
#' \item{model}{model with the highest ARSQ; an object of class \code{lm}}
#' \item{steps}{total number of steps}
#' \item{predictors}{variables added to the model}
#' \item{aics}{akaike information criteria}
#' \item{ess}{error sum of squares}
#' \item{rss}{regression sum of squares}
#' \item{rsq}{rsquare}
#' \item{arsq}{adjusted rsquare}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @section Deprecated Function:
#' \code{ols_steparsq_forward()} has been deprecated. Instead use \code{ols_step_forward_arsq()}.
#'
#' @examples
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_arsq(model)
#'
#' # stepwise forward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_forward_arsq(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_forward_arsq <- function(model, ...) UseMethod("ols_step_forward_arsq")

#' @export
#' @rdname ols_step_forward_arsq
#'
ols_step_forward_arsq.default <- function(model, max_steps = NULL, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- TRUE
  }

  check_model(model)
  check_logic(details)
  check_npredictors(model, 3)

  response  <- names(model$model)[1]
  l         <- mod_sel_data(model)
  nam       <- coeff_names(model)
  all_pred  <- nam
  mlen_p    <- length(all_pred)
  max_steps <- ifelse(is.null(max_steps), mlen_p, max_steps)
  preds     <- c()
  step      <- 1
  aics      <- c()
  ess       <- c()
  rss       <- c()
  rsq       <- c()
  arsq      <- c()
  mo        <- lm(paste(response, "~", 1), data = l)
  arsq1     <- summary(mo)$adj.r.squared

  if (progress) {
    cat(format("Forward Selection Method", justify = "left", width = 24), "\n")
    cat(rep("-", 24), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")

    if (details == TRUE) {
      cat(" Step 0: ARSQ =", arsq1, "\n", paste(response, "~", 1, "\n\n"))
    }
  }

  for (i in seq_len(mlen_p)) {

    predictors <- all_pred[i]
    k <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)

    aics[i] <- ols_aic(k$model)
    ess[i]  <- k$ess
    rss[i]  <- k$rss
    rsq[i]  <- k$rsq
    arsq[i] <- k$adjr
  }

  da <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
  da2 <- da[order(-da$rss), ]

  if (details) {
    w1 <- max(nchar("Predictor"), nchar(all_pred))
    w2 <- 2
    w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
    w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
    w5 <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
    w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
    w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
    w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
    ln <- length(arsq)

    cat(rep("-", w), sep = "", "\n")
    cat(
      fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
      fc("Sum Sq", w4), fs(), fc("RSS", w5), fs(), fc("R-Sq", w6), fs(),
      fc("Adj. R-Sq", w7), "\n"
    )
    cat(rep("-", w), sep = "", "\n")

    for (i in seq_len(ln)) {
      cat(
        fl(da2[i, 1], w1), fs(), fg(1, w2), fs(), fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
        fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(), fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
        fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(),
        fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
      )
    }

    cat(rep("-", w), sep = "", "\n\n")
  }

  maxa     <- which(arsq == max(arsq))
  laic     <- aics[maxa]
  less     <- ess[maxa]
  lrss     <- rss[maxa]
  lrsq     <- rsq[maxa]
  larsq    <- arsq[maxa]
  preds    <- all_pred[maxa]
  lpreds   <- length(preds)
  all_pred <- all_pred[-maxa]
  len_p    <- length(all_pred)
  step     <- 1

  if (progress) {
    cat("\n")
    if (!details) {
      cat("Variables Entered:", "\n\n")
    }
  }

  if (progress) {
    if (interactive()) {
      cat("+", tail(preds, n = 1), "\n")
    } else {
      cat(paste("-", tail(preds, n = 1)), "\n")
    }
  }

  while (step < max_steps) {

    aics <- c()
    ess  <- c()
    rss  <- c()
    rsst <- c()
    rsq  <- c()
    arsq <- c()
    mo   <- ols_regress(paste(response, "~",
                            paste(preds, collapse = " + ")), data = l)
    arsq1 <- summary(mo$model)$adj.r.squared

    if (details) {
      cat("\n\n", "Step", step, ": ARSQ =", arsq1, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))
    }

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      k <- ols_regress(paste(response, "~",
                             paste(predictors, collapse = " + ")), data = l)

      aics[i] <- ols_aic(k$model)
      ess[i]  <- k$ess
      rsst[i] <- k$rss
      rss[i]  <- round(k$rss - mo$rss, 3)
      rsq[i]  <- k$rsq
      arsq[i] <- k$adjr
    }

    if (details) {

      da  <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
      # da2 <- arrange(da, desc(rss))
      da2 <- da[order(-da$rss), ]
      w1  <- max(nchar("Predictor"), nchar(as.character(da2$predictors)))
      w2  <- 2
      w3  <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
      w4  <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
      w5  <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
      w6  <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
      w7  <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
      w   <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
      ln  <- length(arsq)

      cat(rep("-", w), sep = "", "\n")
      cat(
        fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
        fc("Sum Sq", w4), fs(), fc("RSS", w5), fs(), fc("R-Sq", w6), fs(),
        fc("Adj. R-Sq", w7), "\n"
      )
      cat(rep("-", w), sep = "", "\n")

      for (i in seq_len(ln)) {
        cat(
          fl(da2[i, 1], w1), fs(), fg(1, w2), fs(), fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
          fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(), fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
          fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(),
          fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
        )
      }

      cat(rep("-", w), sep = "", "\n\n")
    }

    maxarsq <- which(arsq == max(arsq))

    if (arsq[maxarsq] > larsq[lpreds]) {

      preds    <- c(preds, all_pred[maxarsq])
      maxc     <- aics[maxarsq]
      mess     <- ess[maxarsq]
      mrss     <- round(rsst[maxarsq], 3)
      mrsq     <- rsq[maxarsq]
      marsq    <- arsq[maxarsq]
      laic     <- c(laic, maxc)
      less     <- c(less, mess)
      lrss     <- c(lrss, mrss)
      lrsq     <- c(lrsq, mrsq)
      larsq    <- c(larsq, marsq)
      lpreds   <- length(preds)
      all_pred <- all_pred[-maxarsq]
      len_p    <- length(all_pred)
      step     <- step + 1

      if (progress) {
        if (interactive()) {
          cat("+", tail(preds, n = 1), "\n")
        } else {
          cat(paste("-", tail(preds, n = 1)), "\n")
        }
      }
    } else {
      if (progress) {
        cat("\n")
        cat("No more variables to be added.")
      }
      break
    }
  }

  if (details) {
    cat("\n\n")
    cat("Variables Entered:", "\n\n")
    for (i in seq_len(length(preds))) {
      if (interactive()) {
        cat("+", preds[i], "\n")
      } else {
        cat(paste("-", preds[i]), "\n")
      }
    }
  }

  if (progress) {
    cat("\n\n")
    cat("Final Model Output", "\n")
    cat(rep("-", 18), sep = "", "\n\n")

    fi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
    print(fi)
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  out <- list(predictors = preds,
              steps      = step,
              arsq       = larsq,
              aics       = laic,
              ess        = less,
              rss        = lrss,
              rsq        = lrsq,
              model      = final_model)


  class(out) <- "ols_step_forward_arsq"

  return(out)
}

#' @export
#'
print.ols_step_forward_arsq <- function(x, ...) {
  if (x$steps > 0) {
    print_steparsq_forward(x)
  } else {
    print("No variables have been added to the model.")
  }
}

#' @rdname ols_step_forward_arsq
#' @export
#'
plot.ols_step_forward_arsq <- function(x, print_plot = TRUE, ...) {

  arsq <- NULL
  tx  <- NULL
  a   <- NULL
  b   <- NULL

  y    <- seq_len(x$steps)
  xloc <- y - 0.1
  yloc <- x$arsq - 0.2
  xmin <- min(y) - 1
  xmax <- max(y) + 1
  ymin <- min(x$arsq) - 1
  ymax <- max(x$arsq) + 1

  predictors <- x$predictors

  d2 <- data.frame(x = xloc, y = yloc, tx = predictors)
  d  <- data.frame(a = y, b = x$arsq)

  p <-
    ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + xlab("Step") + ylab("ARSQ") +
    ggtitle("Stepwise Adj. R-Sq Forward Selection") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), hjust = 0, nudge_x = 0.1)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}


#' @export
#' @rdname ols_step_forward_arsq
#' @usage NULL
#'
ols_steparsq_forward <- function(model, ...) {
  .Deprecated("ols_step_forward_arsq()")
}
