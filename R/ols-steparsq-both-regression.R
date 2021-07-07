#' Stepwise Adjusted R-Squared regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering and removing predictors based on adjusted R-squared, in a
#' stepwise manner until there is no variable left to enter or remove any more.
#'
#' @param model An object of class \code{lm}.
#' @param x An object of class \code{ols_step_both_arsq}.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, details of variable selection will
#'   be printed on screen.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#'
#' @return \code{ols_step_both_arsq} returns an object of class \code{"ols_step_both_arsq"}.
#' An object of class \code{"ols_step_both_arsq"} is a list containing the
#' following components:
#'
#' \item{model}{model with the highest ARSQ; an object of class \code{lm}}
#' \item{predictors}{variables added/removed from the model}
#' \item{method}{addition/deletion}
#' \item{aics}{akaike information criteria}
#' \item{ess}{error sum of squares}
#' \item{rss}{regression sum of squares}
#' \item{rsq}{rsquare}
#' \item{arsq}{adjusted rsquare}
#' \item{steps}{total number of steps}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @section Deprecated Function:
#' \code{ols_steparsq_both()} has been deprecated. Instead use \code{ols_step_both_arsq()}.
#'
#' @examples
#' \dontrun{
#' # stepwise regression
#' model <- lm(y ~ ., data = stepdata)
#' ols_step_both_arsq(model)
#'
#' # stepwise regression plot
#' model <- lm(y ~ ., data = stepdata)
#' k <- ols_step_both_arsq(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' }
#' @family variable selection procedures
#'
#' @export
#'
ols_step_both_arsq <- function(model, progress  = FALSE, details = FALSE) UseMethod("ols_step_both_arsq")

#' @export
#'
ols_step_both_arsq.default <- function(model, progress = FALSE, details = FALSE) {

  if (details) {
    progress <- TRUE
  }

  check_model(model)
  check_logic(details)
  check_npredictors(model, 3)

  response   <- names(model$model)[1]
  l          <- mod_sel_data(model)
  nam        <- coeff_names(model)
  predictors <- nam
  mlen_p     <- length(predictors)
  tech       <- c("addition", "removal")
  mo         <- lm(paste(response, "~", 1), data = l)
  arsq_c      <- summary(mo)$adj.r.squared

  if (progress) {
    cat(format("Stepwise Selection Method", justify = "left", width = 25), "\n")
    cat(rep("-", 25), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")
  }

  if (details) {
    cat(" Step 0: ARSQ =", arsq_c, "\n", paste(response, "~", 1, "\n\n"))
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

  if (progress) {
    cat("\n")
    cat("Variables Entered/Removed:", "\n\n")
  }

  while (step < mlen_p) {

    aics <- c()
    ess  <- c()
    rss  <- c()
    rsq  <- c()
    arsq <- c()
    lpds <- length(predictors)

    for (i in seq_len(lpds)) {

      predn <- c(preds, predictors[i])

      m <- ols_regress(paste(response, "~", paste(predn, collapse = " + ")), data = l)

      aics[i] <- ols_aic(m$model)
      ess[i]  <- m$ess
      rss[i]  <- m$rss
      rsq[i]  <- m$rsq
      arsq[i] <- m$adjr
    }

    da <- data.frame(predictors = predictors, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
    # da2 <- arrange(da, desc(rss))
    da2 <- da[order(-da$rss), ]

    if (details) {
      w1 <- max(nchar("Predictor"), nchar(predictors))
      w2 <- 2
      w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
      w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
      w5 <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
      w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
      w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
      w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
      ln <- length(arsq)

      cat(fc("  Enter New Variables", w), sep = "", "\n")
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


    maxc <- which(arsq == max(arsq))

    if (arsq[maxc] > arsq_c) {
      arsq_c     <- arsq[maxc]
      preds      <- c(preds, predictors[maxc])
      predictors <- predictors[-maxc]
      lpds       <- length(predictors)
      method     <- c(method, tech[1])
      lpreds     <- length(preds)
      var_index  <- c(var_index, preds[lpreds])
      step       <- step + 1
      all_step   <- all_step + 1
      maic       <- aics[maxc]
      mess       <- ess[maxc]
      mrss       <- rss[maxc]
      mrsq       <- rsq[maxc]
      marsq      <- arsq[maxc]
      laic       <- c(laic, maic)
      less       <- c(less, mess)
      lrss       <- c(lrss, mrss)
      lrsq       <- c(lrsq, mrsq)
      larsq      <- c(larsq, marsq)

      if (progress) {
        if (interactive()) {
          cat("+", tail(preds, n = 1), "\n")
        } else {
          cat(paste("-", tail(preds, n = 1), "added"), "\n")
        }
      }

      if (details) {
        cat("\n\n", "Step", all_step, ": ARSQ =", marsq, "\n", paste(response, "~", paste(preds, collapse = " + ")), "\n\n")
      }

      if (lpreds > 1) {

        aics <- c()
        ess  <- c()
        rss  <- c()
        rsq  <- c()
        arsq <- c()

        for (i in seq_len(lpreds)) {

          preda <- preds[-i]

          m <- ols_regress(paste(response, "~", paste(preda, collapse = " + ")), data = l)

          aics[i] <- ols_aic(m$model)
          ess[i]  <- m$ess
          rss[i]  <- m$rss
          rsq[i]  <- m$rsq
          arsq[i] <- m$adjr
        }

        da <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
        # da2 <- arrange(da, desc(rss))
        da2 <- da[order(-da$rss), ]

        if (details) {
          w1 <- max(nchar("Predictor"), nchar(preds))
          w2 <- 2
          w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
          w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
          w5 <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
          w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
          w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
          w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
          ln <- length(arsq)

          cat(fc("Remove Existing Variables", w), sep = "", "\n")
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


        maxc2 <- which(arsq == max(arsq))


        if (arsq[maxc2] > larsq[all_step]) {
          arsq_c    <- arsq[maxc2]
          maic      <- aics[maxc2]
          mess      <- ess[maxc2]
          mrss      <- rss[maxc2]
          mrsq      <- rsq[maxc2]
          marsq     <- arsq[maxc2]
          laic      <- c(laic, maic)
          less      <- c(less, mess)
          lrss      <- c(lrss, mrss)
          lrsq      <- c(lrsq, mrsq)
          larsq     <- c(larsq, marsq)
          var_index <- c(var_index, preds[maxc2])
          method    <- c(method, tech[2])
          all_step  <- all_step + 1

          if (progress) {
            if (interactive()) {
              cat("x", preds[maxc2], "\n")
            } else {
              cat(paste("-", preds[maxc2], "removed"), "\n")
            }
          }

          preds <- preds[-maxc2]
          lpreds <- length(preds)

          if (details) {
            cat("\n\n", "Step", all_step, ": ARSQ =", marsq, "\n", paste(response, "~", paste(preds, collapse = " + ")), "\n\n")
          }
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      if (progress) {
        cat("\n")
        cat("No more variables to be added or removed.")
      }
      break
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

  out <- list(predictors = var_index,
              method     = method,
              steps      = all_step,
              arsq       = larsq,
              aic        = laic,
              ess        = less,
              rss        = lrss,
              rsq        = lrsq)

  class(out) <- "ols_step_both_arsq"

  return(out)
}

#' @export
#'
print.ols_step_both_arsq <- function(x, ...) {
  if (x$steps > 0) {
    print_steparsq_both(x)
  } else {
    print("No variables have been added to or removed from the model.")
  }
}

#' @rdname ols_step_both_arsq
#' @export
#'
plot.ols_step_both_arsq <- function(x, print_plot = TRUE, ...) {

  arsq <- NULL
  tx  <- NULL
  a   <- NULL
  b   <- NULL

  predictors <- x$predictors

  y     <- seq_len(length(x$arsq))
  xloc  <- y - 0.1
  yloc  <- x$arsq - 0.2
  xmin  <- min(y) - 0.4
  xmax  <- max(y) + 1
  ymin  <- min(x$arsq) - 1
  ymax  <- max(x$arsq) + 1

  d2 <- data.frame(x = xloc, y = yloc, tx = predictors)
  d  <- data.frame(a = y, b = x$arsq)

  p <-
    ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + xlab("Step") + ylab("ARSQ") +
    ggtitle("Stepwise Adj. R-Sq Both Direction Selection") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), hjust = 0, nudge_x = 0.1)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}


#' @export
#' @rdname ols_step_both_arsq
#' @usage NULL
#'
ols_steparsq_both <- function(model, details = FALSE) {
  .Deprecated("ols_step_both_arsq()")
}
