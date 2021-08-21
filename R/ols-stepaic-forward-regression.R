#' Stepwise AIC forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on akaike information criterion, in a stepwise
#' manner until there is no variable left to enter any more.
#'
#' @param model An object of class \code{lm}.
#' @param include Character vector; force variables to be included in selection process.
#' @param exclude Character vector; force variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_forward_aic}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#' @return \code{ols_step_forward_aic} returns an object of class \code{"ols_step_forward_aic"}.
#' An object of class \code{"ols_step_forward_aic"} is a list containing the
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
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_aic(model)
#'
#' # stepwise forward regression plot
#' k <- ols_step_forward_aic(model)
#' plot(k)
#'
#' # extract final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' ols_step_forward_aic(model, include = c("age"))
#'
#' # force variable to be excluded from selection process
#' ols_step_forward_aic(model, exclude = c("liver_test"))
#'
#' # include & exclude variables in the selection process
#' ols_step_forward_aic(model, include = c("age"), exclude = c("liver_test"))
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_forward_aic <- function(model, ...) UseMethod("ols_step_forward_aic")

#' @export
#' @rdname ols_step_forward_aic
#'
ols_step_forward_aic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_model(model)
  check_logic(details)
  check_npredictors(model, 3)

  response <- names(model$model)[1]
  l        <- model$model
  lockterm <- c(include, exclude)
  nam      <- setdiff(olsrr:::coeff_names(model), lockterm)
  all_pred <- nam
  mlen_p   <- length(all_pred)
  preds    <- include
  step     <- 1
  aics     <- c()
  ess      <- c()
  rss      <- c()
  rsq      <- c()
  arsq     <- c()

  if (is.null(include)) {
    b_model <- lm(paste(response, "~", 1), data = l)
  } else {
    b_model <- lm(paste(response, "~", paste(include, collapse = " + ")), data = l)
  }

  aic1 <- ols_aic(b_model)

  if (progress || details) {
    cat(format("Forward Selection Method", justify = "left", width = 24), "\n")
    cat(rep("-", 24), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")
  }

  if (details) {
    cat("\n")
      if (is.null(include)) {
        cat("Step  => 0", "\n")
        cat("Model =>", paste(response, "~", 1, "\n"))
        cat("AIC   =>", aic1, "\n\n")
      } else {
        cat("Step  => 0", "\n")
        cat("Model =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
        cat("AIC   =>", aic1, "\n\n")
      }
      cat("Initiating stepwise selection...", "\n\n")
  }

  for (i in seq_len(mlen_p)) {

    predictors <- c(include, all_pred[i])
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
    w5 <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
    w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
    w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
    w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
    ln <- length(aics)

    cat(format("Information Criteria Table", justify = "centre", width = w), "\n")
    cat(rep("-", w), sep = "", "\n")
    cat(
      fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
      fc("Sum Sq", w4), fs(), fc("RSS", w5), fs(), fc("R-Sq", w6), fs(),
      fc("Adj. R-Sq", w7), "\n"
    )
    cat(rep("-", w), sep = "", "\n")

    for (i in seq_len(ln)) {
      cat(
        fl(da2[i, 1], w1), fs(), fg(1, w2), fs(),
        fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
        fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(),
        fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
        fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(),
        fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
      )
    }

    cat(rep("-", w), sep = "", "\n\n")
  }

  minc     <- which(aics == min(aics))
  laic     <- aics[minc]
  less     <- ess[minc]
  lrss     <- rss[minc]
  lrsq     <- rsq[minc]
  larsq    <- arsq[minc]
  preds    <- c(preds, all_pred[minc])
  lpreds   <- length(preds) - length(include)
  all_pred <- all_pred[-minc]
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

  while (step < mlen_p) {

    aics <- c()
    ess  <- c()
    rss  <- c()
    rsst <- c()
    rsq  <- c()
    arsq <- c()
    mo   <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
    aic1 <- ols_aic(mo$model)

    if (details) {
      cat("Step     =>", step, "\n")
      cat("Selected =>", tail(preds, n = 1), "\n")
      cat("Model    =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
      cat("AIC      =>", aic1, "\n\n")
    }

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      k          <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)
      aics[i]    <- ols_aic(k$model)
      ess[i]     <- k$ess
      rsst[i]    <- k$rss
      rss[i]     <- round(k$rss - mo$rss, 3)
      rsq[i]     <- k$rsq
      arsq[i]    <- k$adjr
    }

    if (details) {

      da  <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
      da2 <- da[order(-da$rss), ]
      w1  <- max(nchar("Predictor"), nchar(as.character(da2$predictors)))
      w2  <- 2
      w3  <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
      w4  <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
      w5  <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
      w6  <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
      w7  <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
      w   <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
      ln  <- length(aics)

      cat(format("Information Criteria Table", justify = "centre", width = w), "\n")
      cat(rep("-", w), sep = "", "\n")
      cat(
        fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
        fc("Sum Sq", w4), fs(), fc("RSS", w5), fs(), fc("R-Sq", w6), fs(),
        fc("Adj. R-Sq", w7), "\n"
      )
      cat(rep("-", w), sep = "", "\n")

      for (i in seq_len(ln)) {
        cat(
          fl(da2[i, 1], w1), fs(), fg(1, w2), fs(),
          fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
          fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(),
          fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
          fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(),
          fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
        )
      }

      cat(rep("-", w), sep = "", "\n\n")
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
      lpreds   <- length(preds) - length(include)
      all_pred <- all_pred[-minaic]
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
      if (progress || details) {
        cat("\n")
        cat("No more variables to be added.", "\n")
      }
      break
    }
  }

  if (details) {
    cat("\n")
    cat("Variables Selected:", "\n\n")
    for (i in seq_len(length(preds))) {
      if (interactive()) {
        cat("+", preds[i], "\n")
      } else {
        cat(paste("-", preds[i]), "\n")
      }
    }
  }

  if (progress || details) {
    cat("\n\n")
    cat("Final Model Output", "\n")
    cat(rep("-", 18), sep = "", "\n\n")

    fi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
    print(fi)
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  if (is.null(include)) {
    var_selected <- preds
  } else {
    var_selected <- preds[-seq_len(length(include))]
  }

  metrics     <- data.frame(step     = seq_len(step),
                            variable = var_selected,
                            r2       = lrsq,
                            adj_r2   = larsq,
                            aic      = laic,
                            rss      = lrss,
                            ess      = less)

  out <- list(metrics = metrics,
              model   = final_model,
              others  = list(base_model = b_model))


  class(out) <- "ols_step_forward_aic"

  return(out)
}

#' @export
#'
print.ols_step_forward_aic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_stepaic_forward(x)
  } else {
    print("No variables have been added to the model.")
  }
}

#' @rdname ols_step_forward_aic
#' @export
#'
plot.ols_step_forward_aic <- function(x, print_plot = TRUE, ...) {

  aic <- NULL
  tx  <- NULL
  a   <- NULL
  b   <- NULL

  step <- c(0, x$metrics$step)
  aic  <- c(ols_aic(x$others$base_model), x$metrics$aic)
  pred <- c("Base Model", x$metrics$variable)

  y    <- step
  xloc <- y
  yloc <- aic
  xmin <- min(y) - 1
  xmax <- max(y) + 1
  ymin <- min(aic) - 1
  ymax <- max(aic) + 1

  d2 <- data.frame(x = xloc, y = yloc, tx = pred)
  d  <- data.frame(a = y, b = aic)

  p <-
    ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + xlab("Step") + ylab("AIC") +
    ggtitle("Stepwise AIC Forward Selection") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), hjust = 0, nudge_x = 0.1)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
