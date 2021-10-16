#' Stepwise AIC backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on akaike information criterion, in a stepwise
#' manner until there is no variable left to remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
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
#' # include or exclude variable
#' # force variables to be included in the selection process
#' ols_step_backward_aic(model, include = c("alc_mod", "gender"))
#'
#' # use index of variable instead of name
#' ols_step_backward_aic(model, include = c(7, 6))
#'
#' # force variable to be excluded from selection process
#' ols_step_backward_aic(model, exclude = c("alc_heavy", "bcs"))
#'
#' # use index of variable instead of name
#' ols_step_backward_aic(model, exclude = c(8, 1))
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
ols_step_backward_aic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

  response <- names(model$model)[1]
  l        <- model$model
  indterms <- coeff_names(model)
  lenterms <- length(indterms)

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  nam   <- setdiff(indterms, exclude)
  cterm <- setdiff(nam, include)
  preds <- nam
  aic_f <- ols_aic(model)

  mi    <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
  rss_f <- mi$rss
  laic  <- aic_f
  lrss  <- rss_f
  less  <- mi$ess
  lrsq  <- mi$rsq
  larsq <- mi$adjr

  if (progress || details) {
    cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
    cat(rep("-", 27), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(cterm))) {
      cat(paste(i, ".", cterm[i]), "\n")
    }
    cat("\n")
  }

  if (details) {
    cat("Step  => 0", "\n")
    cat("Model =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
    cat("AIC   =>", aic_f, "\n\n")
    cat("Initiating stepwise selection...", "\n\n")
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

  da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
  da2 <- da[order(da$aics), ]
  da3 <- cbind(loc = order(aics), da2)

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
      fl("Removed", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
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

  linc <- length(include)

  while (!end) {

    for (i in seq_len(linc)) {
      lnam <- which(da3$predictors == include[i])
      da3 <- da3[-lnam, ]
    }

    minc <- da3$loc[1]

    if (da3$aics[1] < aic_f) {

      rpred <- c(rpred, preds[minc])
      preds <- preds[-minc]
      ilp   <- length(preds)
      step  <- step + 1
      aic_f <- aics[minc]

      mi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)

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
        cat("\n")
        cat("Variables Removed:", "\n\n")
        cat(paste("=>", tail(rpred, n = 1)), "\n")
      }

      for (i in seq_len(ilp)) {

        predictors <- preds[-i]

        m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)

        aics[i] <- ols_aic(m$model)
        ess[i]  <- m$ess
        rss[i]  <- rss_f - m$rss
        rsq[i]  <- m$rsq
        arsq[i] <- m$adjr
      }

      da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
      da2 <- da[order(da$aics), ]
      da3 <- cbind(loc = order(aics), da2)

      if (details) {
        cat("Step    =>", step, "\n")
        cat("Removed =>", tail(rpred, n = 1), "\n")
        cat("Model   =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
        cat("AIC     =>", aic_f, "\n\n")

        da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
        da2 <- da[order(da$aics), ]

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
          fl("Removed", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
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
      if (progress || details) {
        cat("\n")
        cat("No more variables to be removed.")
        cat("\n")
      }
    }

  }


  if (details) {
    if (length(rpred) > 0) {
      cat("\n\n")
      cat("Variables Removed:", "\n\n")
      for (i in seq_len(length(rpred))) {
        cat(paste("=>", rpred[i]), "\n")
      }
    }
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
              others  = list(full_model = model))

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
plot.ols_step_backward_aic <- function(x, print_plot = TRUE, details = TRUE, ...) {

  tx    <- NULL
  a     <- NULL
  b     <- NULL

  preds <- x$metrics$variable
  aic   <- x$metrics$aic
  step  <- x$metrics$step

  if (details) {
    x$metrics$text <- paste0("[", x$metrics$variable, ", ", round(x$metrics$aic, 2), "]")
    pred <- x$metrics$text
  } else {
    pred <- x$metrics$variable
  }

  y     <- step
  xloc  <- y 
  yloc  <- aic 
  xmin  <- min(y) - 0.4
  xmax  <- max(y) + 1
  ymin  <- min(aic) - (min(aic) * 0.05)
  ymax  <- max(aic) + (max(aic) * 0.05)

  d2    <- data.frame(x = xloc, y = yloc, tx = pred)
  d     <- data.frame(a = y, b = aic)

  # metric info
  full_model_aic  <- round(ols_aic(x$others$full_model), 3)
  final_model_aic <- round(ols_aic(x$model), 3)
  metric_info <- paste0("Full Model AIC  : ", format(full_model_aic, nsmall = 3), "\n",
                        "Final Model AIC : ", format(final_model_aic, nsmall = 3))

  p <-
    ggplot(d, aes(x = a, y = b)) + 
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + 
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + 
    xlab("Step") + 
    ylab("AIC") +
    ggtitle("Stepwise AIC Backward Elimination") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), size = 3,
              vjust = "bottom", hjust = "left", nudge_x = 0.1) +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
             family = "serif", fontface = "italic", size = 3,
             label = metric_info)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
