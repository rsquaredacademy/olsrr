#' @importFrom ggplot2 geom_text
#' @title Stepwise AIC Backward Regression
#' @description Build regression model from a set of candidate predictor variables by removing predictors based on
#' Akaike Information Criteria, in a stepwise manner until there is no variable left to remove any more.
#' @param model an object of class \code{lm}; the model should include all candidate predictor variables
#' @param details logical; if \code{TRUE}, will print the regression result at each step
#' @param x an object of class \code{ols_stepaic_backward}
#' @param ... other arguments
#' @return \code{ols_stepaic_backward} returns an object of class \code{"ols_stepaic_backward"}.
#' An object of class \code{"ols_stepaic_backward"} is a list containing the
#' following components:
#'
#' \item{steps}{total number of steps}
#' \item{predictors}{variables removed from the model}
#' \item{aics}{akaike information criteria}
#' \item{ess}{error sum of squares}
#' \item{rss}{regression sum of squares}
#' \item{rsq}{rsquare}
#' \item{arsq}{adjusted rsquare}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' \dontrun{
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_stepaic_backward(model)
#' }
#'
#' \dontrun{
#' # stepwise backward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_stepaic_backward(model)
#' plot(k)
#' }
#'
#' @export
#'
ols_stepaic_backward <- function(model, ...) UseMethod("ols_stepaic_backward")

#' @export
#' @rdname ols_stepaic_backward
#'
ols_stepaic_backward.default <- function(model, details = FALSE, ...) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  if (!is.logical(details)) {
    stop("details must be either TRUE or FALSE", call. = FALSE)
  }

  if (length(model$coefficients) < 3) {
    stop("Please specify a model with at least 2 predictors.", call. = FALSE)
  }

  l <- mod_sel_data(model)
  nam <- colnames(attr(model$terms, "factors"))
  response <- names(model$model)[1]
  preds <- nam
  # nam      <- names(l)
  # response <- nam[1]
  # preds    <- nam[-1]
  aic_f <- ols_aic(model)
  mi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
  rss_f <- mi$rss
  laic <- aic_f
  lrss <- rss_f
  less <- mi$ess
  lrsq <- mi$rsq
  larsq <- mi$adjr

  cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
  cat(rep("-", 27), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(nam))) {
    cat(paste(i, ".", nam[i]), "\n")
  }
  cat("\n")

  if (details == TRUE) {
    cat(" Step 0: AIC =", aic_f, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))
  }

  ilp <- length(preds)
  end <- FALSE
  step <- 0
  rpred <- c()
  aics <- c()
  ess <- c()
  rss <- c()
  rsq <- c()
  arsq <- c()

  for (i in seq_len(ilp)) {
    predictors <- preds[-i]
    m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)
    aics[i] <- ols_aic(m$model)
    ess[i] <- m$ess
    rss[i] <- rss_f - m$rss
    rsq[i] <- m$rsq
    arsq[i] <- m$adjr
  }

  da <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
  da2 <- arrange(da, rss)

  if (details == TRUE) {
    w1 <- max(nchar("Predictor"), nchar(predictors))
    w2 <- 2
    w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
    w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
    w5 <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
    w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
    w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
    w <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
    ln <- length(aics)

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

  cat("\n")
  if (!details) {
    cat("Variables Removed:", "\n\n")
  }

  while (!end) {
    minc <- which(aics == min(aics))

    if (aics[minc] < aic_f) {
      rpred <- c(rpred, preds[minc])
      preds <- preds[-minc]
      ilp <- length(preds)
      step <- step + 1
      aic_f <- aics[minc]
      mi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
      rss_f <- mi$rss
      laic <- c(laic, aic_f)
      lrss <- c(lrss, rss_f)
      less <- c(less, mi$ess)
      lrsq <- c(lrsq, mi$rsq)
      larsq <- c(larsq, mi$adjr)
      aics <- c()
      ess <- c()
      rss <- c()
      rsq <- c()
      arsq <- c()

      if (interactive()) {
        cat(crayon::red(clisymbols::symbol$cross), crayon::bold(dplyr::last(rpred)), "\n")
      } else {
        cat(paste("-", dplyr::last(rpred)), "\n")
      }

      for (i in seq_len(ilp)) {
        predictors <- preds[-i]
        m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)
        aics[i] <- ols_aic(m$model)
        ess[i] <- m$ess
        rss[i] <- rss_f - m$rss
        rsq[i] <- m$rsq
        arsq[i] <- m$adjr
      }


      if (details == TRUE) {
        cat("\n\n", " Step", step, ": AIC =", aic_f, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))


        da <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
        da2 <- arrange(da, rss)
        w1 <- max(nchar("Predictor"), nchar(predictors))
        w2 <- 2
        w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
        w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
        w5 <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
        w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
        w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
        w <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
        ln <- length(aics)

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
      cat("\n")
      cat(crayon::bold$red("No more variables to be removed."))
    }
  }


  if (details == TRUE) {
    cat("\n\n")
    cat("Variables Removed:", "\n\n")
    for (i in seq_len(length(rpred))) {
      if (interactive()) {
        cat(crayon::red(clisymbols::symbol$cross), crayon::bold(rpred[i]), "\n")
      } else {
        cat(paste("-", rpred[i]), "\n")
      }
    }

    cat("\n\n")
    cat("Final Model Output", "\n")
    cat(rep("-", 18), sep = "", "\n\n")

    fi <- ols_regress(
      paste(response, "~", paste(preds, collapse = " + ")),
      data = l
    )
    print(fi)
  }

  out <- list(
    steps = step,
    predictors = rpred,
    aics = laic,
    ess = less,
    rss = lrss,
    rsq = lrsq,
    arsq = larsq
  )

  class(out) <- "ols_stepaic_backward"

  return(out)
}

#' @export
#'
print.ols_stepaic_backward <- function(x, ...) {
  if (x$steps > 0) {
    print_stepaic_backward(x)
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_stepaic_backward
#' @export
#'
plot.ols_stepaic_backward <- function(x, ...) {
  y <- c(0, seq_len(x$steps))
  xloc <- y - 0.1
  yloc <- x$aics - 0.2
  xmin <- min(y) - 0.4
  xmax <- max(y) + 1
  ymin <- min(x$aics) - 1
  ymax <- max(x$aics) + 1
  predictors <- c("Full Model", x$predictors)
  a <- NULL
  b <- NULL
  tx <- NULL

  d2 <- tibble(x = xloc, y = yloc, tx = predictors)
  d <- tibble(a = y, b = x$aics)
  p <- ggplot(d, aes(x = a, y = b)) +
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) +
    xlim(c(xmin, xmax)) + ylim(c(ymin, ymax)) +
    xlab("Step") + ylab("AIC") + ggtitle("Stepwise AIC Backward Elimination") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), hjust = 0, nudge_x = 0.1)

  print(p)
}
