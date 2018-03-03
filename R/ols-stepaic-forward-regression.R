#' Stepwise AIC forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on akaike information criterion, in a stepwise
#' manner until there is no variable left to enter any more.
#'
#' @param model An object of class \code{lm}.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_forward_aic}.
#' @param ... Other arguments.
#' @return \code{ols_step_forward_aic} returns an object of class \code{"ols_step_forward_aic"}.
#' An object of class \code{"ols_step_forward_aic"} is a list containing the
#' following components:
#'
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
#' \code{ols_stepaic_forward()} has been deprecated. Instead use \code{ols_step_forward_aic()}.
#'
#' @examples
#' \dontrun{
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_aic(model)
#' }
#'
#' \dontrun{
#' # stepwise forward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_forward_aic(model)
#' plot(k)
#' }
#'
#' @importFrom dplyr desc
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_forward_aic <- function(model, ...) UseMethod("ols_step_forward_aic")

#' @export
#' @rdname ols_step_forward_aic
#'
ols_step_forward_aic.default <- function(model, details = FALSE, ...) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  if (!is.logical(details)) {
    stop("details must be either TRUE or FALSE", call. = FALSE)
  }

  if (length(model$coefficients) < 3) {
    stop("Please specify a model with at least 2 predictors.", call. = FALSE)
  }

  response <-
    model %>%
    use_series(model) %>%
    names() %>%
    extract(1)

  l        <- mod_sel_data(model)
  nam      <- coeff_names(model)
  all_pred <- nam
  mlen_p   <- length(all_pred)
  preds    <- c()
  step     <- 1
  aics     <- c()
  ess      <- c()
  rss      <- c()
  rsq      <- c()
  arsq     <- c()
  mo       <- lm(paste(response, "~", 1), data = l)
  aic1     <- ols_aic(mo)

  cat(format("Forward Selection Method", justify = "left", width = 24), "\n")
  cat(rep("-", 24), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(nam))) {
    cat(paste(i, ".", nam[i]), "\n")
  }
  cat("\n")

  if (details == TRUE) {
    cat(" Step 0: AIC =", aic1, "\n", paste(response, "~", 1, "\n\n"))
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
  da2 <- arrange(da, desc(rss))

  if (details == TRUE) {
    w1 <- max(nchar("Predictor"), nchar(all_pred))
    w2 <- 2
    w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
    w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
    w5 <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
    w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
    w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
    w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
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
        fl(da2[i, 1], w1), fs(), fg(1, w2), fs(), fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
        fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(), fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
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
  preds    <- all_pred[minc]
  lpreds   <- length(preds)
  all_pred <- all_pred[-minc]
  len_p    <- length(all_pred)
  step     <- 1

  cat("\n")
  if (!details) {
    cat("Variables Entered:", "\n\n")
  }

  if (interactive()) {
    cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
  } else {
    cat(paste("-", dplyr::last(preds)), "\n")
  }

  while (step < mlen_p) {

    aics <- c()
    ess  <- c()
    rss  <- c()
    rsst <- c()
    rsq  <- c()
    arsq <- c()
    mo   <- ols_regress(paste(response, "~",
                            paste(preds, collapse = " + ")), data = l)
    aic1 <- ols_aic(mo$model)

    if (details == TRUE) {
      cat("\n\n", "Step", step, ": AIC =", aic1, "\n", paste(response, "~", paste(preds, collapse = " + "), "\n\n"))
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

    if (details == TRUE) {

      da  <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
      da2 <- arrange(da, desc(rss))
      w1  <- max(nchar("Predictor"), nchar(as.character(da2$predictors)))
      w2  <- 2
      w3  <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
      w4  <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
      w5  <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
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
          fl(da2[i, 1], w1), fs(), fg(1, w2), fs(), fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
          fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(), fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
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
      lpreds   <- length(preds)
      all_pred <- all_pred[-minaic]
      len_p    <- length(all_pred)
      step     <- step + 1

      if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
      } else {
        cat(paste("-", dplyr::last(preds)), "\n")
      }
    } else {
      cat("\n")
      cat(crayon::bold$red("No more variables to be added."))
      break
    }
  }

  if (details == TRUE) {
    cat("\n\n")
    cat("Variables Entered:", "\n\n")
    for (i in seq_len(length(preds))) {
      if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(preds[i]), "\n")
      } else {
        cat(paste("-", preds[i]), "\n")
      }
    }

    cat("\n\n")
    cat("Final Model Output", "\n")
    cat(rep("-", 18), sep = "", "\n\n")

    fi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
    print(fi)
  }

  out <- list(predictors = preds,
              steps      = step,
              arsq       = larsq,
              aics       = laic,
              ess        = less,
              rss        = lrss,
              rsq        = lrsq)

  class(out) <- "ols_step_forward_aic"

  return(out)
}

#' @export
#'
print.ols_step_forward_aic <- function(x, ...) {
  if (x$steps > 0) {
    print_stepaic_forward(x)
  } else {
    print("No variables have been added to the model.")
  }
}

#' @rdname ols_step_forward_aic
#' @export
#'
plot.ols_step_forward_aic <- function(x, ...) {

  aic <- NULL
  tx  <- NULL
  a   <- NULL
  b   <- NULL

  y    <- seq_len(x$steps)
  xloc <- y - 0.1
  yloc <- x$aics - 0.2
  xmin <- min(y) - 1
  xmax <- max(y) + 1

  ymin <-
    x %>%
    use_series(aic) %>%
    min() %>%
    subtract(1)

  ymax <-
    x %>%
    use_series(aic) %>%
    max() %>%
    add(1)

  predictors <- x$predictors

  d2 <- tibble(x = xloc, y = yloc, tx = predictors)
  d  <- tibble(a = y, b = x$aics)

  p <- ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + xlab("Step") + ylab("AIC") +
    ggtitle("Stepwise AIC Forward Selection") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), hjust = 0, nudge_x = 0.1)

  print(p)
}


#' @export
#' @rdname ols_step_forward_aic
#' @usage NULL
#'
ols_stepaic_forward <- function(model, ...) {
  .Deprecated("ols_step_forward_aic()")
}
