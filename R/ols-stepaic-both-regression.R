#' Stepwise AIC regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering and removing predictors based on akaike information criteria, in a
#' stepwise manner until there is no variable left to enter or remove any more.
#'
#' @param model An object of class \code{lm}.
#' @param x An object of class \code{ols_stepaic_both}.
#' @param details Logical; if \code{TRUE}, details of variable selection will
#'   be printed on screen.
#' @param ... Other arguments.
#'
#' @return \code{ols_stepaic_both} returns an object of class \code{"ols_stepaic_both"}.
#' An object of class \code{"ols_stepaic_both"} is a list containing the
#' following components:
#'
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
#' @examples
#' \dontrun{
#' # stepwise regression
#' model <- lm(y ~ ., data = stepdata)
#' ols_stepaic_both(model)
#' }
#'
#' \dontrun{
#' # stepwise regression plot
#' model <- lm(y ~ ., data = stepdata)
#' k <- ols_stepaic_both(model)
#' plot(k)
#' }
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_stepaic_both <- function(model, details = FALSE) UseMethod("ols_stepaic_both")

#' @export
#'
ols_stepaic_both.default <- function(model, details = FALSE) {

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

  l          <- mod_sel_data(model)
  nam        <- coeff_names(model)
  predictors <- nam
  mlen_p     <- length(predictors)
  tech       <- c("addition", "removal")
  mo         <- lm(paste(response, "~", 1), data = l)
  aic_c      <- ols_aic(mo)

  cat(format("Stepwise Selection Method", justify = "left", width = 25), "\n")
  cat(rep("-", 25), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(nam))) {
    cat(paste(i, ".", nam[i]), "\n")
  }
  cat("\n")

  if (details == TRUE) {
    cat(" Step 0: AIC =", aic_c, "\n", paste(response, "~", 1, "\n\n"))
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

  cat("\n")
  if (!details) {
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
    da2 <- arrange(da, desc(rss))

    if (details == TRUE) {
      w1 <- max(nchar("Predictor"), nchar(predictors))
      w2 <- 2
      w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
      w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
      w5 <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
      w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
      w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
      w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
      ln <- length(aics)

      cat(fc(crayon::bold$green("  Enter New Variables"), w), sep = "", "\n")
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

      if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
      } else {
        cat(paste("-", dplyr::last(preds), "added"), "\n")
      }

      if (details == TRUE) {
        cat("\n\n", "Step", all_step, ": AIC =", maic, "\n", paste(response, "~", paste(preds, collapse = " + ")), "\n\n")
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
        da2 <- arrange(da, desc(rss))

        if (details == TRUE) {
          w1 <- max(nchar("Predictor"), nchar(preds))
          w2 <- 2
          w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
          w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
          w5 <- max(nchar("RSS"), nchar(format(round(ess, 3), nsmall = 3)))
          w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
          w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
          w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
          ln <- length(aics)

          cat(fc(crayon::bold$red("Remove Existing Variables"), w), sep = "", "\n")
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


        minc2 <- which(aics == min(aics))


        if (aics[minc2] < laic[all_step]) {
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

          if (interactive()) {
            cat(crayon::red(clisymbols::symbol$cross), crayon::bold(preds[minc2]), "\n")
          } else {
            cat(paste("-", preds[minc2], "removed"), "\n")
          }

          preds <- preds[-minc2]
          lpreds <- length(preds)

          if (details == TRUE) {
            cat("\n\n", "Step", all_step, ": AIC =", maic, "\n", paste(response, "~", paste(preds, collapse = " + ")), "\n\n")
          }
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      cat("\n")
      cat(crayon::bold$red("No more variables to be added or removed."))
      break
    }
  }

  if (details == TRUE) {
    cat("\n\n")
    cat("Final Model Output", "\n")
    cat(rep("-", 18), sep = "", "\n\n")

    fi <- ols_regress(
      paste(response, "~", paste(preds, collapse = " + ")),
      data = l
    )
    print(fi)
  }

  out <- list(predictors = var_index,
              method     = method,
              steps      = all_step,
              arsq       = larsq,
              aic        = laic,
              ess        = less,
              rss        = lrss,
              rsq        = lrsq)

  class(out) <- "ols_stepaic_both"

  return(out)
}

#' @export
#'
print.ols_stepaic_both <- function(x, ...) {
  if (x$steps > 0) {
    print_stepaic_both(x)
  } else {
    print("No variables have been added to or removed from the model.")
  }
}

#' @rdname ols_stepaic_both
#' @export
#'
plot.ols_stepaic_both <- function(x, ...) {

  a  <- NULL
  b  <- NULL
  tx <- NULL

  predictors <- x$predictors

  y <-
    x %>%
    use_series(aic) %>%
    length() %>%
    seq_len()

  xloc  <- y - 0.1
  yloc  <- x$aic - 0.2
  xmin  <- min(y) - 0.4
  xmax  <- max(y) + 1

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

  d2 <- tibble(x = xloc, y = yloc, tx = predictors)
  d  <- tibble(a = y, b = x$aic)

  p <- ggplot(d, aes(x = a, y = b)) + geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + xlab("Step") + ylab("AIC") +
    ggtitle("Stepwise AIC Both Direction Selection") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), hjust = 0, nudge_x = 0.1)

  print(p)

}
