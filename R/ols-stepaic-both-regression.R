#' Stepwise AIC regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering and removing predictors based on akaike information criteria, in a
#' stepwise manner until there is no variable left to enter or remove any more.
#'
#' @param model An object of class \code{lm}.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, details of variable selection will
#'   be printed on screen.
#' @param x An object of class \code{ols_step_both_aic}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#'
#' @return \code{ols_step_both_aic} returns an object of class \code{"ols_step_both_aic"}.
#' An object of class \code{"ols_step_both_aic"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' \dontrun{
#' # stepwise regression
#' model <- lm(y ~ ., data = stepdata)
#' ols_step_both_aic(model)
#'
#' # stepwise regression plot
#' model <- lm(y ~ ., data = stepdata)
#' k <- ols_step_both_aic(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' model <- lm(y ~ ., data = stepdata)
#'
#' ols_step_both_aic(model, include = c("x6"))
#'
#' # use index of variable instead of name
#' ols_step_both_aic(model, include = c(6))
#'
#' # force variable to be excluded from selection process
#' ols_step_both_aic(model, exclude = c("x2"))
#'
#' # use index of variable instead of name
#' ols_step_both_aic(model, exclude = c(2))
#'
#' # include & exclude variables in the selection process
#' ols_step_both_aic(model, include = c("x6"), exclude = c("x2"))
#'
#' # use index of variable instead of name
#' ols_step_both_aic(model, include = c(6), exclude = c(2))
#' }
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_both_aic <- function(model, ...) UseMethod("ols_step_both_aic")

#' @export
#' @rdname ols_step_both_aic
#'
ols_step_both_aic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

  response <- names(model$model)[1]
  l        <- model$model
  nam      <- coeff_names(model)
  indterms <- nam
  lenterms <- length(indterms)
  len_inc  <- length(include) + 1
  
  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  lockterm   <- c(include, exclude)
  predictors <- setdiff(nam, lockterm)
  mlen_p     <- length(predictors)
  tech       <- c("addition", "removal")

  if (progress || details) {
    cat(format("Stepwise Selection Method", justify = "left", width = 25), "\n")
    cat(rep("-", 25), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")
  }

  if (is.null(include)) {
    base_model <- lm(paste(response, "~", 1), data = l)
  } else {
    base_model <- lm(paste(response, "~", paste(include, collapse = " + ")), data = l)
  }

  aic_c <- ols_aic(base_model)

  if (details) {
    cat("\n")
      if (is.null(include)) {
        cat("Step  => 0", "\n")
        cat("Model =>", paste(response, "~", 1, "\n"))
        cat("AIC   =>", aic_c, "\n\n")
      } else {
        cat("Step  => 0", "\n")
        cat("Model =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
        cat("AIC   =>", aic_c, "\n\n")
      }
      cat("Initiating stepwise selection...", "\n\n")
  }

  step      <- 0
  all_step  <- 0
  preds     <- include
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

      cat(fc("  Enter New Variables", w), sep = "", "\n")
      cat(rep("-", w), sep = "", "\n")
      cat(
        fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
        fc("Sum Sq", w4), fs(), fc("ESS", w5), fs(), fc("R-Sq", w6), fs(),
        fc("Adj. R-Sq", w7), "\n"
      )
      cat(rep("-", w), sep = "", "\n")

      for (i in seq_len(ln)) {
        cat(
          fl(da[i, 1], w1), fs(), 
          fg(1, w2), fs(), 
          fg(format(round(da[i, 2], 3), nsmall = 3), w3), fs(),
          fg(format(round(da[i, 4], 3), nsmall = 3), w4), fs(), 
          fg(format(round(da[i, 3], 3), nsmall = 3), w5), fs(),
          fg(format(round(da[i, 5], 3), nsmall = 3), w6), fs(),
          fg(format(round(da[i, 6], 3), nsmall = 3), w7), "\n"
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

      if (progress) {
        cat(paste("=>", tail(preds, n = 1), "added"), "\n")
      }

      if (details) {
        cat("Step  =>", all_step, "\n")
        cat("Added =>", tail(preds, n = 1), "\n")
        cat("Model =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
        cat("AIC   =>", maic, "\n\n")
      }

      if (lpreds > 1) {

        aics <- c()
        ess  <- c()
        rss  <- c()
        rsq  <- c()
        arsq <- c()
        j    <- 1

        for (i in len_inc:lpreds) {

          preda <- preds[-i]

          m <- ols_regress(paste(response, "~", paste(preda, collapse = " + ")), data = l)

          aics[j] <- ols_aic(m$model)
          ess[j]  <- m$ess
          rss[j]  <- m$rss
          rsq[j]  <- m$rsq
          arsq[j] <- m$adjr

          j <- j + 1
        }

        da <- data.frame(predictors = preds[len_inc: lpreds], aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)

        if (details) {
          w1 <- max(nchar("Predictor"), nchar(preds))
          w2 <- 2
          w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
          w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
          w5 <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
          w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
          w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
          w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
          ln <- length(aics)

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
              fl(da[i, 1], w1), fs(), 
              fg(1, w2), fs(), 
              fg(format(round(da[i, 2], 3), nsmall = 3), w3), fs(),
              fg(format(round(da[i, 4], 3), nsmall = 3), w4), fs(), 
              fg(format(round(da[i, 3], 3), nsmall = 3), w5), fs(),
              fg(format(round(da[i, 5], 3), nsmall = 3), w6), fs(),
              fg(format(round(da[i, 6], 3), nsmall = 3), w7), "\n"
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
          var_index <- c(var_index, preds[minc2 + length(include)])
          method    <- c(method, tech[2])
          all_step  <- all_step + 1

          if (progress) {
            cat(paste("=>", preds[minc2 + length(include)], "removed"), "\n")
          }

          preds <- preds[-(minc2 + length(include))]
          lpreds <- length(preds)

          if (details) {
            cat("Step    =>", all_step, "\n")
            cat("Removed =>", preds[minc2 + length(include)], "\n")
            cat("Model   =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
            cat("AIC     =>", maic, "\n\n")
          }
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      if (progress || details) {
        cat("\n")
        cat("No more variables to be added or removed.")
        cat("\n")
      }
      break
    }
  }

  if (details) {
    cat("\n")
    cat("Variables Selected:", "\n\n")
    for (i in seq_len(length(preds))) {
      cat(paste("=>", preds[i]), "\n")
    }
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  metrics     <- data.frame(step     = seq_len(all_step),
                            variable = var_index,
                            method   = method,
                            r2       = lrsq,
                            adj_r2   = larsq,
                            aic      = laic, 
                            rss      = lrss, 
                            ess      = less)

  out <- list(metrics = metrics,
              model   = final_model,
              others   = list(base_model = base_model))

  class(out) <- "ols_step_both_aic"

  return(out)
}

#' @export
#'
print.ols_step_both_aic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_stepaic_both(x)
  } else {
    print("No variables have been added to or removed from the model.")
  }
}

#' @rdname ols_step_both_aic
#' @export
#'
plot.ols_step_both_aic <- function(x, print_plot = TRUE, details = TRUE, ...) {

  tx  <- NULL
  a   <- NULL
  b   <- NULL

  step <- x$metrics$step
  aic  <- x$metrics$aic

  # text annotation
  if (details) {
    x$metrics$text <- ifelse(x$metrics$method == "addition", 
                           paste0("[+", x$metrics$variable, ", ", round(x$metrics$aic, 2), "]"), 
                           paste0("[-", x$metrics$variable, ", ", round(x$metrics$aic, 2), "]"))
    pred <- x$metrics$text
  } else {
    x$metrics$text <- ifelse(x$metrics$method == "addition", 
                             paste0("+", x$metrics$variable),
                             paste0("-", x$metrics$variable))
    pred <- x$metrics$text
  }
  
  y     <- step
  xloc  <- y 
  yloc  <- aic 
  xmin  <- min(y) - 0.4
  xmax  <- max(y) + 1
  ymin  <- min(aic) - (min(aic) * 0.05)
  ymax  <- max(aic) + (max(aic) * 0.05)

  d2 <- data.frame(x = xloc, y = yloc, tx = pred)
  d  <- data.frame(a = y, b = aic)

  # metric info
  base_model_aic  <- round(ols_aic(x$others$base_model), 3)
  final_model_aic <- round(ols_aic(x$model), 3)
  metric_info <- paste0("Base Model AIC  : ", format(base_model_aic, nsmall = 3), "\n",
                        "Final Model AIC : ", format(final_model_aic, nsmall = 3))

  p <-
    ggplot(d, aes(x = a, y = b)) + 
    geom_line(color = "blue") +
    geom_point(color = "blue", 
               shape = 1, 
               size = 2) + 
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + 
    xlab("Step") + 
    ylab("AIC") +
    ggtitle("Stepwise AIC Both Direction Selection") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), size = 3, 
              hjust = "left", vjust = "bottom", nudge_x = 0.05) +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
             family = "serif", fontface = "italic", size = 3,
             label = metric_info)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
