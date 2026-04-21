#' @title Regressors selection
#' @description
#' The function allows to choose regressors based on multiple criteria as AIC, RMSE etc
#' @param model is a linear regression model
#' @param pval p value; variables with p value less than \code{pval} will
#'   enter into the model
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param ... other arguments
#' @param progress Logical; if TRUE, will display variable selection progress.
#' @param metric statistical metrics used to estimate the best model
#' @examples
#' data(macroKZ)
#' model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
#' regsel_f(model)
#' @references Hebbali, Aravind. Published 2020-02-10. olssr package
#' @import stats
#' @import olsrr
#' @importFrom car Anova
#' @importFrom utils tail
#' @rdname regsel_f
#' @export

regsel_f<-function(model, pval = 0.3, metric="adjr"&"aic",progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- TRUE
  }

  l        <- eval(model$call$data)
  nam      <- colnames(attr(model$terms, "factors"))
  df       <- nrow(l) - 2
  tenter   <- qt(1 - (pval) / 2, df)
  n        <- ncol(l)
  response <- names(model$model)[1]
  all_pred <- nam
  cterms   <- all_pred
  mlen_p   <- length(all_pred)

  step     <- 1
  ppos     <- step
  preds    <- c()
  rped     <- c()
  pvals    <- c()
  tvals    <- c()
  rsq      <- c()
  adjrsq   <- c()
  aic      <- c()
  bic      <- c()
  cp       <- c()

  if (progress) {
    cat(format("Forward Selection Method", justify = "left", width = 27), "\n")
    cat(rep("-", 27), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste0(i, ". ", nam[i]), "\n")
    }
    cat("\n")

    cat("We are selecting variables based on p value...")
    cat("\n")

    cat("\n")
    if (!details) {
      cat("Variables Entered:", "\n\n")
    }
  }


  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    m <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
    m_sum <- Anova(m)
    pvals[i] <- m_sum$`Pr(>F)`[ppos]
  }

  minp   <- which(pvals == min(pvals, na.rm = TRUE))
  rped   <- c(rped, preds[minp])
  preds  <- all_pred[minp]
  lpreds <- length(preds)
  fr     <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
  rsq    <- fr$rsq
  adjrsq <- fr$adjr
  cp     <- ols_mallows_cp(fr$model, model)
  aic    <- ols_aic(fr$model)
  sbc    <- ols_sbc(fr$model)
  sbic   <- ols_sbic(fr$model, model)
  rmse   <- sqrt(fr$ems)

  if (details) {
    cat("\n")
    cat(paste("Forward Selection: Step", step), "\n\n")
  }

  if (progress) {
    if (interactive()) {
      cat("+", tail(preds, n = 1), "\n")
    } else {
      cat(paste("-", tail(preds, n = 1)), "\n")
    }
  }

  if (details) {
    cat("\n")
    m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
    print(m)
    cat("\n\n")
  }

  while (step < mlen_p) {

    all_pred <- all_pred[-minp]
    len_p    <- length(all_pred)
    ppos     <- ppos + length(minp)
    pvals    <- c()
    tvals    <- c()

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      m          <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
      m_sum      <- Anova(m)
      pvals[i]   <- m_sum$`Pr(>F)`[ppos]
    }

    minp  <- which(pvals == min(pvals, na.rm = TRUE))

    if (pvals[minp] <= pval) {

      step   <- step + 1
      preds  <- c(preds, all_pred[minp])
      lpreds <- length(preds)
      fr     <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
      rsq    <- c(rsq, fr$rsq)
      adjrsq <- c(adjrsq, fr$adjr)
      aic    <- c(aic, ols_aic(fr$model))
      sbc    <- c(sbc, ols_sbc(fr$model))
      sbic   <- c(sbic, ols_sbic(fr$model, model))
      cp     <- c(cp, ols_mallows_cp(fr$model, model))
      rmse   <- c(rmse, sqrt(fr$ems))

      if (details) {
        cat("\n")
        cat(paste("Forward Selection: Step", step), "\n\n")
      }

      if (progress) {
        if (interactive()) {
          cat("+", tail(preds, n = 1), "\n")
        } else {
          cat(paste("-", tail(preds, n = 1)), "\n")
        }
      }

      if (details) {
        cat("\n")
        m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        print(m)
        cat("\n\n")
      }
    } else {
      if (progress) {
        cat("\n")
        cat("No more variables to be added.")
      }
      break
    }
  }

  prsq <- c(rsq[1], diff(rsq))

  if (details) {
    cat("\n\n")
    cat("Variables Entered:", "\n\n")
    for (i in seq_len(length(preds))) {
      if (details) {
        cat("+", preds[i], "\n")
      } else {
        cat(paste("+", preds[i]), "\n")
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

  out <- list(predictors = preds,
              removed=rped,
              mallows_cp = cp,
              indvar     = cterms,
              rsquare    = rsq,
              steps      = step,
              sbic       = sbic,
              adjr       = adjrsq,
              rmse       = rmse,
              aic        = aic,
              sbc        = sbc,
              model      = final_model)

  class(out) <- "ols_step_forward_p"

  return(out)
}

#' @export
#' @noRd
#'
#'
print.regsel_f <- function(x, ...) {
  if (x$steps > 0) {
    print_step_forward(x)
  } else {
    print("No variables have been added to the model.")
  }
}

#' @importFrom gridExtra marrangeGrob
#' @export
#' @noRd
#'
plot.regsel_f <- function(x, model=NA, print_plot = TRUE, ...) {

  a <- NULL
  b <- NULL

  y <- seq_len(length(x$rsquare))

  d1 <- data.frame(a = y, b = x$rsquare)
  d2 <- data.frame(a = y, b = x$adjr)
  d3 <- data.frame(a = y, b = x$mallows_cp)
  d4 <- data.frame(a = y, b = x$aic)
  d5 <- data.frame(a = y, b = x$sbic)
  d6 <- data.frame(a = y, b = x$sbc)

  p1 <- plot_stepwise(d1, "R-Square")
  p2 <- plot_stepwise(d2, "Adj. R-Square")
  p3 <- plot_stepwise(d3, "C(p)")
  p4 <- plot_stepwise(d4, "AIC")
  p5 <- plot_stepwise(d5, "SBIC")
  p6 <- plot_stepwise(d6, "SBC")

  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3,
                  plot_4 = p4, plot_5 = p5, plot_6 = p6)

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}
