#' Stepwise backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on p values, in a stepwise manner until there is
#' no variable left to remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param prem p value; variables with p more than \code{prem} will be removed
#'   from the model.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_backward_p}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other inputs.
#'
#' @return \code{ols_step_backward_p} returns an object of class \code{"ols_step_backward_p"}.
#' An object of class \code{"ols_step_backward_p"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{steps}{total number of steps}
#' \item{removed}{variables removed from the model}
#' \item{rsquare}{coefficient of determination}
#' \item{aic}{akaike information criteria}
#' \item{sbc}{bayesian information criteria}
#' \item{sbic}{sawa's bayesian information criteria}
#' \item{adjr}{adjusted r-square}
#' \item{rmse}{root mean square error}
#' \item{mallows_cp}{mallow's Cp}
#' \item{indvar}{predictors}
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @section Deprecated Function:
#' \code{ols_step_backward()} has been deprecated. Instead use \code{ols_step_backward_p()}.
#'
#' @examples
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward_p(model)
#'
#' # stepwise backward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_backward_p(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_backward_p <- function(model, ...) UseMethod("ols_step_backward_p")

#' @export
#' @rdname ols_step_backward_p
#'
ols_step_backward_p.default <- function(model, prem = 0.3, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- TRUE
  }

  check_model(model)
  check_logic(details)
  check_values(prem, 0, 1)
  check_npredictors(model, 3)


  l        <- model$model
  nam      <- colnames(attr(model$terms, "factors"))
  response <- names(model$model)[1]
  preds    <- nam
  cterms   <- preds
  ilp      <- length(preds)
  end      <- FALSE
  step     <- 0
  rpred    <- c()
  rsq      <- c()
  adjrsq   <- c()
  aic      <- c()
  sbic     <- c()
  sbc      <- c()
  cp       <- c()
  rmse     <- c()

  if (progress) {
    cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
    cat(rep("-", 27), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")

    cat("We are eliminating variables based on p value...")
    cat("\n")

    cat("\n")
    if (!details) {
      cat("Variables Removed:", "\n\n")
    }
  }

  while (!end) {
    m     <- lm(paste(response, "~", paste(preds, collapse = " + ")), l)
    m_sum <- Anova(m)
    pvals <- m_sum$`Pr(>F)`
    maxp  <- which(pvals == max(pvals, na.rm = TRUE))

    suppressWarnings(
      if (pvals[maxp] > prem) {

        step   <- step + 1
        rpred  <- c(rpred, preds[maxp])
        preds  <- preds[-maxp]
        lp     <- length(rpred)
        fr     <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        rsq    <- c(rsq, fr$rsq)
        adjrsq <- c(adjrsq, fr$adjr)
        aic    <- c(aic, ols_aic(fr$model))
        sbc    <- c(sbc, ols_sbc(fr$model))
        sbic   <- c(sbic, ols_sbic(fr$model, model))
        cp     <- c(cp, ols_mallows_cp(fr$model, model))
        rmse   <- c(rmse, sqrt(fr$ems))

        if (progress) {
          if (interactive()) {
            cat("x", tail(rpred, n = 1), "\n")
          } else {
            cat(paste("-", tail(rpred, n = 1)), "\n")
          }
        }

        if (details) {
          cat("\n")
          cat(paste("Backward Elimination: Step", step, "\n\n"), paste("Variable", rpred[lp], "Removed"), "\n\n")
          m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
          print(m)
          cat("\n\n")
        }
      } else {
        end <- TRUE
        if (progress) {
          cat("\n")
          cat(paste0("No more variables satisfy the condition of p value = ", prem))
          cat("\n")
        }
      }
    )
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

  out <- list(mallows_cp = cp,
              removed    = rpred,
              rsquare    = rsq,
              indvar     = cterms,
              steps      = step,
              sbic       = sbic,
              adjr       = adjrsq,
              rmse       = rmse,
              aic        = aic,
              sbc        = sbc,
              model      = final_model)

  class(out) <- "ols_step_backward_p"

  return(out)
}

#' @export
#'
print.ols_step_backward_p <- function(x, ...) {
  if (x$steps > 0) {
    print_step_backward(x)
  } else {
    print("No variables have been removed from the model.")
  }
}



#' @export
#' @rdname ols_step_backward_p
#'
plot.ols_step_backward_p <- function(x, model = NA, print_plot = TRUE, ...) {

  a <- NULL
  b <- NULL

  y <- seq_len(x$steps)

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


#' @export
#' @rdname ols_step_backward_p
#' @usage NULL
#'
ols_step_backward <- function(model, prem = 0.3, details = FALSE, ...) {
  .Deprecated("ols_step_backward_p()")
}
