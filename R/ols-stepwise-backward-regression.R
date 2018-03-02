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
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_backward_p}.
#' @param ... Other inputs.
#'
#' @return \code{ols_step_backward_p} returns an object of class \code{"ols_step_backward_p"}.
#' An object of class \code{"ols_step_backward_p"} is a list containing the
#' following components:
#'
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
#' @examples
#' \dontrun{
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward_p(model)
#' }
#'
#' \dontrun{
#' # stepwise backward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_backward_p(model)
#' plot(k)
#' }
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_backward_p <- function(model, ...) UseMethod("ols_step_backward_p")

#' @export
#' @rdname ols_step_backward_p
#'
ols_step_backward_p.default <- function(model, prem = 0.3, details = FALSE, ...) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS regression model.", call. = FALSE)
  }

  if ((prem < 0) | (prem > 1)) {
    stop("p value for removing variables from the model must be between 0 and 1.", call. = FALSE)
  }

  if (!is.logical(details)) {
    stop("details must be either TRUE or FALSE", call. = FALSE)
  }

  if (length(model$coefficients) < 3) {
    stop("Please specify a model with at least 2 predictors.", call. = FALSE)
  }


  l        <- mod_sel_data(model)
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

  cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
  cat(rep("-", 27), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(nam))) {
    cat(paste(i, ".", nam[i]), "\n")
  }
  cat("\n")

  cat(crayon::bold$red("We are eliminating variables based on p value..."))
  cat("\n")

  cat("\n")
  if (!details) {
    cat("Variables Removed:", "\n\n")
  }

  while (!end) {
    m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
    pvals <- m$pvalues[-1]
    maxp  <- which(pvals == max(pvals))

    suppressWarnings(
      if (pvals[maxp] > prem) {

        step   <- step + 1
        rpred  <- c(rpred, preds[maxp])
        preds  <- preds[-maxp]
        lp     <- length(rpred)
        fr     <- ols_regress(paste(response, "~",
                                paste(preds, collapse = " + ")), l)
        rsq    <- c(rsq, fr$rsq)
        adjrsq <- c(adjrsq, fr$adjr)
        aic    <- c(aic, ols_aic(fr$model))
        sbc    <- c(sbc, ols_sbc(fr$model))
        sbic   <- c(sbic, ols_sbic(fr$model, model))
        cp     <- c(cp, ols_mallows_cp(fr$model, model))
        rmse   <- c(rmse, sqrt(fr$ems))

        if (interactive()) {
          cat(crayon::red(clisymbols::symbol$cross), crayon::bold(dplyr::last(rpred)), "\n")
        } else {
          cat(paste("-", dplyr::last(rpred)), "\n")
        }

        if (details == TRUE) {
          cat("\n")
          cat(paste("Backward Elimination: Step", step, "\n\n"), paste("Variable", rpred[lp], "Removed"), "\n\n")
          m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
          print(m)
          cat("\n\n")
        }
      } else {
        end <- TRUE
        cat("\n")
        cat(crayon::bold$red(glue("No more variables satisfy the condition of p value = {prem}")))
        cat("\n")
      }
    )
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
  }

  cat("\n\n")
  cat("Final Model Output", "\n")
  cat(rep("-", 18), sep = "", "\n\n")

  fi <- ols_regress(
    paste(response, "~", paste(preds, collapse = " + ")),
    data = l
  )
  print(fi)

  out <- list(mallows_cp = cp,
              removed    = rpred,
              rsquare    = rsq,
              indvar     = cterms,
              steps      = step,
              sbic       = sbic,
              adjr       = adjrsq,
              rmse       = rmse,
              aic        = aic,
              sbc        = sbc)

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
plot.ols_step_backward_p <- function(x, model = NA, ...) {

  a <- NULL
  b <- NULL

  y <- seq_len(x$steps)

  d1 <- tibble(a = y, b = x$rsquare)
  d2 <- tibble(a = y, b = x$adjr)
  d3 <- tibble(a = y, b = x$mallows_cp)
  d4 <- tibble(a = y, b = x$aic)
  d5 <- tibble(a = y, b = x$sbic)
  d6 <- tibble(a = y, b = x$sbc)

  p1 <- plot_stepwise(d1, "R-Square") + theme(axis.text.x = element_blank())
  p2 <- plot_stepwise(d2, "Adj. R-Square") + theme(axis.text.x = element_blank())
  p3 <- plot_stepwise(d3, "C(p)") + theme(axis.text.x = element_blank())
  p4 <- plot_stepwise(d4, "AIC") + theme(axis.text.x = element_blank())
  p5 <- plot_stepwise(d5, "SBIC")
  p6 <- plot_stepwise(d6, "SBC")

  grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = "Stepwise Backward Regression")

  result <- list(rsquare_plot     = p1,
                 adj_rsquare_plot = p2,
                 mallows_cp_plot  = p3,
                 aic_plot         = p4,
                 sbic_plot        = p5,
                 sbc_plot         = p6)

  invisible(result)

}


#' @export
#' @rdname ols_step_backward_p
#' @usage NULL
#'
ols_step_backward <- function(model) {
  .Deprecated("ols_step_backward_p()")
}
