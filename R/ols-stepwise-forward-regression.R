#' Stepwise forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on p values, in a stepwise manner until there is
#' no variable left to enter any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param penter p value; variables with p value less than \code{penter} will
#'   .enter into the model
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_forward_p}.
#' @param ... Other arguments.
#'
#' @return \code{ols_step_forward_p} returns an object of class \code{"ols_step_forward_p"}.
#' An object of class \code{"ols_step_forward_p"} is a list containing the
#' following components:
#'
#' \item{steps}{number of steps}
#' \item{predictors}{variables added to the model}
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
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#'
#' @section Deprecated Function:
#' \code{ols_step_forward()} has been deprecated. Instead use \code{ols_step_forward_p()}.
#'
#' @examples
#' \dontrun{
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_p(model)
#' }
#'
#' \dontrun{
#' # stepwise forward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_forward_p(model)
#' plot(k)
#' }
#'
#' @importFrom stats qt
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_forward_p <- function(model, ...) UseMethod("ols_step_forward_p")

#' @export
#' @rdname ols_step_forward_p
#'
ols_step_forward_p.default <- function(model, penter = 0.3, details = FALSE, ...) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  if ((penter < 0) | (penter > 1)) {
    stop("p value for entering variables into the model must be between 0 and 1.", call. = FALSE)
  }

  if (!is.logical(details)) {
    stop("details must be either TRUE or FALSE", call. = FALSE)
  }

  if (length(model$coefficients) < 3) {
    stop("Please specify a model with at least 2 predictors.", call. = FALSE)
  }

  l        <- mod_sel_data(model)
  df       <- nrow(l) - 2
  tenter   <- qt(1 - (penter) / 2, df)
  n        <- ncol(l)
  nam      <- colnames(attr(model$terms, "factors"))
  response <- names(model$model)[1]
  all_pred <- nam
  cterms   <- all_pred
  mlen_p   <- length(all_pred)

  step     <- 1
  ppos     <- step + 1
  preds    <- c()
  pvals    <- c()
  tvals    <- c()
  rsq      <- c()
  adjrsq   <- c()
  aic      <- c()
  bic      <- c()
  cp       <- c()

  cat(format("Forward Selection Method", justify = "left", width = 27), "\n")
  cat(rep("-", 27), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(nam))) {
    cat(paste0(i, ". ", nam[i]), "\n")
  }
  cat("\n")

  cat(crayon::bold$red("We are selecting variables based on p value..."))
  cat("\n")

  cat("\n")
  if (!details) {
    cat("Variables Entered:", "\n\n")
  }


  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), l)
    pvals[i] <- m$pvalues[ppos]
    tvals[i] <- m$tvalues[ppos]
  }

  minp   <- which(pvals == min(pvals))
  tvals  <- abs(tvals)
  maxt   <- which(tvals == max(tvals))
  preds  <- all_pred[maxt]
  lpreds <- length(preds)
  fr     <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
  rsq    <- fr$rsq
  adjrsq <- fr$adjr
  cp     <- ols_mallows_cp(fr$model, model)
  aic    <- ols_aic(fr$model)
  sbc    <- ols_sbc(fr$model)
  sbic   <- ols_sbic(fr$model, model)
  rmse   <- sqrt(fr$ems)

  if (details == TRUE) {
    cat("\n")
    cat(paste("Forward Selection: Step", step), "\n\n")
  }

  if (interactive()) {
    cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
  } else {
    cat(paste("-", dplyr::last(preds)), "\n")
  }

  if (details == TRUE) {
    cat("\n")
    m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
    print(m)
    cat("\n\n")
  }

  while (step < mlen_p) {

    all_pred <- all_pred[-maxt]
    len_p    <- length(all_pred)
    ppos     <- ppos + length(maxt)
    pvals    <- c()
    tvals    <- c()

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      m <- ols_regress(paste(response, "~",
                             paste(predictors, collapse = " + ")), l)
      pvals[i] <- m$pvalues[ppos]
      tvals[i] <- m$tvalues[ppos]
    }

    minp  <- which(pvals == min(pvals))
    tvals <- abs(tvals)
    maxt  <- which(tvals == max(tvals))

    if (tvals[maxt] >= tenter) {

      step   <- step + 1
      preds  <- c(preds, all_pred[maxt])
      lpreds <- length(preds)
      fr     <- ols_regress(paste(response, "~",
                                  paste(preds, collapse = " + ")), l)
      rsq    <- c(rsq, fr$rsq)
      adjrsq <- c(adjrsq, fr$adjr)
      aic    <- c(aic, ols_aic(fr$model))
      sbc    <- c(sbc, ols_sbc(fr$model))
      sbic   <- c(sbic, ols_sbic(fr$model, model))
      cp     <- c(cp, ols_mallows_cp(fr$model, model))
      rmse   <- c(rmse, sqrt(fr$ems))

      if (details == TRUE) {
        cat("\n")
        cat(paste("Forward Selection: Step", step), "\n\n")
      }

      if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
      } else {
        cat(paste("-", dplyr::last(preds)), "\n")
      }

      if (details == TRUE) {
        cat("\n")
        m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        print(m)
        cat("\n\n")
      }
    } else {
      cat("\n")
      cat(crayon::bold$red("No more variables to be added."))
      break
    }
  }

  prsq <- c(rsq[1], diff(rsq))

  if (details == TRUE) {
    cat("\n\n")
    cat("Variables Entered:", "\n\n")
    for (i in seq_len(length(preds))) {
      if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(preds[i]), "\n")
      } else {
        cat(paste("+", preds[i]), "\n")
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


  out <- list(predictors = preds,
              mallows_cp = cp,
              indvar     = cterms,
              rsquare    = rsq,
              steps      = step,
              sbic       = sbic,
              adjr       = adjrsq,
              rmse       = rmse,
              aic        = aic,
              sbc        = sbc)

  class(out) <- "ols_step_forward_p"

  return(out)
}

#' @export
#'
print.ols_step_forward_p <- function(x, ...) {
  if (x$steps > 0) {
    print_step_forward(x)
  } else {
    print("No variables have been added to the model.")
  }
}

#' @export
#' @rdname ols_step_forward_p
#'
plot.ols_step_forward_p <- function(x, model = NA, ...) {

  a <- NULL
  b <- NULL

  y <- seq_len(length(x$rsquare))

  d1 <- tibble(a = y, b = x$rsquare)
  d2 <- tibble(a = y, b = x$adjr)
  d3 <- tibble(a = y, b = x$mallows_cp)
  d4 <- tibble(a = y, b = x$aic)
  d5 <- tibble(a = y, b = x$sbic)
  d6 <- tibble(a = y, b = x$sbc)

  p1 <- plot_stepwise(d1, "R-Square")
  p2 <- plot_stepwise(d2, "Adj. R-Square")
  p3 <- plot_stepwise(d3, "C(p)")
  p4 <- plot_stepwise(d4, "AIC")
  p5 <- plot_stepwise(d5, "SBIC")
  p6 <- plot_stepwise(d6, "SBC")

  # grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = "Stepwise Forward Regression")
  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3,
                  plot_4 = p4, plot_5 = p5, plot_6 = p6)
  result <- marrangeGrob(myplots, nrow = 2, ncol = 2)
  result

}


#' @export
#' @rdname ols_step_forward_p
#' @usage NULL
#'
ols_step_forward <- function(model, penter = 0.3, details = FALSE, ...) {
  .Deprecated("ols_step_forward_p()")
}
