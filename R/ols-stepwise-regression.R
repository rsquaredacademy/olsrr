#' Stepwise regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering and removing predictors based on p values, in a stepwise manner
#' until there is no variable left to enter or remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param pent p value; variables with p value less than \code{pent} will enter
#'   into the model.
#' @param prem p value; variables with p more than \code{prem} will be removed
#'   from the model.
#' @param details Logical; if \code{TRUE}, will print the regression result at
  #' each step.
#' @param x An object of class \code{ols_step_both_p}.
#' @param ... Other arguments.
#' @return \code{ols_step_both_p} returns an object of class \code{"ols_step_both_p"}.
#' An object of class \code{"ols_step_both_p"} is a list containing the
#' following components:
#'
#' \item{orders}{candidate predictor variables according to the order by which they were added or removed from the model}
#' \item{method}{addition/deletion}
#' \item{steps}{total number of steps}
#' \item{predictors}{variables retained in the model (after addition)}
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
#' # stepwise regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_both_p(model)
#' }
#'
#' \dontrun{
#' # stepwise regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_both_p(model)
#' plot(k)
#' }
#'
#' @family variable selection_procedures
#'
#' @export
#'
ols_step_both_p <- function(model, ...) UseMethod("ols_step_both_p")

#' @export
#' @rdname ols_step_both_p
#'
ols_step_both_p.default <- function(model, pent = 0.1, prem = 0.3, details = FALSE, ...) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  if ((pent < 0) | (pent > 1)) {
    stop("p value for entering variables into the model must be between 0 and 1.", call. = FALSE)
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

  response <-
    model %>%
    use_series(model) %>%
    names() %>%
    extract(1)

  l        <- mod_sel_data(model)
  nam      <- coeff_names(model)
  df       <- nrow(l) - 2
  tenter   <- qt(1 - (pent) / 2, df)
  trem     <- qt(1 - (prem) / 2, df)
  n        <- ncol(l)
  all_pred <- nam
  cterms   <- all_pred
  mlen_p   <- length(all_pred)


  pvalues <- c()
  lbetas  <- c()
  betas   <- c()
  preds   <- c()
  pvals   <- c()
  tvals   <- c()
  step    <- 1
  ppos    <- step + 1
  rsq     <- c()
  cp      <- c()
  f       <- c()
  fp      <- c()




  cat(format("Stepwise Selection Method", justify = "left", width = 27), "\n")
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
    cat("Variables Entered/Removed:", "\n\n")
  }


  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), l)
    pvals[i] <- m$pvalues[ppos]
    tvals[i] <- m$tvalues[ppos]
  }

  minp    <- which(pvals == min(pvals))
  tvals   <- abs(tvals)
  maxt    <- which(tvals == max(tvals))
  preds   <- all_pred[maxt]
  lpreds  <- length(preds)
  fr      <- ols_regress(paste(response, "~",
                               paste(preds, collapse = " + ")), l)
  rsq     <- fr$rsq
  adjrsq  <- fr$adjr
  cp      <- ols_mallows_cp(fr$model, model)
  aic     <- ols_aic(fr$model)
  sbc     <- ols_sbc(fr$model)
  sbic    <- ols_sbic(fr$model, model)
  rmse    <- sqrt(fr$ems)
  betas   <- append(betas, fr$betas)
  lbetas  <- append(lbetas, length(fr$betas))
  pvalues <- append(pvalues, fr$pvalues)

  if (details == TRUE) {
    cat("\n")
    cat(paste("Stepwise Selection: Step", step), "\n\n")
  }

  if (interactive()) {
    cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
  } else {
    cat(paste("-", dplyr::last(preds), "added"), "\n")
  }


  if (details == TRUE) {
    cat("\n")
    m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
    print(m)
    cat("\n\n")
  }

  all_step  <- 1
  tech      <- c("addition", "removal")
  var_index <- preds
  method    <- tech[1]

  while (step < mlen_p) {

    all_pred <- all_pred[-maxt]
    len_p    <- length(all_pred)
    step     <- step + 1
    ppos     <- ppos + length(maxt)
    pvals    <- c()
    tvals    <- c()

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      m          <- ols_regress(paste(response, "~",
                                      paste(predictors, collapse = " + ")), l)
      pvals[i]   <- m$pvalues[ppos]
      tvals[i]   <- m$tvalues[ppos]
    }

    minp  <- which(pvals == min(pvals))
    tvals <- abs(tvals)
    maxt  <- which(tvals == max(tvals))

    if (tvals[maxt] >= tenter) {

      preds     <- c(preds, all_pred[maxt])
      var_index <- c(var_index, all_pred[maxt])
      method    <- c(method, tech[1])
      lpreds    <- length(preds)
      all_step  <- all_step + 1
      fr        <- ols_regress(paste(response, "~",
                                     paste(preds, collapse = " + ")), l)
      rsq       <- c(rsq, fr$rsq)
      adjrsq    <- c(adjrsq, fr$adjr)
      aic       <- c(aic, ols_aic(fr$model))
      sbc       <- c(sbc, ols_sbc(fr$model))
      sbic      <- c(sbic, ols_sbic(fr$model, model))
      cp        <- c(cp, ols_mallows_cp(fr$model, model))
      rmse      <- c(rmse, sqrt(fr$ems))
      betas     <- append(betas, fr$betas)
      lbetas    <- append(lbetas, length(fr$betas))
      pvalues   <- append(pvalues, fr$pvalues)

      if (details == TRUE) {
        cat("\n")
        cat(paste("Stepwise Selection: Step", step), "\n\n")
      }

      if (interactive()) {
        cat(crayon::green(clisymbols::symbol$tick), crayon::bold(dplyr::last(preds)), "\n")
      } else {
        cat(paste("-", dplyr::last(preds), "added"), "\n")
      }


      if (details == TRUE) {
        cat("\n")
        m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        print(m)
        cat("\n\n")
      }


      if (details == TRUE) {
        cat("\n")
        m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        print(m)
        cat("\n\n")
      }

      m2      <- ols_regress(paste(response, "~",
                                   paste(preds, collapse = " + ")), l)
      tvals_r <- abs(m2$tvalues[-1])
      mint    <- which(tvals_r == min(tvals_r))
      if (tvals_r[mint] < trem) {

        var_index <- c(var_index, preds[mint])
        lvar      <- length(var_index)
        method    <- c(method, tech[2])
        preds     <- preds[-mint]
        all_step  <- all_step + 1
        ppos      <- ppos - length(mint)
        fr        <- ols_regress(paste(response, "~",
                                       paste(preds, collapse = " + ")), l)
        rsq       <- c(rsq, fr$rsq)
        adjrsq    <- c(adjrsq, fr$adjr)
        aic       <- c(aic, ols_aic(fr$model))
        sbc       <- c(sbc, ols_sbc(fr$model))
        sbic      <- c(sbic, ols_sbic(fr$model, model))
        cp        <- c(cp, ols_mallows_cp(fr$model, model))
        rmse      <- c(rmse, sqrt(fr$ems))
        betas     <- append(betas, fr$betas)
        lbetas    <- append(lbetas, length(fr$betas))
        pvalues   <- append(pvalues, fr$pvalues)

        if (details == TRUE) {
          cat("\n")
          cat(paste("Stepwise Selection: Step", all_step), "\n\n")
        }

        if (interactive()) {
          cat(crayon::red(clisymbols::symbol$cross), crayon::bold(dplyr::last(var_index)), "\n")
        } else {
          cat(paste("-", dplyr::last(var_index), "added"), "\n")
        }


        if (details == TRUE) {
          cat("\n")
          m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
          print(m)
          cat("\n\n")
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      cat("\n")
      cat(crayon::bold$red(glue("No more variables to be added/removed.")))
      cat("\n")
      break
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

  beta_pval <- tibble(
    model     = rep(seq_len(all_step), lbetas),
    predictor = names(betas),
    beta      = betas,
    pval      = pvalues
  )

  out <- list(
    orders     = var_index,
    method     = method,
    steps      = all_step,
    predictors = preds,
    rsquare    = rsq,
    aic        = aic,
    sbc        = sbc,
    sbic       = sbic,
    adjr       = adjrsq,
    rmse       = rmse,
    mallows_cp = cp,
    indvar     = cterms,
    betas      = betas,
    lbetas     = lbetas,
    pvalues    = pvalues,
    beta_pval  = beta_pval
  )

  class(out) <- "ols_step_both_p"

  return(out)
}

#' @export
#'
print.ols_step_both_p <- function(x, ...) {
  if (x$steps > 0) {
    print_stepwise(x)
  } else {
    print("No variables have been added to or removed from the model.")
  }
}

#' @export
#' @rdname ols_step_both_p
#'
plot.ols_step_both_p <- function(x, model = NA, ...) {

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

  grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, top = "Stepwise Regression")

  result <- list(rsquare_plot     = p1,
                 adj_rsquare_plot = p2,
                 mallows_cp_plot  = p3,
                 aic_plot         = p4,
                 sbic_plot        = p5,
                 sbc_plot         = p6)

  invisible(result)

}

plot_stepwise <- function(d, title) {

  ggplot(d, aes(x = a, y = b)) +
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) +
    xlab("") + ylab("") + ggtitle(title) +
    theme(
      axis.ticks = element_blank()
    )

}


#' @export
#' @rdname ols_step_both_p
#' @usage NULL
#'
ols_stepwise <- function(model) {
  .Deprecated("ols_step_both_p()")
}
