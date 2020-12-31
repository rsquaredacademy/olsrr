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
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
  #' each step.
#' @param x An object of class \code{ols_step_both_p}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#' @return \code{ols_step_both_p} returns an object of class \code{"ols_step_both_p"}.
#' An object of class \code{"ols_step_both_p"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
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
#' @section Deprecated Function:
#' \code{ols_stepwise()} has been deprecated. Instead use \code{ols_step_both_p()}.
#'
#' @examples
#' # stepwise regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_both_p(model)
#'
#' # stepwise regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_both_p(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' @family variable selection_procedures
#'
#' @export
#'
ols_step_both_p <- function(model, ...) UseMethod("ols_step_both_p")

#' @export
#' @rdname ols_step_both_p
#'
ols_step_both_p.default <- function(model, pent = 0.1, prem = 0.3, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- TRUE
  }

  check_model(model)
  check_logic(details)
  check_values(pent, 0, 1)
  check_values(prem, 0, 1)
  check_npredictors(model, 3)

  response <- names(model$model)[1]
  l        <- model$model
  nam      <- colnames(attr(model$terms, "factors"))
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
  step    <- 0
  ppos    <- step + 1
  rsq     <- c()
  adjrsq  <- c()
  aic     <- c()
  sbc     <- c()
  sbic    <- c()
  rmse    <- c()
  cp      <- c()
  f       <- c()
  fp      <- c()
  method  <- c()

  if (progress) {
    cat(format("Stepwise Selection Method", justify = "left", width = 27), "\n")
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
      cat("Variables Entered/Removed:", "\n\n")
    }
  }

  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    m          <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
    m_sum      <- Anova(m)
    pvals[i]   <- m_sum$`Pr(>F)`[ppos]
  }

  minp    <- which(pvals == min(pvals, na.rm = TRUE))
  preds   <- all_pred[minp]
  lpreds  <- length(preds)
  tech    <- c("addition", "removal")

  aic_model <- ols_step_forward_aic(model)

  for (i in seq_len(lpreds)) {

    step <- step + 1

    if (details) {
      cat("\n")
      cat(paste("Stepwise Selection: Step", step), "\n\n")
    }

    npreds  <- aic_model$predictors[1:i]
    fr      <- ols_regress(paste(response, "~", paste(npreds, collapse = " + ")), l)
    rsq     <- c(rsq, fr$rsq)
    adjrsq  <- c(adjrsq, fr$adjr)
    cp      <- c(cp, ols_mallows_cp(fr$model, model))
    aic     <- c(aic, ols_aic(fr$model))
    sbc     <- c(sbc, ols_sbc(fr$model))
    sbic    <- c(sbic, ols_sbic(fr$model, model))
    rmse    <- c(rmse, fr$rmse)
    betas   <- append(betas, fr$betas)
    lbetas  <- append(lbetas, length(fr$betas))
    pvalues <- append(pvalues, fr$pvalues)

    method  <- c(method, tech[1])

    if (progress) {
      if (interactive()) {
        cat("+", tail(npreds, n = 1), "\n")
      } else {
        cat(paste("-", tail(npreds, n = 1), "added"), "\n")
      }
    }

    if (details) {
      cat("\n")
      m <- ols_regress(paste(response, "~", paste(npreds, collapse = " + ")), l)
      print(m)
      cat("\n\n")
    }
  }

  # fr      <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
  # rsq     <- fr$rsq
  # adjrsq  <- fr$adjr
  # cp      <- ols_mallows_cp(fr$model, model)
  # aic     <- ols_aic(fr$model)
  # sbc     <- ols_sbc(fr$model)
  # sbic    <- ols_sbic(fr$model, model)
  # rmse    <- fr$rmse
  # betas   <- append(betas, fr$betas)
  # lbetas  <- append(lbetas, length(fr$betas))
  # pvalues <- append(pvalues, fr$pvalues)

  # if (details) {
  #   cat("\n")
  #   m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
  #   print(m)
  #   cat("\n\n")
  # }

  all_step  <- lpreds
  preds     <- npreds
  var_index <- preds

  while (step < mlen_p) {

    all_pred <- all_pred[-minp]
    len_p    <- length(all_pred)

    if (len_p == 0) {
      break
    }

    step     <- step + lpreds
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

    if (pvals[minp] <= pent) {

      preds     <- c(preds, all_pred[minp])
      var_index <- c(var_index, all_pred[minp])
      method    <- c(method, tech[1])
      lpreds    <- length(preds)
      all_step  <- all_step + 1
      fr        <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
      rsq       <- c(rsq, fr$rsq)
      adjrsq    <- c(adjrsq, fr$adjr)
      aic       <- c(aic, ols_aic(fr$model))
      sbc       <- c(sbc, ols_sbc(fr$model))
      sbic      <- c(sbic, ols_sbic(fr$model, model))
      cp        <- c(cp, ols_mallows_cp(fr$model, model))
      rmse      <- c(rmse, fr$rmse)
      betas     <- append(betas, fr$betas)
      lbetas    <- append(lbetas, length(fr$betas))
      pvalues   <- append(pvalues, fr$pvalues)

      if (details) {
        cat("\n")
        cat(paste("Stepwise Selection: Step", step), "\n\n")
      }

      if (progress) {
        if (interactive()) {
          cat("+", tail(preds, n = 1), "\n")
        } else {
          cat(paste("-", tail(preds, n = 1), "added"), "\n")
        }
      }

      if (details) {
        cat("\n")
        m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        print(m)
        cat("\n\n")
      }


      if (details) {
        cat("\n")
        m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        print(m)
        cat("\n\n")
      }

      m2      <- lm(paste(response, "~", paste(preds, collapse = " + ")), l)
      m2_sum  <- Anova(m2)
      pvals_r <- m2_sum$`Pr(>F)`
      maxp    <- which(pvals_r == max(pvals_r, na.rm = TRUE))
      
      if (pvals_r[maxp] > prem) {

        var_index <- c(var_index, preds[maxp])
        lvar      <- length(var_index)
        method    <- c(method, tech[2])
        preds     <- preds[-maxp]
        all_step  <- all_step + 1
        ppos      <- ppos - length(maxp)
        fr        <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        rsq       <- c(rsq, fr$rsq)
        adjrsq    <- c(adjrsq, fr$adjr)
        aic       <- c(aic, ols_aic(fr$model))
        sbc       <- c(sbc, ols_sbc(fr$model))
        sbic      <- c(sbic, ols_sbic(fr$model, model))
        cp        <- c(cp, ols_mallows_cp(fr$model, model))
        rmse      <- c(rmse, fr$rmse)
        betas     <- append(betas, fr$betas)
        lbetas    <- append(lbetas, length(fr$betas))
        pvalues   <- append(pvalues, fr$pvalues)

        if (details) {
          cat("\n")
          cat(paste("Stepwise Selection: Step", all_step), "\n\n")
        }

        if (progress) {
          if (interactive()) {
            cat("x", tail(var_index, n = 1), "\n")
          } else {
            cat(paste("-", tail(var_index, n = 1), "added"), "\n")
          }
        }

        if (details) {
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
      if (progress) {
        cat("\n")
        cat("No more variables to be added/removed.")
        cat("\n")
      }
      break
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
  
  metrics     <- data.frame(r2 = rsq[all_step], adj_r2 = adjrsq[all_step], aic = aic[all_step], 
                            sbic = sbic[all_step], sbc = sbc[all_step], 
                            mallows_cp = cp[all_step], rmse = rmse[all_step])

  beta_pval <- data.frame(
    model     = rep(seq_len(all_step), lbetas),
    predictor = names(betas),
    beta      = betas,
    pval      = pvalues
  )

  out <- list(
    adjr       = adjrsq,
    aic        = aic,
    beta_pval  = beta_pval,
    betas      = betas,
    indvar     = cterms,
    lbetas     = lbetas,
    mallows_cp = cp,
    method     = method,
    metrics    = metrics,
    model      = final_model,
    orders     = var_index,
    predictors = preds,
    pvalues    = pvalues,
    rmse       = rmse,
    rsquare    = rsq,
    sbc        = sbc,
    sbic       = sbic,
    steps      = all_step)

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
plot.ols_step_both_p <- function(x, model = NA, print_plot = TRUE, ...) {

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

plot_stepwise <- function(d, title) {

  a <- NULL
  b <- NULL

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
ols_stepwise <- function(model, pent = 0.1, prem = 0.3, details = FALSE, ...) {
  .Deprecated("ols_step_both_p()")
}
