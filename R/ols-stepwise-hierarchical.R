#' Stepwise hierarchical selection
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on p values, in a stepwise hierarchical manner
#' until there is no variable left to enter any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param p_val p value; variables with p value less/higher than \code{p_val} will
#'   enter/exit the model.
#' @param forward Logical; if \code{TRUE}, performs forward selection else backward.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#'
#' @examples
#' model <- lm(y ~ ., data = surgical)
#' model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + alc_mod + age + gender, data = surgical)
#' model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
#'
#' @keywords internal
#'
#' @noRd
#'
ols_step_hierarchical <- function(model, p_val = 0.1, forward = TRUE, progress = FALSE, details = FALSE) {

  if (forward) {
    ols_step_hierarchical_forward(model, p_val, progress, details)
  } else {
    ols_step_hierarchical_backward(model, p_val, progress, details)
  }

}

ols_step_hierarchical_forward <- function(model, p_val = 0.1, progress = FALSE, details = FALSE) {

  if (details) {
    progress <- TRUE
  }

  l        <- model$model
  nam      <- colnames(attr(model$terms, "factors"))
  n        <- ncol(l)
  response <- names(model$model)[1]
  mlen_p   <- length(nam)
  preds    <- c()
  pvals    <- c()
  p_val    <- c()
  all_pred <- nam
  cterms   <- all_pred
  step     <- 0
  rsq      <- c()
  adjrsq   <- c()
  aic      <- c()
  sbic     <- c()
  cp       <- c()
  sbc      <- c()
  rmse     <- c()

  if (progress) {
    cat(format("Forward Hierarchical Selection Method", justify = "left", width = 27), "\n")
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
    predictors <- c(preds, all_pred[i])
    m          <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
    m_sum      <- Anova(m)
    pvals      <- c(pvals, m_sum$`Pr(>F)`[i])

    if (pvals[i] <= p_val) {
      preds  <- c(preds, cterms[i])
      p_val  <- c(p_val, pvals[i])
      step   <- step + 1
      fr     <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
      rsq    <- c(rsq, fr$rsq)
      adjrsq <- c(adjrsq, fr$adjr)
      aic    <- c(aic, ols_aic(fr$model))
      sbc    <- c(sbc, ols_sbc(fr$model))
      sbic   <- c(sbic, ols_sbic(fr$model, model))
      cp     <- c(cp, ols_mallows_cp(fr$model, model))
      rmse   <- c(rmse, fr$rmse)

      if (details) {
        cat("\n")
        cat(paste("Forward Selection: Step", step), "\n\n")
      }

      if (progress) {
        cat(paste("-", tail(preds, n = 1)), "\n")
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

  if (details) {
    cat("\n\n")
    len_pred <- length(preds)
    if (len_pred < 1) {
      cat("Variables Entered: None", "\n\n")
    } else if (len_pred == 1) {
      cat(paste("Variables Entered:", preds[1]), "\n\n")
    } else {
      cat("Variables Entered:", "\n\n")
      for (i in seq_len(length(preds))) {
        if (details) {
          cat("+", preds[i], "\n")
        } else {
          cat(paste("+", preds[i]), "\n")
        }
      }
    }
  }

  if (progress) {
    cat("\n\n")
    cat("Final Model Output", "\n")
    cat(rep("-", 18), sep = "", "\n\n")

    fi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
    print(fi)
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  metrics <- data.frame(step       = seq_len(step),
                        variable   = preds,
                        r2         = rsq,
                        adj_r2     = adjrsq,
                        aic        = aic,
                        sbic       = sbic,
                        sbc        = sbc,
                        mallows_cp = cp,
                        rmse       = rmse)

  result <- list(metrics = metrics,
                 model   = final_model,
                 others  = list(model = model)) 

  class(result) <- "ols_step_forward_p"

  return(result)

}

ols_step_hierarchical_backward <- function(model, p_val = 0.1, progress = FALSE, details = FALSE) {

  l        <- model$model
  nam      <- colnames(attr(model$terms, "factors"))
  n        <- ncol(l)
  response <- names(model$model)[1]
  mlen_p   <- length(nam)
  preds    <- c()
  cterms   <- nam
  step     <- 0
  rsq      <- c()
  adjrsq   <- c()
  aic      <- c()
  sbic     <- c()
  cp       <- c()
  sbc      <- c()
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

  for (i in rev(seq_len(mlen_p))) {
    predictors <- cterms[1:i]
    m          <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
    m_sum      <- Anova(m)
    pvals      <- m_sum$`Pr(>F)`[i]

    if (pvals >= p_val) {
      preds  <- c(preds, cterms[i])
      step   <- step + 1
      rpred  <- setdiff(cterms, preds)      
      fr     <- ols_regress(paste(response, "~", paste(rpred, collapse = " + ")), l)
      rsq    <- c(rsq, fr$rsq)
      adjrsq <- c(adjrsq, fr$adjr)
      aic    <- c(aic, ols_aic(fr$model))
      sbc    <- c(sbc, ols_sbc(fr$model))
      sbic   <- c(sbic, ols_sbic(fr$model, model))
      cp     <- c(cp, ols_mallows_cp(fr$model, model))
      rmse   <- c(rmse, fr$rmse)

      if (progress) {
        cat(paste("-", tail(preds, n = 1)), "\n")
      }

      if (details) {
        cat("\n")
        cat(paste("Backward Elimination: Step", step, "\n\n"), paste("Variable", tail(preds, n = 1), "Removed"), "\n\n")
        m <- ols_regress(paste(response, "~", paste(rpred, collapse = " + ")), l)
        print(m)
        cat("\n\n")
      }

    } else {

      if (progress) {
        cat("\n")
        cat(paste0("No more variables satisfy the condition of p value = ", p_val))
        cat("\n")
      }

      break
    }

  }

  pterms      <- setdiff(nam, preds)

  if (details) {
    cat("\n\n")
    len_pred <- length(preds)
    if (len_pred < 1) {
      cat("Variables Removed: None", "\n\n")
    } else if (len_pred == 1) {
      cat(paste("Variables Removed:", preds[1]), "\n\n")
    } else {
      cat("Variables Removed:", "\n\n")
      for (i in seq_len(len_pred)) {
        cat(paste("-", preds[i]), "\n")
      }
    }
  }

  if (progress) {
    cat("\n\n")
    cat("Final Model Output", "\n")
    cat(rep("-", 18), sep = "", "\n\n")

    fi <- ols_regress(paste(response, "~", paste(pterms, collapse = " + ")),data = l)
    print(fi)
  }

  final_model <- lm(paste(response, "~", paste(pterms, collapse = " + ")), data = l)

  metrics <- data.frame(step       = seq_len(step),
                        variable   = preds,
                        r2         = rsq,
                        adj_r2     = adjrsq,
                        aic        = aic,
                        sbic       = sbic,
                        sbc        = sbc,
                        mallows_cp = cp,
                        rmse       = rmse)

  result <- list(metrics = metrics,
                 model   = final_model,
                 others  = list(model = model))

  class(result) <- "ols_step_backward_p"

  return(result)

}
