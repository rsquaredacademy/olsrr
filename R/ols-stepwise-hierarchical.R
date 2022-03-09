#' Stepwise hierarchical selection
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on p values, in a stepwise hierarchical manner
#' until there is no variable left to enter any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param p_value p value; variables with p value less/higher than \code{p_val} will
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
ols_step_hierarchical <- function(model, p_value = 0.1, forward = TRUE, progress = FALSE, details = FALSE) {

  if (forward) {
    ols_step_hierarchical_forward(model, p_value, progress, details)
  } else {
    ols_step_hierarchical_backward(model, p_value, progress, details)
  }

}

ols_step_hierarchical_forward <- function(model, p_value = 0.1, progress = FALSE, details = FALSE) {

  if (details) {
    progress <- FALSE
  }

  l        <- model$model
  nam      <- colnames(attr(model$terms, "factors"))
  n        <- ncol(l)
  response <- names(model$model)[1]
  mlen_p   <- length(nam)
  preds    <- c()
  pvals    <- c()
  aicvals  <- c()
  rsqvals  <- c()
  arsqvals <- c()
  p_val    <- c()
  all_pred <- nam
  cterms   <- all_pred

  if (progress || details) {
    ols_candidate_terms(nam, "forward")
  }
    
  step     <- 0
  rsq      <- c()
  adjrsq   <- c()
  aic      <- c()
  sbic     <- c()
  cp       <- c()
  sbc      <- c()
  rmse     <- c()

  base_model <- lm(paste(response, "~", 1), data = l)
  rsq_base   <- summary(base_model)$r.squared
    
  if (details) {
    ols_rsquared_init(NULL, "r2", response, rsq_base)
  }

  if (progress) {
    ols_progress_init("forward")
  }

  for (i in seq_len(mlen_p)) {
    predictors  <- c(preds, all_pred[i])
    m           <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
    m_sum       <- Anova(m)
    pvals       <- c(pvals, m_sum$`Pr(>F)`[i])
    aicvals[i]  <- ols_aic(m)
    rsqvals[i]  <- summary(m)$r.squared
    arsqvals[i] <- summary(m)$adj.r.squared

    if (details) {
      d <- data.frame(predictor = predictors, p_val = pvals, rsq = rsqvals,
                       arsq = arsqvals, aic = aicvals)
      d <- d[order(d$p_val), ]
      ols_stepwise_table(d, predictors, pvals, rsqvals, arsqvals, aicvals)
    }

    if (pvals[i] <= p_value) {
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
        rsq1 <- tail(rsq, n = 1)
        ols_rsquared_selected("r2", step, preds, response, rsq1)
      }

      if (progress) {
        ols_progress_display(preds, "others")
      }

    } else {
      if (progress) {
        ols_stepwise_break("forward")
      }
      break
    }

  }

  if (details) {
    ols_stepwise_vars(preds, "forward")
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  metrics <- data.frame(step       = seq_len(step),
                        variable   = preds,
                        r2         = rsq,
                        adj_r2     = adjrsq,
                        aic        = aic,
                        sbc        = sbc,
                        sbic       = sbic,
                        mallows_cp = cp,
                        rmse       = rmse)

  result <- list(metrics = metrics,
                 model   = final_model,
                 others  = list(base_model = base_model,
                                full_model = model))

  class(result) <- "ols_step_forward_p"

  return(result)

}

ols_step_hierarchical_backward <- function(model, p_value = 0.1, progress = FALSE, details = FALSE) {

  if (details) {
    progress <- FALSE
  }

  l        <- model$model
  nam      <- colnames(attr(model$terms, "factors"))
  n        <- ncol(l)
  response <- names(model$model)[1]
  mlen_p   <- length(nam)
  preds    <- c()
  cterms   <- nam

  if (progress || details) {
    ols_candidate_terms(nam, "backward")  
  }
    
  step     <- 0
  rsq      <- c()
  adjrsq   <- c()
  aic      <- c()
  sbic     <- c()
  cp       <- c()
  sbc      <- c()
  rmse     <- c()

  rsq_base   <- summary(model)$r.squared
    
  if (details) {
    ols_rsquared_init(NULL, "r2", response, rsq_base)
  }

  if (progress) {
    ols_progress_init("backward")
  }

  for (i in rev(seq_len(mlen_p))) {
    predictors  <- cterms[1:i]
    m           <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
    m_sum       <- Anova(m)
    pvals       <- m_sum$`Pr(>F)`[1:i]
    p_vals      <- pvals[i]
  
    if (details) {
      d <- data.frame(predictors = predictors, p_val = pvals)
      ols_stepwise_table_p(d, predictors, pvals)
    }

    if (p_vals >= p_value) {
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
        ols_progress_display(preds, "others")
      }

      if (details) {
        rsq1   <- tail(rsq, n = 1)
        ols_stepwise_details(step, preds, rpred, response, rsq1, "removed", "rsq")
      }

    } else {

      if (progress || details) {
        ols_stepwise_break(direction = "backward")
      }

      break
    }

  }

  pterms <- setdiff(nam, preds)

  if (details) {
    ols_stepwise_vars(preds, "backward")
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
                 others  = list(full_model = model))

  class(result) <- "ols_step_backward_p"

  return(result)

}
