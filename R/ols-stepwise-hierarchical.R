# model    <- lm(y ~ ., data = surgical)
# model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + alc_mod + age + alc_mod, data = surgical)

ols_stepwise_hierarchical <- function(model, pval = 0.1, forward = TRUE, progress = FALSE, details = FALSE) {

  if (forward) {
    ols_stepwise_hierarchical_forward(model, pval, progress, details)
  } else {
    ols_stepwise_hierarchical_backward(model, pval, progress, details)
  }

}

ols_stepwise_hierarchical_forward <- function(model, pval = 0.1, progress = FALSE, details = FALSE) {

  l        <- model$model
  nam      <- colnames(attr(model$terms, "factors"))
  n        <- ncol(l)
  response <- names(model$model)[1]
  mlen_p   <- length(nam)
  preds    <- c()
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

  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    m          <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
    m_sum      <- Anova(m)
    pvals      <- m_sum$`Pr(>F)`[1]

    if (pvals <= pval) {
      preds <- c(preds, cterms[i])
      fr     <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
      rsq    <- c(rsq, fr$rsq)
      adjrsq <- c(adjrsq, fr$adjr)
      aic    <- c(aic, ols_aic(fr$model))
      sbc    <- c(sbc, ols_sbc(fr$model))
      sbic   <- c(sbic, ols_sbic(fr$model, model))
      cp     <- c(cp, ols_mallows_cp(fr$model, model))
      rmse   <- c(rmse, fr$rmse)
      step   <- step + 1
    } else {
      break
    }

  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  metrics <- data.frame(step = seq_len(step),
                        variable = preds,
                        r2 = rsq,
                        adj_r2 = adjrsq,
                        aic = aic,
                        sbic = sbic,
                        sbc = sbc,
                        mallows_cp = cp,
                        rmse = rmse)

  result <- list(metrics = metrics,
                 model   = final_model)

  return(result)

}

# model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)

ols_stepwise_hierarchical_backward <- function(model, pval = 0.1, progress = FALSE, details = FALSE) {

  l        <- model$model
  nam      <- colnames(attr(model$terms, "factors"))
  n        <- ncol(l)
  response <- names(model$model)[1]
  mlen_p   <- length(nam)
  preds    <- c()
  all_pred <- rev(nam)
  cterms   <- all_pred
  step     <- 0
  rsq      <- c()
  adjrsq   <- c()
  aic      <- c()
  sbic     <- c()
  cp       <- c()
  sbc      <- c()
  rmse     <- c()

  for (i in seq_len(mlen_p)) {
    predictors <- all_pred[i]
    m          <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
    m_sum      <- Anova(m)
    pvals      <- m_sum$`Pr(>F)`[1]

    if (pvals >= pval) {
      preds  <- c(preds, cterms[i])
      rpred  <- setdiff(nam, preds)
      fr     <- ols_regress(paste(response, "~", paste(rpred, collapse = " + ")), l)
      rsq    <- c(rsq, fr$rsq)
      adjrsq <- c(adjrsq, fr$adjr)
      aic    <- c(aic, ols_aic(fr$model))
      sbc    <- c(sbc, ols_sbc(fr$model))
      sbic   <- c(sbic, ols_sbic(fr$model, model))
      cp     <- c(cp, ols_mallows_cp(fr$model, model))
      rmse   <- c(rmse, fr$rmse)
      step   <- step + 1
    } else {
      break
    }

  }

  pterms      <- setdiff(nam, preds)
  final_model <- lm(paste(response, "~", paste(pterms, collapse = " + ")), data = l)

  metrics <- data.frame(step = seq_len(step),
                        variable = preds,
                        r2 = rsq,
                        adj_r2 = adjrsq,
                        aic = aic,
                        sbic = sbic,
                        sbc = sbc,
                        mallows_cp = cp,
                        rmse = rmse)

  result <- list(metrics = metrics,
                 model   = final_model)

  return(result)

}
