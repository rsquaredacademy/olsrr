ols_step_forward <- function(model, metric = c("aic", "sbic", "bic", "rsq", "adjrsq"), include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

  criteria <- match.arg(metric)
  response <- names(model$model)[1]
  l        <- model$model
  indterms <- coeff_names(model)
  lenterms <- length(indterms)

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  lockterm <- c(include, exclude)
  nam      <- setdiff(indterms, lockterm)
  all_pred <- nam
  mlen_p   <- length(all_pred)
  preds    <- include

  if (progress || details) {
    ols_candidate_terms(nam, "forward")
    if (interactive()) {
      Sys.sleep(0.5)  
    }
  }
    
  aics  <- c()
  bics  <- c()
  sbics <- c()
  ess   <- c()
  rss   <- c()
  rsq   <- c()
  arsq  <- c()

  base_model  <- ols_base_model(include, response, l)
  base_metric <- ols_base_criteria(base_model, criteria)
  # aic1 <- ols_aic(base_model)

  if (details) {
    ols_base_model_stats(response, include, criteria, base_metric)
    if (interactive()) {
      Sys.sleep(0.5)  
    }
  }

  for (i in seq_len(mlen_p)) {

    predictors <- c(include, all_pred[i])
    k <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)

    aics[i]  <- ols_aic(k$model)
    bics[i]  <- ols_sbc(k$model)
    sbics[i] <- ols_sbic(k$model, k$model)
    ess[i]   <- k$ess
    rss[i]   <- k$rss
    rsq[i]   <- k$rsq
    arsq[i]  <- k$adjr
  }

  da <- data.frame(predictors = all_pred, aics = aics, bics = bics, sbics = sbics, rsq = rsq, arsq = arsq)
  # da2 <- da[order(da$aics), ]
  da2 <- ols_sort_metrics(da, criteria)

  if (details) {
    ols_stepwise_metrics(da2, criteria, da2$predictors, aics, bics, sbics, rsq, arsq)
    if (interactive()) {
      Sys.sleep(0.5)  
    }
  }

  mat  <- switch(criteria,
    aic = aics,
    bic = bics,
    sbic = sbics,
    rsq = rsq,
    adjrsq = arsq)

  # minc <- which(aics == min(aics))
  minc <- ols_threshold(mat, criteria)
  crit <- ols_f_criteria(criteria, mat, minc, base_metric)

  if (crit) {
    laic     <- aics[minc]
    lbic     <- bics[minc]
    lsbic    <- sbics[minc]
    lrsq     <- rsq[minc]
    larsq    <- arsq[minc]
    preds    <- c(preds, all_pred[minc])
    lpreds   <- length(preds) - length(include)
    all_pred <- all_pred[-minc]
    len_p    <- length(all_pred)
    step     <- 1

    if (progress) {
      ols_progress_init("forward")
      if (interactive()) {
        Sys.sleep(0.5)  
      }
      ols_progress_display(preds, "others")
      if (interactive()) {
        Sys.sleep(0.5)  
      }
    }
  } else {
    ols_stepwise_break("forward")
    if (interactive()) {
      Sys.sleep(0.5)  
    }
  }

  while (step < mlen_p) {

    aics  <- c()
    bics  <- c()
    sbics <- c()
    ess   <- c()
    rss   <- c()
    rsq   <- c()
    arsq  <- c()
    mo    <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
    met   <- ols_base_criteria(mo$model, criteria)
    # aic1 <- ols_aic(mo$model)

    if (details) {
      ols_stepwise_details(step, preds, preds, response, met, "added", criteria)
      if (interactive()) {
        Sys.sleep(0.5)  
      }
    }

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      k          <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)
      aics[i]    <- ols_aic(k$model)
      bics[i]    <- ols_sbc(k$model)
      sbics[i]   <- ols_sbic(k$model, k$model)
      ess[i]     <- k$ess
      rss[i]     <- k$rss
      rsq[i]     <- k$rsq
      arsq[i]    <- k$adjr
    l}

    if (details) {
      da  <- data.frame(predictors = all_pred, aics = aics, bics = bics, sbics = sbics, rsq = rsq, arsq = arsq)
      # da2 <- da[order(da$aics), ]
      da2 <- ols_sort_metrics(da, criteria)
      ols_stepwise_metrics(da2, criteria, da2$predictors, aics, bics, sbics, rsq, arsq)
      if (interactive()) {
        Sys.sleep(0.5)  
      }
    }

    # minaic <- which(aics == min(aics))
    mat  <- switch(criteria,
      aic = aics,
      bic = bics,
      sbic = sbics,
      rsq = rsq,
      adjrsq = arsq)

    faic  <- switch(criteria,
      aic = laic,
      bic = lbic,
      sbic = lsbic,
      rsq = lrsq,
      adjrsq = larsq)
    minaic <- ols_threshold(mat, criteria)
    crit   <- ols_next_criteria(criteria, mat, minaic, faic, lpreds)

    if (crit) {

      preds    <- c(preds, all_pred[minaic])
      minc     <- aics[minaic]
      mbic     <- bics[minaic]
      msbic    <- sbics[minaic]
      mrsq     <- rsq[minaic]
      marsq    <- arsq[minaic]
      laic     <- c(laic, minc)
      lbic     <- c(lbic, mbic)
      lsbic    <- c(lsbic, msbic)
      lrsq     <- c(lrsq, mrsq)
      larsq    <- c(larsq, marsq)
      lpreds   <- length(preds) - length(include)
      all_pred <- all_pred[-minaic]
      len_p    <- length(all_pred)
      step     <- step + 1

      if (progress) {
        ols_progress_display(preds, "others")
        if (interactive()) {
          Sys.sleep(0.5)  
        }
      }
    } else {
      if (progress || details) {
        ols_stepwise_break("forward")
        if (interactive()) {
          Sys.sleep(0.5)  
        }
      }
      break
    }
  }

  if (details) {
    ols_stepwise_vars(preds, "forward")
    if (interactive()) {
      Sys.sleep(0.5)  
    }
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  if (is.null(include)) {
    var_selected <- preds
  } else {
    var_selected <- preds[-seq_len(length(include))]
  }

  metrics     <- data.frame(step     = seq_len(step),
                            variable = var_selected,
                            r2       = lrsq,
                            adj_r2   = larsq,
                            aic      = laic,
                            bic      = lbic,
                            sbic     = lsbic)

  out <- list(metrics = metrics,
              model   = final_model,
              others  = list(base_model = base_model))


  class(out) <- "ols_step_forward_aic"

  return(out)
}