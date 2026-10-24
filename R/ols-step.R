ols_step_forward <- function(model, metric = c("aic", "sbic", "sbc", "rsq", "adjrsq"), include = NULL, exclude = NULL, progress = FALSE, details = FALSE, steps = NULL, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

  criteria <- match.arg(metric)
  response <- names(model$model)[1]
  l        <- ols_model_data(model)
  indterms <- coeff_names(model)
  lenterms <- length(indterms)

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  include  <- unique(include)
  exclude  <- unique(exclude)
  lockterm <- c(include, exclude)
  nam      <- setdiff(indterms, lockterm)
  all_pred <- nam
  mlen_p   <- length(all_pred)
  preds    <- include

  indicator <- switch(criteria,
    aic    = "aic",
    sbc    = "sbc",
    sbic   = "sbic",
    rsq    = "r2",
    adjrsq = "adj_r2")

  if (ols_zero_rss(model)) {
    ols_zero_rss_stop()
    out <- list(metrics = data.frame(step = integer(0), variable = character(0),
                                     r2 = numeric(0), adj_r2 = numeric(0),
                                     aic = numeric(0), sbc = numeric(0), sbic = numeric(0)),
                model   = model,
                others  = list(base_model = model,
                               criteria   = indicator,
                               direction  = "forward",
                               full_model = model))
    return(out)
  }

  if (progress || details) {
    ols_candidate_terms(nam, "forward")
    if (interactive()) {
      Sys.sleep(0.5)  
    }
  }
    
  aics  <- c()
  bics  <- c()
  sbics <- c()
  rsq   <- c()
  arsq  <- c()

  step  <- 0
  laic  <- numeric(0)
  lbic  <- numeric(0)
  lsbic <- numeric(0)
  lrsq  <- numeric(0)
  larsq <- numeric(0)

  base_model  <- ols_base_model(include, response, l)
  base_metric <- ols_base_criteria(base_model, criteria)

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
    sbics[i] <- ols_sbic(k$model, model)
    rsq[i]   <- k$rsq
    arsq[i]  <- k$adjr
  }

  da <- data.frame(predictors = all_pred, aics = aics, bics = bics, sbics = sbics, rsq = rsq, arsq = arsq)
  da2 <- ols_sort_metrics(da, criteria)

  if (details) {
    ols_stepwise_metrics(da2, criteria, da2$predictors, aics, bics, sbics, rsq, arsq, "add")
    if (interactive()) {
      Sys.sleep(0.5)  
    }
  }

  mat  <- switch(criteria,
    aic    = aics,
    sbc    = bics,
    sbic   = sbics,
    rsq    = rsq,
    adjrsq = arsq)

  minc <- ols_threshold(mat, criteria)
  crit <- ols_f_criteria(criteria, mat, minc, base_metric)

  if (crit) {
    add_var  <- all_pred[minc]
    laic     <- aics[minc]
    lbic     <- bics[minc]
    lsbic    <- sbics[minc]
    lrsq     <- rsq[minc]
    larsq    <- arsq[minc]
    preds    <- unique(c(preds, add_var))
    lpreds   <- length(preds) - length(include)
    all_pred <- setdiff(all_pred, add_var)
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

  if (!is.null(steps)) {
    mlen_p <- steps
  }

  while (crit && step < mlen_p) {

    aics  <- c()
    bics  <- c()
    sbics <- c()
    rsq   <- c()
    arsq  <- c()
    mo    <- ols_regress(ols_step_formula(response, preds), data = l)
    met   <- ols_base_criteria(mo$model, criteria)

    if (details) {
      ols_stepwise_details(step, preds, preds, response, met, "added", criteria)
      if (interactive()) {
        Sys.sleep(0.5)  
      }
    }

    for (i in seq_len(len_p)) {

      predictors <- unique(c(preds, all_pred[i]))
      k          <- ols_regress(ols_step_formula(response, predictors), data = l)
      aics[i]    <- ols_aic(k$model)
      bics[i]    <- ols_sbc(k$model)
      sbics[i]   <- ols_sbic(k$model, model)
      rsq[i]     <- k$rsq
      arsq[i]    <- k$adjr
    l}

    if (details) {
      da  <- data.frame(predictors = all_pred, aics = aics, bics = bics, sbics = sbics, rsq = rsq, arsq = arsq)
      da2 <- ols_sort_metrics(da, criteria)
      ols_stepwise_metrics(da2, criteria, da2$predictors, aics, bics, sbics, rsq, arsq, "add")
      if (interactive()) {
        Sys.sleep(0.5)  
      }
    }

    mat  <- switch(criteria,
      aic    = aics,
      sbc    = bics,
      sbic   = sbics,
      rsq    = rsq,
      adjrsq = arsq)

    faic  <- switch(criteria,
      aic    = laic,
      sbc    = lbic,
      sbic   = lsbic,
      rsq    = lrsq,
      adjrsq = larsq)

    minaic <- ols_threshold(mat, criteria)
    crit   <- ols_next_criteria(criteria, mat, minaic, faic, lpreds)

    if (crit) {

      add_var  <- all_pred[minaic]
      preds    <- unique(c(preds, add_var))
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
      all_pred <- setdiff(all_pred, add_var)
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

  final_model <- lm(ols_step_formula(response, preds), data = l)

  var_selected <- setdiff(preds, include)
  if (length(var_selected) == 0) {
    var_selected <- character(0)
  }

  metrics     <- data.frame(step     = seq_len(step),
                            variable = var_selected,
                            r2       = lrsq,
                            adj_r2   = larsq,
                            aic      = laic,
                            sbc      = lbic,
                            sbic     = lsbic)

  out <- list(metrics = metrics,
              model   = final_model,
              others  = list(base_model = base_model,
                             criteria   = indicator,
                             direction  = "forward",
                             full_model = model))

  return(out)
}

ols_step_backward <- function(model, metric = c("aic", "sbic", "sbc", "rsq", "adjrsq"), include = NULL, exclude = NULL, progress = FALSE, details = FALSE, steps = NULL, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

  criteria <- match.arg(metric)
  response <- names(model$model)[1]
  l        <- ols_model_data(model)
  indterms <- coeff_names(model)
  lenterms <- length(indterms)

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  include <- unique(include)
  exclude <- unique(exclude)

  nam   <- setdiff(indterms, exclude)
  cterm <- setdiff(nam, include)
  preds <- nam

  indicator <- switch(criteria,
    aic    = "aic",
    sbc    = "sbc",
    sbic   = "sbic",
    rsq    = "r2",
    adjrsq = "adj_r2")

  if (ols_zero_rss(model)) {
    ols_zero_rss_stop()
    out <- list(metrics = data.frame(step = integer(0), variable = character(0),
                                     r2 = numeric(0), adj_r2 = numeric(0),
                                     aic = numeric(0), sbc = numeric(0), sbic = numeric(0)),
                model   = model,
                others  = list(full_model = model,
                               criteria   = indicator,
                               direction  = "backward"))
    return(out)
  }

  if (progress || details) {
    ols_candidate_terms(nam, "backward")
  }
    
  mi    <- ols_regress(ols_step_formula(response, preds), data = l)
  laic  <- ols_aic(model)
  lbic  <- ols_sbc(model)
  lsbic <- ols_sbic(model, model)
  lrsq  <- mi$rsq
  larsq <- mi$adjr

  base_metric <- ols_base_criteria(model, criteria)

  if (details) {
    ols_base_model_stats(response, preds, criteria, base_metric)
  }

  ilp   <- length(preds)
  end   <- FALSE
  step  <- 0
  rpred <- c()
  aics  <- c()
  bics  <- c()
  sbics <- c()
  rsq   <- c()
  arsq  <- c()

  for (i in seq_len(ilp)) {

    predictors <- preds[-i]

    m <- ols_regress(ols_step_formula(response, predictors), data = l)

    aics[i]  <- ols_aic(m$model)
    bics[i]  <- ols_sbc(m$model)
    sbics[i] <- ols_sbic(m$model, model)
    rsq[i]   <- m$rsq
    arsq[i]  <- m$adjr
  }

  da  <- data.frame(predictors = preds, aics = aics, bics = bics, sbics = sbics, rsq = rsq, arsq = arsq)
  da2 <- ols_sort_metrics(da, criteria)

  mat  <- switch(criteria,
               aic    = aics,
               sbc    = bics,
               sbic   = sbics,
               rsq    = rsq,
               adjrsq = arsq)

  if (grepl("rsq", criteria)) {
    da3 <- cbind(loc = order(-mat), da2)  
  } else {
    da3 <- cbind(loc = order(mat), da2)
  }
 

  if (details) {
    ols_stepwise_metrics(da2, criteria, da2$predictors, aics, bics, sbics, rsq, arsq, "remove")
  }

  linc <- length(include)

  if (progress) {
    ols_progress_init("backward")
  }

  while (!end) {

    if (length(preds) < 1) {
      end <- TRUE
      if (progress || details) {
        ols_stepwise_break("backward")
      }
      break
    }

    for (i in seq_len(linc)) {
      lnam <- which(da3$predictors == include[i])
      da3 <- da3[-lnam, ]
    }

    minc <- da3$loc[1]
    crit <- ols_f_criteria(criteria, mat, minc, base_metric)

    if (crit) {

      drop_var <- da3$predictors[1]
      rpred    <- c(rpred, drop_var)
      preds    <- setdiff(preds, drop_var)
      ilp      <- length(preds)
      step     <- step + 1
      base_metric <- mat[minc]

      mi <- ols_regress(ols_step_formula(response, preds), data = l)

      laic   <- c(laic, mi$aic)
      lbic   <- c(lbic, mi$sbc)
      lsbic  <- c(lsbic, mi$sbic)
      lrsq   <- c(lrsq, mi$rsq)
      larsq  <- c(larsq, mi$adjr)
      aics   <- c()
      bics   <- c()
      sbics  <- c()
      rsq    <- c()
      arsq   <- c()

      if (progress) {
        ols_progress_display(rpred, "others")
      }

      if (ilp < 1) {
        end <- TRUE
        if (progress || details) {
          ols_stepwise_break("backward")
        }
        break
      }

      for (i in seq_len(ilp)) {

        predictors <- preds[-i]

        m <- ols_regress(ols_step_formula(response, predictors), data = l)

        aics[i]  <- ols_aic(m$model)
        bics[i]  <- ols_sbc(m$model)
        sbics[i] <- ols_sbic(m$model, model)
        rsq[i]   <- m$rsq
        arsq[i]  <- m$adjr
      }

      da  <- data.frame(predictors = preds, aics = aics, bics = bics, sbics = sbics, rsq = rsq, arsq = arsq)
      da2 <- ols_sort_metrics(da, criteria)

      mat  <- switch(criteria,
               aic    = aics,
               sbc    = bics,
               sbic   = sbics,
               rsq    = rsq,
               adjrsq = arsq)

      if (grepl("rsq", criteria)) {
        da3 <- cbind(loc = order(-mat), da2)  
      } else {
        da3 <- cbind(loc = order(mat), da2)
      }

      if (details) {
        ols_stepwise_details(step, rpred, preds, response, base_metric, "removed", criteria)
        ols_stepwise_metrics(da2, criteria, da2$predictors, aics, bics, sbics, rsq, arsq, "remove")
      }

      if (!is.null(steps)) {
        if (step == steps) {
          end <- TRUE
          if (progress || details) {
            ols_stepwise_break("backward")
          }
        }
      }
    } else {
      end <- TRUE
      if (progress || details) {
        ols_stepwise_break("backward")
      }
    }

  }


  if (details) {
    ols_stepwise_vars(rpred, "backward")
  }

  final_model <- lm(ols_step_formula(response, preds), data = l)

  metrics     <- data.frame(step     = seq_len(step),
                            variable = rpred,
                            r2       = tail(lrsq,  n = step),
                            adj_r2   = tail(larsq, n = step),
                            aic      = tail(laic,  n = step),
                            sbc      = tail(lbic,  n = step),
                            sbic     = tail(lsbic,  n = step))

  out <- list(metrics = metrics,
              model   = final_model,
              others  = list(full_model = model,
                             criteria   = indicator,
                             direction  = "backward"))

  return(out)
}

ols_step_both <- function(model, metric = c("aic", "sbic", "sbc", "rsq", "adjrsq"), include = NULL, exclude = NULL, progress = FALSE, details = FALSE, steps = NULL, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

  criteria <- match.arg(metric)
  response <- names(model$model)[1]
  l        <- ols_model_data(model)
  nam      <- coeff_names(model)
  indterms <- nam
  lenterms <- length(indterms)

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  include  <- unique(include)
  exclude  <- unique(exclude)
  len_inc  <- length(include) + 1

  indicator <- switch(criteria,
    aic    = "aic",
    sbc    = "sbc",
    sbic   = "sbic",
    rsq    = "r2",
    adjrsq = "adj_r2")

  if (ols_zero_rss(model)) {
    ols_zero_rss_stop()
    out <- list(metrics = data.frame(step = integer(0), variable = character(0),
                                     method = character(0), r2 = numeric(0),
                                     adj_r2 = numeric(0), aic = numeric(0),
                                     sbc = numeric(0), sbic = numeric(0)),
                model   = model,
                others  = list(base_model = model,
                               criteria   = indicator,
                               direction  = "both",
                               full_model = model))
    return(out)
  }

  if (progress || details) {
    ols_candidate_terms(nam, "both")
  }

  lockterm   <- c(include, exclude)
  predictors <- setdiff(nam, lockterm)
  mlen_p     <- length(predictors)
  tech       <- c("addition", "removal")

  base_model <- ols_base_model(include, response, l)
  bmetric <- ols_base_criteria(base_model, criteria)

  if (details) {
    ols_base_model_stats(response, include, criteria, bmetric)
  }

  step      <- 0
  all_step  <- 0
  preds     <- include
  var_index <- character(0)
  method    <- character(0)
  laic      <- numeric(0)
  lbic      <- numeric(0)
  lsbic     <- numeric(0)
  lrsq      <- numeric(0)
  larsq     <- numeric(0)

  if (progress) {
    ols_progress_init("both")
  }
  
  if (!is.null(steps)) {
    mlen_p <- steps
  }

  while (step < mlen_p) {

    aics  <- c()
    bics  <- c()
    sbics <- c()
    rsq   <- c()
    arsq  <- c()
    lpds  <- length(predictors)

    for (i in seq_len(lpds)) {

      predn <- unique(c(preds, predictors[i]))

      m <- ols_regress(ols_step_formula(response, predn), data = l)

      aics[i]  <- ols_aic(m$model)
      bics[i]  <- ols_sbc(m$model)
      sbics[i] <- ols_sbic(m$model, model)
      rsq[i]   <- m$rsq
      arsq[i]  <- m$adjr
    }

    da <- data.frame(predictors = predictors, aics = aics, bics = bics, sbics = sbics, rsq = rsq, arsq = arsq)

    if (details) {
      ols_stepwise_metrics(da, criteria, predictors, aics, bics, sbics, rsq, arsq, "add")
    }

    mat  <- switch(criteria,
    aic    = aics,
    sbc    = bics,
    sbic   = sbics,
    rsq    = rsq,
    adjrsq = arsq)

    minc <- ols_threshold(mat, criteria)
    crit <- ols_f_criteria(criteria, mat, minc, bmetric)

    if (crit) {
      add_var    <- predictors[minc]
      bmetric    <- mat[minc]
      preds      <- unique(c(preds, add_var))
      predictors <- setdiff(predictors, add_var)
      lpds       <- length(predictors)
      method     <- c(method, tech[1])
      lpreds     <- length(preds)
      var_index  <- c(var_index, add_var)
      step       <- step + 1
      all_step   <- all_step + 1
      maic       <- aics[minc]
      mbic       <- bics[minc]
      msbic      <- sbics[minc]
      mrsq       <- rsq[minc]
      marsq      <- arsq[minc]
      laic       <- c(laic, maic)
      lbic       <- c(lbic, mbic)
      lsbic      <- c(lsbic, msbic)
      lrsq       <- c(lrsq, mrsq)
      larsq      <- c(larsq, marsq)

      if (progress) {
        ols_progress_display(preds, "both", "added")
      }

      kaic  <- switch(criteria,
          aic    = maic,
          sbc    = mbic,
          sbic   = msbic,
          rsq    = mrsq,
          adjrsq = marsq)

      if (details) {
        ols_stepwise_details(all_step, preds, preds, response, kaic, "added", criteria)
      }

      if (lpreds > 1) {

        aics  <- c()
        bics  <- c()
        sbics <- c()
        rsq   <- c()
        arsq  <- c()
        j     <- 1
        remove_candidates <- preds[len_inc:lpreds]

        for (i in seq_along(remove_candidates)) {

          preda <- setdiff(preds, remove_candidates[i])

          m <- ols_regress(ols_step_formula(response, preda), data = l)

          aics[j]  <- ols_aic(m$model)
          bics[j]  <- ols_sbc(m$model)
          sbics[j] <- ols_sbic(m$model, model)
          rsq[j]   <- m$rsq
          arsq[j]  <- m$adjr

          j <- j + 1
        }

        da <- data.frame(predictors = remove_candidates, aics = aics, bics = bics, sbics = sbics, rsq = rsq, arsq = arsq)

        if (details) {
          ols_stepwise_metrics(da, criteria, da$predictors, aics, bics, sbics, rsq, arsq, "remove")
        }

        mat  <- switch(criteria,
          aic    = aics,
          sbc    = bics,
          sbic   = sbics,
          rsq    = rsq,
          adjrsq = arsq)

        faic  <- switch(criteria,
          aic    = laic,
          sbc    = lbic,
          sbic   = lsbic,
          rsq    = lrsq,
          adjrsq = larsq)

        minc2 <- ols_threshold(mat, criteria)
        crit  <- ols_next_criteria(criteria, mat, minc2, faic, all_step)

        if (crit) {
          bmetric   <- mat[minc2]
          maic      <- aics[minc2]
          mbic      <- bics[minc2]
          msbic     <- sbics[minc2]
          mrsq      <- rsq[minc2]
          marsq     <- arsq[minc2]
          laic      <- c(laic, maic)
          lbic      <- c(lbic, mbic)
          lsbic     <- c(lsbic, msbic)
          lrsq      <- c(lrsq, mrsq)
          larsq     <- c(larsq, marsq)
          temp      <- remove_candidates[minc2]
          var_index <- c(var_index, temp)
          method    <- c(method, tech[2])
          all_step  <- all_step + 1

          if (progress) {
            ols_progress_display(temp, "both", "removed")
          }

          preds  <- setdiff(preds, temp)
          lpreds <- length(preds)

          kaic  <- switch(criteria,
            aic    = maic,
            sbc    = mbic,
            sbic   = msbic,
            rsq    = mrsq,
            adjrsq = marsq)

          if (details) {
            ols_stepwise_details(all_step, temp, preds, response, kaic, "removed", criteria)
          }
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      if (progress || details) {
        ols_stepwise_break("both")
      }
      break
    }
  }

  if (details) {
    ols_stepwise_vars(preds, "both")
  }

  final_model <- lm(ols_step_formula(response, preds), data = l)

  metrics     <- data.frame(step     = seq_len(all_step),
                            variable = var_index,
                            method   = method,
                            r2       = lrsq,
                            adj_r2   = larsq,
                            aic      = laic,
                            sbc      = lbic,
                            sbic     = lsbic)

  out <- list(metrics = metrics,
              model   = final_model,
              others   = list(base_model = base_model,
                              criteria   = indicator,
                              direction  = "both",
                              full_model = model))

  return(out)
}