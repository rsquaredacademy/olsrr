ols_step_rsquared_forward <- function(model, metric, include = NULL, exclude = NULL, progress = FALSE, details = FALSE) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

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
  }

  aics     <- c()
  ess      <- c()
  rss      <- c()
  rsq      <- c()
  arsq     <- c()

  base_model <- ols_base_model(include, response, l)

  if (metric == "r2") {
    rsq_base <- summary(base_model)$r.squared
  } else {
    rsq_base <- summary(base_model)$adj.r.squared
  }

  if (details) {
    ols_rsquared_init(include, metric, response, rsq_base)
  }

  if (progress) {
    ols_progress_init("forward")
  }

  for (i in seq_len(mlen_p)) {

    predictors <- c(include, all_pred[i])
    k <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)

    aics[i] <- ols_aic(k$model)
    ess[i]  <- k$ess
    rss[i]  <- k$rss
    rsq[i]  <- k$rsq
    arsq[i] <- k$adjr
  }

  da <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
  da2 <- da[order(-da$rsq), ]

  if (details) {
    ols_stepwise_metrics(da2, metric, predictors, aics, rss, ess, rsq, arsq)
  }

  if (metric == "r2") {
    minc <- which(rsq == max(rsq))
  } else {
    minc <- which(arsq == max(arsq))
  }

  if (aics[minc] > rsq_base) {
    laic     <- aics[minc]
    less     <- ess[minc]
    lrss     <- rss[minc]
    lrsq     <- rsq[minc]
    larsq    <- arsq[minc]
    preds    <- c(preds, all_pred[minc])
    lpreds   <- length(preds) - length(include)
    all_pred <- all_pred[-minc]
    len_p    <- length(all_pred)
    step     <- 1
  } else {
    ols_stepwise_break(direction = "forward")
  }

  if (progress) {
    ols_progress_display(preds, "others")
  }

  while (step < mlen_p) {

    aics <- c()
    ess  <- c()
    rss  <- c()
    rsq  <- c()
    arsq <- c()
    mo   <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
    rsq1 <- mo$rsq

    if (details) {
      ols_rsquared_selected(metric, step, preds, response, rsq1)
    }

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      k          <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)
      aics[i]    <- ols_aic(k$model)
      ess[i]     <- k$ess
      rss[i]     <- k$rss
      rsq[i]     <- k$rsq
      arsq[i]    <- k$adjr
    }

    if (details) {
      da  <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
      da2 <- da[order(-da$rsq), ]
      ols_stepwise_metrics(da2, metric, predictors, aics, rss, ess, rsq, arsq)
    }

    if (metric == "r2") {
      minaic   <- which(rsq == max(rsq))
      add_crit <- rsq[minaic] > lrsq[lpreds]
    } else {
      minaic   <- which(arsq == max(arsq))
      add_crit <- arsq[minaic] > larsq[lpreds]
    }

    if (add_crit) {

      preds    <- c(preds, all_pred[minaic])
      minc     <- aics[minaic]
      mess     <- ess[minaic]
      mrss     <- round(rss[minaic], 3)
      mrsq     <- rsq[minaic]
      marsq    <- arsq[minaic]
      laic     <- c(laic, minc)
      less     <- c(less, mess)
      lrss     <- c(lrss, mrss)
      lrsq     <- c(lrsq, mrsq)
      larsq    <- c(larsq, marsq)
      lpreds   <- length(preds) - length(include)
      all_pred <- all_pred[-minaic]
      len_p    <- length(all_pred)
      step     <- step + 1

      if (progress) {
        ols_progress_display(preds, "others")
      }
    } else {
      if (progress || details) {
        ols_stepwise_break(direction = "forward")
      }
      break
    }
  }

  if (details) {
    ols_stepwise_vars(preds, "forward")
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
                            rss      = lrss,
                            ess      = less)

  out <- list(metrics = metrics,
              model   = final_model,
              others  = list(base_model = base_model,
                             direction  = "forward",
                             metric     = metric))


  class(out) <- "ols_step_rsquared"

  return(out)

}


ols_step_rsquared_backward <- function(model, metric, include = NULL, exclude = NULL, progress = FALSE, details = FALSE) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

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

  nam   <- setdiff(indterms, exclude)
  cterm <- setdiff(nam, include)
  preds <- nam

  if (progress || details) {
    ols_candidate_terms(cterm, "backward")
  }

  if (metric == "r2") {
    aic_f <- summary(model)$r.squared
  } else {
    aic_f <- summary(model)$adj.r.squared
  }

  mi    <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
  laic  <- aic_f
  lrss  <- mi$rss
  less  <- mi$ess
  lrsq  <- mi$rsq
  larsq <- mi$adjr

  if (details) {
    ols_rsquared_init(include, metric, response, aic_f)
    # if (metric == "r2") {
    #   cat("Step      => 0", "\n")
    #   cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
    #   cat("R-Squared =>", aic_f, "\n\n")
    # } else {
    #   cat("Step           => 0", "\n")
    #   cat("Model          =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
    #   cat("Adj. R-Squared =>", aic_f, "\n\n")
    # }
    # cat("Initiating stepwise selection...", "\n\n")
  }

  ilp   <- length(preds)
  end   <- FALSE
  step  <- 0
  rpred <- c()
  aics  <- c()
  ess   <- c()
  rss   <- c()
  rsq   <- c()
  arsq  <- c()

  for (i in seq_len(ilp)) {

    predictors <- preds[-i]

    m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)

    aics[i] <- m$aic
    ess[i]  <- m$ess
    rss[i]  <- m$rss
    rsq[i]  <- m$rsq
    arsq[i] <- m$adjr
  }

  da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
  da2 <- da[order(-da$arsq), ]
  da3 <- cbind(loc = order(-arsq), da2)

  if (details) {
    ols_stepwise_metrics(da2, metric, predictors, aics, rss, ess, rsq, arsq)
  }

  linc <- length(include)

  if (progress) {
    ols_progress_init("backward")
  }

  while (!end) {

    for (i in seq_len(linc)) {
      lnam <- which(da3$predictors == include[i])
      da3 <- da3[-lnam, ]
    }

    minc <- da3$loc[1]

    if (metric == "r2") {
      rem_crit <- da3$rsq[1] > aic_f
    } else {
      rem_crit <- da3$arsq[1] > aic_f
    }

    if (rem_crit) {

      rpred <- c(rpred, preds[minc])
      preds <- preds[-minc]
      ilp   <- length(preds)
      step  <- step + 1

      if (metric == "r2") {
        aic_f <- rsq[minc]
      } else {
        aic_f <- arsq[minc]
      }

      mi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)

      rss_f <- mi$rss
      laic  <- c(laic, mi$aic)
      lrss  <- c(lrss, mi$rss)
      less  <- c(less, mi$ess)
      lrsq  <- c(lrsq, mi$rsq)
      larsq <- c(larsq, mi$adjr)
      aics  <- c()
      ess   <- c()
      rss   <- c()
      rsq   <- c()
      arsq  <- c()

      if (progress) {
        ols_progress_display(rpred, "others")
      }

      for (i in seq_len(ilp)) {

        predictors <- preds[-i]

        m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)

        aics[i] <- m$aic
        ess[i]  <- m$ess
        rss[i]  <- m$rss
        rsq[i]  <- m$rsq
        arsq[i] <- m$adjr
      }

      da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)

      if (metric == "r2") {
        da2 <- da[order(-da$rsq), ]
        da3 <- cbind(loc = order(-rsq), da2)
      } else {
        da2 <- da[order(-da$arsq), ]
        da3 <- cbind(loc = order(-arsq), da2)
      }


      if (details) {
        ols_rsquared_removed(metric, step, rpred, preds, response, aic_f)
        
        da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)

        if (metric == "r2") {
          da2 <- da[order(-da$rsq), ]
        } else {
          da2 <- da[order(-da$arsq), ]
        }

        ols_stepwise_metrics(da2, metric, predictors, aics, rss, ess, rsq, arsq)
      }
    } else {
      end <- TRUE
      if (progress || details) {
        ols_stepwise_break(direction = "backward")
      }
    }

  }


  if (details) {
    ols_stepwise_vars(rpred, "backward")
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  metrics     <- data.frame(step     = seq_len(step),
                            variable = rpred,
                            r2       = tail(lrsq,  n = step),
                            adj_r2   = tail(larsq, n = step),
                            aic      = tail(laic,  n = step),
                            rss      = tail(lrss,  n = step),
                            ess      = tail(less,  n = step))

  out <- list(metrics = metrics,
              model   = final_model,
              others  = list(full_model = model,
                             direction  = "backward",
                             metric     = metric))

  class(out) <- "ols_step_rsquared"

  return(out)

}

ols_step_rsquared_both <- function(model, metric, include = NULL, exclude = NULL, progress = FALSE, details = FALSE) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

  response <- names(model$model)[1]
  l        <- model$model
  nam      <- coeff_names(model)
  indterms <- nam
  lenterms <- length(indterms)
  len_inc  <- length(include) + 1

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  if (progress || details) {
    ols_candidate_terms(nam, "both")
  }

  lockterm   <- c(include, exclude)
  predictors <- setdiff(nam, lockterm)
  mlen_p     <- length(predictors)
  tech       <- c("addition", "removal")

  base_model <- ols_base_model(include, response, l)

  if (metric == "r2") {
    aic_c <- summary(base_model)$r.squared
  } else {
    aic_c <- summary(base_model)$adj.r.squared
  }


  if (details) {
    ols_rsquared_init(include, metric, response, aic_c)
  }

  step      <- 0
  all_step  <- 0
  preds     <- include
  var_index <- c()
  method    <- c()
  laic      <- c()
  less      <- c()
  lrss      <- c()
  lrsq      <- c()
  larsq     <- c()

  if (progress) {
    ols_progress_init("both")
  }

  while (step < mlen_p) {

    aics <- c()
    ess  <- c()
    rss  <- c()
    rsq  <- c()
    arsq <- c()
    lpds <- length(predictors)

    for (i in seq_len(lpds)) {

      predn <- c(preds, predictors[i])

      m <- ols_regress(paste(response, "~", paste(predn, collapse = " + ")), data = l)

      aics[i] <- ols_aic(m$model)
      ess[i]  <- m$ess
      rss[i]  <- m$rss
      rsq[i]  <- m$rsq
      arsq[i] <- m$adjr
    }

    da <- data.frame(predictors = predictors, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)

    if (details) {
      ols_stepwise_metrics(da, metric, predictors, aics, rss, ess, rsq, arsq)
    }

    if (metric == "r2") {
      minc <- which(rsq == max(rsq))
      add_crit <- rsq[minc] > aic_c
    } else {
      minc <- which(arsq == max(arsq))
      add_crit <- arsq[minc] > aic_c
    }

    if (add_crit) {
      if (metric == "r2") {
        aic_c <- rsq[minc]
      } else {
        aic_c <- arsq[minc]
      }
      preds      <- c(preds, predictors[minc])
      predictors <- predictors[-minc]
      lpds       <- length(predictors)
      method     <- c(method, tech[1])
      lpreds     <- length(preds)
      var_index  <- c(var_index, preds[lpreds])
      step       <- step + 1
      all_step   <- all_step + 1
      maic       <- aics[minc]
      mess       <- ess[minc]
      mrss       <- rss[minc]
      mrsq       <- rsq[minc]
      marsq      <- arsq[minc]
      laic       <- c(laic, maic)
      less       <- c(less, mess)
      lrss       <- c(lrss, mrss)
      lrsq       <- c(lrsq, mrsq)
      larsq      <- c(larsq, marsq)

      if (progress) {
        ols_progress_display(preds, "both", "added")
        # cat(paste("=>", tail(preds, n = 1), "added"), "\n")
      }

      if (details) {
        if (metric == "r2") {
          rsq1 <- mrsq
        } else {
          rsq1 <- marsq
        }
        ols_rsquared_selected("r2", all_step, preds, response, rsq1)
        # if (metric == "r2") {
        #   cat("Step      =>", all_step, "\n")
        #   cat("Added     =>", tail(preds, n = 1), "\n")
        #   cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
        #   cat("R-Squared =>", mrsq, "\n\n")
        # } else {
        #   cat("Step      =>", all_step, "\n")
        #   cat("Added     =>", tail(preds, n = 1), "\n")
        #   cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
        #   cat("Adj. R-Squared =>", marsq, "\n\n")
        # }
      }

      if (lpreds > 1) {

        aics <- c()
        ess  <- c()
        rss  <- c()
        rsq  <- c()
        arsq <- c()
        j    <- 1

        for (i in len_inc:lpreds) {

          preda <- preds[-i]

          m <- ols_regress(paste(response, "~", paste(preda, collapse = " + ")), data = l)

          aics[j] <- ols_aic(m$model)
          ess[j]  <- m$ess
          rss[j]  <- m$rss
          rsq[j]  <- m$rsq
          arsq[j] <- m$adjr

          j <- j + 1
        }

        da <- data.frame(predictors = preds[len_inc: lpreds], aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)

        if (details) {
          ols_stepwise_metrics(da, metric, predictors, aics, rss, ess, rsq, arsq)
        }

        if (metric == "r2") {
          minc2 <- which(rsq == max(rsq))
          rem_crit <- rsq[minc2] > lrsq[all_step]
        } else {
          minc2 <- which(arsq == max(arsq))
          rem_crit <- arsq[minc2] > larsq[all_step]
        }


        if (rem_crit) {
          if(metric == "r2") {
            aic_c <- rsq[minc2]
          } else {
            aic_c <- arsq[minc2]
          }

          maic      <- aics[minc2]
          mess      <- ess[minc2]
          mrss      <- rss[minc2]
          mrsq      <- rsq[minc2]
          marsq     <- arsq[minc2]
          laic      <- c(laic, maic)
          less      <- c(less, mess)
          lrss      <- c(lrss, mrss)
          lrsq      <- c(lrsq, mrsq)
          larsq     <- c(larsq, marsq)
          var_index <- c(var_index, preds[minc2 + length(include)])
          method    <- c(method, tech[2])
          all_step  <- all_step + 1

          preds_removed <- preds[minc2 + length(include)]

          if (progress) {
            ols_progress_display(preds_removed, "both", "removed")
            # cat(paste("=>", preds[minc2 + length(include)], "removed"), "\n")
          }

          preds <- preds[-(minc2 + length(include))]
          lpreds <- length(preds)

          if (details) {
            if (metric == "r2") {
              rsq1 <- mrsq
            } else {
              rsq1 <- marsq
            }
            ols_rsquared_selected("r2", all_step, preds_removed, response, rsq1)
            # if (metric == "r2") {
            #   cat("Step      =>", all_step, "\n")
            #   cat("Removed   =>", preds[minc2 + length(include)], "\n")
            #   cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
            #   cat("R-Squared =>", mrsq, "\n\n")
            # } else {
            #   cat("Step      =>", all_step, "\n")
            #   cat("Removed   =>", preds[minc2 + length(include)], "\n")
            #   cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
            #   cat("Adj. R-Squared =>", marsq, "\n\n")
            # }
          }
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      if (progress || details) {
        ols_stepwise_break(direction = "both")
      }
      break
    }
  }

  if (details) {
    ols_stepwise_vars(preds, "both")
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  metrics     <- data.frame(step     = seq_len(all_step),
                            variable = var_index,
                            method   = method,
                            r2       = lrsq,
                            adj_r2   = larsq,
                            aic      = laic,
                            rss      = lrss,
                            ess      = less)

  out <- list(metrics = metrics,
              model   = final_model,
              others   = list(base_model = base_model,
                              direction  = "both",
                              metric     = metric))

  class(out) <- "ols_step_rsquared"

  return(out)

}
