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
  aics     <- c()
  ess      <- c()
  rss      <- c()
  rsq      <- c()
  arsq     <- c()

  if (is.null(include)) {
    base_model <- lm(paste(response, "~", 1), data = l)
  } else {
    base_model <- lm(paste(response, "~", paste(include, collapse = " + ")), data = l)
  }

  if (metric == "r2") {
    rsq_base <- summary(base_model)$r.squared  
  } else {
    rsq_base <- summary(base_model)$adj.r.squared
  }
  

  if (progress || details) {
    cat(format("Forward Selection Method", justify = "left", width = 24), "\n")
    cat(rep("-", 24), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")
  }

  if (details) {
    cat("\n")
    if (is.null(include)) {
      if (metric == "r2") {
        cat("Step      => 0", "\n")
        cat("Model     =>", paste(response, "~", 1, "\n"))
        cat("R-Squared =>", rsq_base, "\n\n")
      } else {
        cat("Step           => 0", "\n")
        cat("Model          =>", paste(response, "~", 1, "\n"))
        cat("Adj. R-Squared =>", rsq_base, "\n\n")
      }
    } else { 
      if (metric == "r2") {
        cat("Step      => 0", "\n")
        cat("Model     =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
        cat("R-Squared =>", rsq_base, "\n\n")
      } else {
        cat("Step           => 0", "\n")
        cat("Model          =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
        cat("Adj. R-Squared =>", rsq_base, "\n\n")
      }
    }
    cat("Initiating stepwise selection...", "\n\n")
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
    w1 <- max(nchar("Predictor"), nchar(all_pred))
    w2 <- 2
    w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
    w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
    w5 <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
    w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
    w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
    w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
    ln <- length(aics)

    if (metric == 'r2') {
      cat(format("R-Squared Table", justify = "centre", width = w), "\n")
    } else {
      cat(format("Adjusted R-Squared Table", justify = "centre", width = w), "\n")
    }
    cat(rep("-", w), sep = "", "\n")
    cat(
      fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
      fc("Sum Sq", w4), fs(), fc("RSS", w5), fs(), fc("R-Sq", w6), fs(),
      fc("Adj. R-Sq", w7), "\n"
    )
    cat(rep("-", w), sep = "", "\n")

    for (i in seq_len(ln)) {
      cat(
        fl(da2[i, 1], w1), fs(), fg(1, w2), fs(),
        fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
        fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(),
        fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
        fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(),
        fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
      )
    }

    cat(rep("-", w), sep = "", "\n\n")
  }

  if (metric == "r2") {
    minc <- which(rsq == max(rsq))
  } else {
    minc <- which(arsq == max(arsq))
  }

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

  if (progress) {
    cat("\n")
    if (!details) {
      cat("Variables Entered:", "\n\n")
    }
  }

  if (progress) {
    cat(paste("=>", tail(preds, n = 1)), "\n")
  }

  while (step < mlen_p) {

    aics <- c()
    ess  <- c()
    rss  <- c()
    rsst <- c()
    rsq  <- c()
    arsq <- c()
    mo   <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
    rsq1 <- mo$rsq

    if (details) {
      if (metric == "r2") {
        cat("Step      =>", step, "\n")
        cat("Selected  =>", tail(preds, n = 1), "\n")
        cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
        cat("R-Squared =>", rsq1, "\n\n")
      } else {
        cat("Step           =>", step, "\n")
        cat("Selected       =>", tail(preds, n = 1), "\n")
        cat("Model          =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
        cat("Adj. R-Squared =>", rsq1, "\n\n")
      }
    }

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      k          <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)
      aics[i]    <- ols_aic(k$model)
      ess[i]     <- k$ess
      rsst[i]    <- k$rss
      rss[i]     <- round(k$rss - mo$rss, 3)
      rsq[i]     <- k$rsq
      arsq[i]    <- k$adjr
    }

    if (details) {

      da  <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
      da2 <- da[order(-da$rsq), ]
      w1  <- max(nchar("Predictor"), nchar(as.character(da2$predictors)))
      w2  <- 2
      w3  <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
      w4  <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
      w5  <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
      w6  <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
      w7  <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
      w   <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
      ln  <- length(aics)

      if (metric == "r2") {
        cat(format("R-Squared Table", justify = "centre", width = w), "\n")  
      } else {
        cat(format("Adj. R-Squared Table", justify = "centre", width = w), "\n")  
      }
      
      cat(rep("-", w), sep = "", "\n")
      cat(
        fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
        fc("Sum Sq", w4), fs(), fc("RSS", w5), fs(), fc("R-Sq", w6), fs(),
        fc("Adj. R-Sq", w7), "\n"
      )
      cat(rep("-", w), sep = "", "\n")

      for (i in seq_len(ln)) {
        cat(
          fl(da2[i, 1], w1), fs(), fg(1, w2), fs(),
          fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
          fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(),
          fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
          fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(),
          fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
        )
      }

      cat(rep("-", w), sep = "", "\n\n")
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
      mrss     <- round(rsst[minaic], 3)
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
        cat(paste("=>", tail(preds, n = 1)), "\n")
      }
    } else {
      if (progress || details) {
        cat("\n")
        cat("No more variables to be added.", "\n")
        cat("\n")
      }
      break
    }
  }

  if (details) {
    cat("\n")
    cat("Variables Selected:", "\n\n")
    for (i in seq_len(length(preds))) {
      cat(paste("=>", preds[i]), "\n")
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

  if (metric == "r2") {
    aic_f <- summary(model)$r.squared  
  } else {
    aic_f <- summary(model)$adj.r.squared
  }

  mi    <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
  rss_f <- mi$rss
  laic  <- aic_f
  lrss  <- rss_f
  less  <- mi$ess
  lrsq  <- mi$rsq
  larsq <- mi$adjr

  if (progress || details) {
    cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
    cat(rep("-", 27), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(cterm))) {
      cat(paste(i, ".", cterm[i]), "\n")
    }
    cat("\n")
  }

  if (details) { 
    if (metric == "r2") {
      cat("Step      => 0", "\n")
      cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
      cat("R-Squared =>", aic_f, "\n\n")  
    } else {
      cat("Step           => 0", "\n")
      cat("Model          =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
      cat("Adj. R-Squared =>", aic_f, "\n\n")
    }
    cat("Initiating stepwise selection...", "\n\n")
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
    rss[i]  <- rss_f - m$rss
    rsq[i]  <- m$rsq
    arsq[i] <- m$adjr
  }

  da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
  da2 <- da[order(-da$arsq), ]
  da3 <- cbind(loc = order(-arsq), da2)

  if (details) {
    w1 <- max(nchar("Predictor"), nchar(predictors))
    w2 <- 2
    w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
    w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
    w5 <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
    w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
    w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
    w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
    ln <- length(aics)

    if (metric == "r2") {
      cat(format("R-Squared Table", justify = "centre", width = w), "\n")  
    } else {
      cat(format("Adj. R-Squared Table", justify = "centre", width = w), "\n")
    }
    
    cat(rep("-", w), sep = "", "\n")
    cat(
      fl("Removed", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
      fc("Sum Sq", w4), fs(), fc("ESS", w5), fs(), fc("R-Sq", w6), fs(),
      fc("Adj. R-Sq", w7), "\n"
    )
    cat(rep("-", w), sep = "", "\n")

    for (i in seq_len(ln)) {
      cat(
        fl(da2[i, 1], w1), fs(), fc(1, w2), fs(), fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
        fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(), fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
        fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(), fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
      )
    }

    cat(rep("-", w), sep = "", "\n\n")
  }

  linc <- length(include)

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
      lrss  <- c(lrss, rss_f)
      less  <- c(less, mi$ess)
      lrsq  <- c(lrsq, mi$rsq)
      larsq <- c(larsq, mi$adjr)
      aics  <- c()
      ess   <- c()
      rss   <- c()
      rsq   <- c()
      arsq  <- c()

      if (progress) {
        cat("\n")
        cat("Variables Removed:", "\n\n")
        cat(paste("=>", tail(rpred, n = 1)), "\n")
      }

      for (i in seq_len(ilp)) {

        predictors <- preds[-i]

        m <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)

        aics[i] <- m$aic
        ess[i]  <- m$ess
        rss[i]  <- rss_f - m$rss
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
        if (metric == "r2") {
          cat("Step      =>", step, "\n")
          cat("Removed   =>", tail(rpred, n = 1), "\n")
          cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
          cat("R-Squared =>", aic_f, "\n\n")  
        } else {
          cat("Step           =>", step, "\n")
          cat("Removed        =>", tail(rpred, n = 1), "\n")
          cat("Model          =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
          cat("Adj. R-Squared =>", aic_f, "\n\n")
        }

        da  <- data.frame(predictors = preds, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)

        if (metric == "r2") {
          da2 <- da[order(-da$rsq), ]
        } else {
          da2 <- da[order(-da$arsq), ]
        }

        w1  <- max(nchar("Predictor"), nchar(predictors))
        w2  <- 2
        w3  <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
        w4  <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
        w5  <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
        w6  <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
        w7  <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
        w   <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
        ln  <- length(aics)

        if (metric == "r2") {
          cat(format("R-Squared Table", justify = "centre", width = w), "\n")  
        } else {
          cat(format("Adj. R-Squared Table", justify = "centre", width = w), "\n")
        }
        
        cat(rep("-", w), sep = "", "\n")
        cat(
          fl("Removed", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
          fc("Sum Sq", w4), fs(), fc("RSS", w5), fs(), fc("R-Sq", w6), fs(),
          fc("Adj. R-Sq", w7), "\n"
        )
        cat(rep("-", w), sep = "", "\n")

        for (i in seq_len(ln)) {
          cat(
            fl(da2[i, 1], w1), fs(), fc(1, w2), fs(), fg(format(round(da2[i, 2], 3), nsmall = 3), w3), fs(),
            fg(format(round(da2[i, 4], 3), nsmall = 3), w4), fs(), fg(format(round(da2[i, 3], 3), nsmall = 3), w5), fs(),
            fg(format(round(da2[i, 5], 3), nsmall = 3), w6), fs(), fg(format(round(da2[i, 6], 3), nsmall = 3), w7), "\n"
          )
        }

        cat(rep("-", w), sep = "", "\n\n")
      }
    } else {
      end <- TRUE
      if (progress || details) {
        cat("\n")
        cat("No more variables to be removed.")
        cat("\n")
      }
    }

  }


  if (details) {
    if (length(rpred) > 0) {
      cat("\n\n")
      cat("Variables Removed:", "\n\n")
      for (i in seq_len(length(rpred))) {
        cat(paste("=>", rpred[i]), "\n")
      }
    }
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

  lockterm   <- c(include, exclude)
  predictors <- setdiff(nam, lockterm)
  mlen_p     <- length(predictors)
  tech       <- c("addition", "removal")

  if (progress || details) {
    cat(format("Stepwise Selection Method", justify = "left", width = 25), "\n")
    cat(rep("-", 25), sep = "", "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
    for (i in seq_len(length(nam))) {
      cat(paste(i, ".", nam[i]), "\n")
    }
    cat("\n")
  }

  if (is.null(include)) {
    base_model <- lm(paste(response, "~", 1), data = l)
  } else {
    base_model <- lm(paste(response, "~", paste(include, collapse = " + ")), data = l)
  }

  if (metric == "r2") {
    aic_c <- summary(base_model)$r.squared  
  } else {
    aic_c <- summary(base_model)$adj.r.squared  
  }
  

  if (details) {
    cat("\n")
      if (is.null(include)) {
        if (metric == "r2") {
          cat("Step           => 0", "\n")
          cat("Model          =>", paste(response, "~", 1, "\n"))
          cat("Adj. R-Squared =>", aic_c, "\n\n")
        } else {
          cat("Step           => 0", "\n")
          cat("Model          =>", paste(response, "~", 1, "\n"))
          cat("Adj. R-Squared =>", aic_c, "\n\n")
        }
      } else {
        if (metric == "r2") {
          cat("Step      => 0", "\n")
          cat("Model     =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
          cat("R-Squared =>", aic_c, "\n\n")
        } else {
          cat("Step           => 0", "\n")
          cat("Model          =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
          cat("Adj. R-Squared =>", aic_c, "\n\n")
        }
      }
      cat("Initiating stepwise selection...", "\n\n")
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
    cat("\n")
    cat("Variables Entered/Removed:", "\n\n")
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
      w1 <- max(nchar("Predictor"), nchar(predictors))
      w2 <- 2
      w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
      w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
      w5 <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
      w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
      w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
      w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
      ln <- length(aics)

      cat(fc("  Enter New Variables", w), sep = "", "\n")
      cat(rep("-", w), sep = "", "\n")
      cat(
        fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
        fc("Sum Sq", w4), fs(), fc("ESS", w5), fs(), fc("R-Sq", w6), fs(),
        fc("Adj. R-Sq", w7), "\n"
      )
      cat(rep("-", w), sep = "", "\n")

      for (i in seq_len(ln)) {
        cat(
          fl(da[i, 1], w1), fs(), 
          fg(1, w2), fs(), 
          fg(format(round(da[i, 2], 3), nsmall = 3), w3), fs(),
          fg(format(round(da[i, 4], 3), nsmall = 3), w4), fs(), 
          fg(format(round(da[i, 3], 3), nsmall = 3), w5), fs(),
          fg(format(round(da[i, 5], 3), nsmall = 3), w6), fs(),
          fg(format(round(da[i, 6], 3), nsmall = 3), w7), "\n"
        )
      }

      cat(rep("-", w), sep = "", "\n\n")
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
        cat(paste("=>", tail(preds, n = 1), "added"), "\n")
      }

      if (details) {
        if (metric == "r2") {
          cat("Step      =>", all_step, "\n")
          cat("Added     =>", tail(preds, n = 1), "\n")
          cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
          cat("R-Squared =>", mrsq, "\n\n")
        } else {
          cat("Step      =>", all_step, "\n")
          cat("Added     =>", tail(preds, n = 1), "\n")
          cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
          cat("Adj. R-Squared =>", marsq, "\n\n")
        }
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
          w1 <- max(nchar("Predictor"), nchar(preds))
          w2 <- 2
          w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
          w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
          w5 <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
          w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
          w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
          w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
          ln <- length(aics)

          cat(fc("Remove Existing Variables", w), sep = "", "\n")
          cat(rep("-", w), sep = "", "\n")
          cat(
            fl("Variable", w1), fs(), fc("DF", w2), fs(), fc("AIC", w3), fs(),
            fc("Sum Sq", w4), fs(), fc("RSS", w5), fs(), fc("R-Sq", w6), fs(),
            fc("Adj. R-Sq", w7), "\n"
          )
          cat(rep("-", w), sep = "", "\n")

          for (i in seq_len(ln)) {
            cat(
              fl(da[i, 1], w1), fs(), 
              fg(1, w2), fs(), 
              fg(format(round(da[i, 2], 3), nsmall = 3), w3), fs(),
              fg(format(round(da[i, 4], 3), nsmall = 3), w4), fs(), 
              fg(format(round(da[i, 3], 3), nsmall = 3), w5), fs(),
              fg(format(round(da[i, 5], 3), nsmall = 3), w6), fs(),
              fg(format(round(da[i, 6], 3), nsmall = 3), w7), "\n"
            )
          }

          cat(rep("-", w), sep = "", "\n\n")
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

          if (progress) {
            cat(paste("=>", preds[minc2 + length(include)], "removed"), "\n")
          }

          preds <- preds[-(minc2 + length(include))]
          lpreds <- length(preds)

          if (details) {
            if (metric == "r2") {
              cat("Step      =>", all_step, "\n")
              cat("Removed   =>", preds[minc2 + length(include)], "\n")
              cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
              cat("R-Squared =>", mrsq, "\n\n")
            } else {
              cat("Step      =>", all_step, "\n")
              cat("Removed   =>", preds[minc2 + length(include)], "\n")
              cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
              cat("Adj. R-Squared =>", marsq, "\n\n")
            }
          }
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      if (progress || details) {
        cat("\n")
        cat("No more variables to be added or removed.")
        cat("\n")
      }
      break
    }
  }

  if (details) {
    cat("\n")
    cat("Variables Selected:", "\n\n")
    for (i in seq_len(length(preds))) {
      cat(paste("=>", preds[i]), "\n")
    }
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
