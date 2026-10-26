#' @importFrom stats model.matrix confint.lm
#' @importFrom stats as.formula deviance
reg_comp <- function(formula, data, conf.level = 0.95, iterm, title = "model", model = NULL) {

  if (is.null(model)) {
    model <- lm(formula = formula, data = data)
  }
  nam        <- names(model.frame(model))
  response   <- nam[1]

  # a model frame adds synthetic columns for prior weights and offsets; neither
  # is a predictor
  predictors <- setdiff(nam[-1], synthetic_model_columns(model))
  output     <- summary(model)
  anovam     <- anova(model)
  dep        <- model.frame(model)[1]
  rsq        <- output$r.squared
  r          <- sqrt(rsq)
  adjr       <- output$adj.r.squared
  ems        <- deviance(model) / model$df.residual
  mse        <- ems
  rmse       <- sqrt(ems)
  cv         <- (output$sigma / mean(dep[[1]])) * 100
  mae        <- mean(abs(model_residuals_used(model)))
  aic        <- ols_aic(model)
  sbc        <- ols_sbc(model)
  sbic       <- ols_sbic(model, model)
  prsq       <- ols_pred_rsq(model)
  n          <- length(anovam$Df)
  error_df   <- anovam$Df[n]
  model_df   <- sum(anovam$Df) - error_df
  total_df   <- sum(anovam$Df)
  ess        <- anovam$`Sum Sq`[n]
  tss        <- sum(anovam$`Sum Sq`)
  rss        <- tss - ess
  rms        <- rss / model_df
  ems        <- ess / error_df
  f          <- rms / ems
  p          <- pf(f, model_df, error_df, lower.tail = F)

  # standardised betas
  int <- attr(model$terms, "intercept")

  if ((ncol(model.matrix(model)) - int) < 1) {

    # intercept-only model: no predictors to standardise
    sbetas <- numeric(0)

  } else if (iterm) {

    # scale only the variables used by the model; the response (which may be
    # a call such as log(y)) and any other columns, including the weights, are
    # left untouched
    data_scaled <- as.data.frame(data)
    resp        <- unique(c(all.vars(formula)[1], nam[1]))
    model_vars  <- setdiff(all.vars(attr(model$terms, "variables")), resp)

    is_num     <- vapply(data_scaled, is.numeric, logical(1))
    scale_cols <- intersect(names(data_scaled)[is_num], model_vars)

    newdata <- data_scaled[, setdiff(names(data_scaled), scale_cols), drop = FALSE]
    if (length(scale_cols) > 0) {
      num_scale <- as.data.frame(lapply(data_scaled[, scale_cols, drop = FALSE], scale))
      newdata   <- cbind(newdata, num_scale)
    }

    if (!is.null(model$weights)) {
      # carry the weights in a column whose name cannot clash with the data
      # (passing `weights = w` would otherwise be looked up in `newdata`,
      # where a column named `w` has been scaled and may be negative)
      wt_name <- ".ols_wt"
      while (wt_name %in% names(newdata)) {
        wt_name <- paste0(wt_name, "_")
      }
      newdata[[wt_name]] <- unname(model$weights)
      model2 <- eval(bquote(
        lm(formula, data = newdata, weights = .(as.name(wt_name)))
      ))
    } else {
      model2 <- lm(formula, data = newdata)
    }
    output2   <- summary(model2)
    b         <- output2$coef[, 1]
    g         <- as.data.frame(model.matrix(model2))
    if (int) {
      b <- b[-1]
      g <- g[, -1, drop = FALSE]
    }
    sx        <- sapply(g, sd)
    sy        <- sd(model.response(model.frame(model)))
    sbeta     <- b * sx / sy
    sbetas    <- sbeta

  } else {

    # keep every coefficient when there is no intercept
    b        <- output$coef[, 1]
    g        <- as.data.frame(model.matrix(model))
    if (int) {
      b <- b[-1]
      g <- g[, -1, drop = FALSE]
    }
    sx       <- sapply(g, sd)
    sy       <- sapply(model$model[1], sd)
    sbeta    <- b * sx / sy
    sbetas   <- sbeta

  }

  betas      <- coefficients(model)
  std_errors <- output$coefficients[, 2]
  tvalues    <- output$coefficients[, 3]
  pvalues    <- output$coefficients[, 4]
  df         <- rep(1, n)
  conf_lm    <- confint.lm(model, level = conf.level)
  mvars      <- names(model$coefficients)


  result <- list(
    r          = r,
    rsq        = rsq,
    adjr       = adjr,
    rmse       = rmse,
    cv         = cv,
    mse        = mse,
    mae        = mae,
    aic        = aic,
    sbc        = sbc,
    sbic       = sbic,
    prsq       = prsq,
    error_df   = error_df,
    model_df   = model_df,
    total_df   = total_df,
    ess        = ess,
    rss        = rss,
    tss        = tss,
    rms        = rms,
    ems        = ems,
    f          = f,
    p          = p,
    n          = n,
    betas      = betas,
    sbetas     = sbetas,
    std_errors = std_errors,
    tvalues    = tvalues,
    pvalues    = pvalues,
    df         = df,
    conf_lm    = conf_lm,
    title      = title,
    dependent  = response,
    predictors = predictors,
    mvars      = mvars,
    model      = model
  )

  return(result)
}
