#' @importFrom stats model.matrix confint.lm
#' @importFrom recipes recipe step_center step_scale prep bake all_numeric
#' @importFrom glue glue
#' @importFrom magrittr extract2
#' @importFrom stats as.formula
reg_comp <- function(formula, data, conf.level = 0.95, iterm, title = "model") {
  model <- lm(formula = formula, data = data)
  nam <- names(model.frame(model))
  response <- nam[1]
  predictors <- nam[-1]
  output <- summary(model)
  anovam <- anova(model)
  dep <- model.frame(model)[1]
  rsq <- output$r.squared
  r <- sqrt(rsq)
  adjr <- output$adj.r.squared
  sigma <- output$sigma
  cv <- (output$sigma / mean(dep[[1]])) * 100
  mae <- mean(abs(residuals(model)))
  aic <- ols_aic(model)
  sbc <- ols_sbc(model)
  sbic <- ols_sbic(model, model)
  prsq <- ols_pred_rsq(model)
  n <- length(anovam$Df)
  mse <- anovam$`Mean Sq`[n]
  error_df <- anovam$Df[n]
  model_df <- sum(anovam$Df) - error_df
  total_df <- sum(anovam$Df)
  ess <- anovam$`Sum Sq`[n]
  tss <- sum(anovam$`Sum Sq`)
  rss <- tss - ess
  rms <- rss / model_df
  ems <- ess / error_df
  f <- rms / ems
  p <- pf(f, model_df, error_df, lower.tail = F)

  # standardised betas
  if (iterm) {
    data_scaled <- data
    mod_formula <- formula %>%
      extract2(2) %>%
      glue(" ~ .") %>%
      as.formula()
    rec_obj <- recipe(mod_formula, data = data)
    standardized <- rec_obj %>%
      step_center(all_numeric()) %>%
      step_scale(all_numeric())
    trained_rec <- prep(standardized, training = data)
    newdata <- bake(trained_rec, newdata = data_scaled)
    model2 <- lm(formula, data = newdata)
    output2 <- summary(model2)

    b <- output2$coef[-1, 1]
    g <- as.data.frame(model.matrix(model2)[, -1])
    sx <- sapply(g, sd)
    sy <- sapply(model2$model[1], sd)
    sbeta <- b * sx / sy
    sbetas <- sbeta
  } else {
    b <- output$coef[-1, 1]
    g <- as.data.frame(model.matrix(model)[, -1])
    sx <- sapply(g, sd)
    sy <- sapply(model$model[1], sd)
    sbeta <- b * sx / sy
    sbetas <- sbeta
  }

  betas <- coefficients(model)
  std_errors <- output$coefficients[, 2]
  tvalues <- output$coefficients[, 3]
  pvalues <- output$coefficients[, 4]
  df <- rep(1, n)
  conf_lm <- confint.lm(model, level = conf.level)
  mvars <- names(model$coefficients)


  result <- list(
    r = r,
    rsq = rsq,
    adjr = adjr,
    sigma = sigma,
    cv = cv,
    mse = mse,
    mae = mae,
    aic = aic,
    sbc = sbc,
    sbic = sbic,
    prsq = prsq,
    error_df = error_df,
    model_df = model_df,
    total_df = total_df,
    ess = ess,
    rss = rss,
    tss = tss,
    rms = rms,
    ems = ems,
    f = f,
    p = p,
    n = n,
    betas = betas,
    sbetas = sbetas,
    std_errors = std_errors,
    tvalues = tvalues,
    pvalues = pvalues,
    df = df,
    conf_lm = conf_lm,
    title = title,
    dependent = response,
    predictors = predictors,
    mvars = mvars,
    model = model
  )

  return(result)
}
