#' @importFrom stats model.matrix confint.lm
reg_comp <- function(formula, data, conf.level = 0.95, title = 'model') {

	model      <- lm(formula = formula, data = data)
    nam        <- names(model.frame(model))
    response   <- nam[1]
    predictors <- nam[-1]
    output     <- summary(model)
    anovam     <- anova(model)
    dep        <- model.frame(model)[1]
    rsq        <- output$r.squared
    r          <- sqrt(rsq)
    adjr       <- output$adj.r.squared
    sigma      <- output$sigma
    cv         <- (output$sigma / mean(dep[[1]])) * 100
    mae        <- mean(abs(residuals(model)))
    aic        <- ols_aic(model)
    sbc        <- ols_sbc(model)
    sbic       <- ols_sbic(model, model)
    prsq       <- ols_pred_rsq(model)
    n          <- length(anovam$Df)
    mse        <- anovam$`Mean Sq`[n]
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
    b          <- output$coef[-1, 1]
    g          <- as.data.frame(model.matrix(model)[, -1])
    sx         <- sapply(g, sd)
    sy         <- sapply(model$model[1], sd)
    sbeta      <- b * sx/sy
    betas      <- coefficients(model)
    sbetas     <- sbeta
    std_errors <- output$coefficients[, 2]
    tvalues    <- output$coefficients[, 3]
    pvalues    <- output$coefficients[, 4]
    df         <- rep(1, n)
    conf_lm    <- confint.lm(model, level = conf.level)
    mvars      <- names(model$coefficients)


    result <- list(r          = r,
                   rsq        = rsq,
                   adjr       = adjr,
                   sigma      = sigma,
                   cv         = cv,
                   mse       = mse,
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
                   model      = model)

	return(result)

}
