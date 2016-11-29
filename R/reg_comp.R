reg_comp <- function(formula, data, conf.level = 0.95, title = 'model') {

	  model      <- lm(formula =  formula, data = data)
    nam        <- names(model.frame(model))
    response   <- nam[1]
    predictors <- nam[-1]
    output     <- summary(model)
    anovam     <- anova(model)
    dep        <- model.frame(model)[1]
    rsq        <- round(output$r.squared, 3)                         
    r          <- round(sqrt(rsq), 3)                                  
    adjr       <- round(output$adj.r.squared, 3)                    
    sigma      <- round(output$sigma, 3)                           
    cv         <- round((output$sigma / mean(dep[[1]])) * 100, 3)          
    mae        <- round(mean(abs(residuals(model))), 3)              
    aic        <- round(aic(model), 3)                               
    sbc        <- round(sbc(model), 3)                               
    sbic       <- round(sbic(model, model), 3)                      
    prsq       <- round(pred_rsq(model), 3)                         
    n          <- length(anovam$Df)
    mse       <- round(anovam$`Mean Sq`[n], 3)         
    error_df   <- anovam$Df[n]                        
    model_df   <- sum(anovam$Df) - error_df
    total_df   <- sum(anovam$Df)
    ess        <- round(anovam$`Sum Sq`[n], 3)                       
    tss        <- round(sum(anovam$`Sum Sq`), 3)                     
    rss        <- round(tss - ess, 3)                                
    rms        <- round(rss / model_df, 3)                           
    ems        <- round(ess / error_df, 3)                           
    f          <- round(rms / ems, 3)
    p          <- round(pf(f, model_df, error_df, lower.tail = F), 4)
    b          <- output$coef[-1, 1]
    g          <- as.data.frame(model.matrix(model)[, -1])
    sx         <- sapply(g, sd)
    sy         <- sapply(model$model[1], sd)
    sbeta      <- b * sx/sy
    betas      <- round(coefficients(model), 3)                   
    sbetas     <- round(sbeta, 3)                                 
    std_errors <- round(output$coefficients[, 2], 3)          
    tvalues    <- round(output$coefficients[, 3], 3)             
    pvalues    <- round(output$coefficients[, 4], 3)             
    df         <- rep(1, n)                                           
    conf_lm    <- round(confint.lm(model, level = conf.level), 3)     
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