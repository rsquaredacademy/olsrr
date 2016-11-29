leverage <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    return(unname(hatvalues(model)))

}


hadi <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    lev       <- unname(hatvalues(model))
    pii       <- 1 - lev
    q         <- model$rank
    p         <- q - 1
    aov_m     <- anova(model)
    dii       <- (model$residuals / sqrt(aov_m[q, 2])) ^ 2
    potential <- lev / pii
    residual  <- ((p + 1) / pii) * (dii / (1 - dii))
    hi        <- potential + residual

    result <- list(hadi      = hi,
                   potential = potential,
                   residual  = residual)
    
    return(result)
}


press <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    hm  <- unname(hatvalues(model))
    pr  <- residuals(model) / (1 - hm)
    pr2 <- sum(pr ^ 2)

    return(pr2)

}



pred_rsq <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    tss     <- sum(anova(model)$`Sum Sq`)
    prs     <- press(model)
    predrsq <- round(1 - (prs / tss), 4)

    return(predrsq)

}











