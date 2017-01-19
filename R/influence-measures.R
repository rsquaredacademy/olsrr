#' @importFrom stats hatvalues
#' @title Leverage
#' @description Leverage
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return leverage of the fitted model
#' @export
#'
leverage <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    lev <- model %>% hatvalues() %>% unname()
    return(lev)

}


#' @title Hadi's Measure
#' @description Hadi's Measure
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return hadi's measure
#' @export
#'
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

#' @title PRESS Statistic
#' @description PRESS Statistic
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return PRESS Statistic
#' @export
#'
press <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    hm  <- unname(hatvalues(model))
    pr  <- residuals(model) / (1 - hm)
    pr2 <- sum(pr ^ 2)

    return(pr2)

}

#' @title Predicted Rsquare
#' @description Predicted Rsquare
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return Predicted Rsquare
#' @export
#'
pred_rsq <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    tss     <- sum(anova(model)$`Sum Sq`)
    prs     <- press(model)
    predrsq <- round(1 - (prs / tss), 4)

    return(predrsq)

}
