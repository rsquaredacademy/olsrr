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

    potential <- hadipot(model)
     residual <- hadires(model)
           hi <- potential + residual

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

  k <- 1 %>%
    `-`(model %>% leverage())

  out <- model %>%
          residuals %>%
          `/`(k) %>%
          `^`(2) %>%
          sum()


    return(out)

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

    tss <- model %>%
      anova() %>%
      `$`(`Sum Sq`) %>%
      sum()

    prts <- model %>%
      press() %>%
      `/`(tss)

    predrsq <- 1 %>%
      `-`(prts) %>%
      round(4)

    return(predrsq)

}
