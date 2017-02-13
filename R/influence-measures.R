#' @importFrom stats hatvalues
#' @title Leverage
#' @description The leverage of an observation is based on how much the observation's value on the predictor variable 
#' differs from the mean of the predictor variable. The greater an observation's leverage, the more potential it has 
#' to be an influential observation.
#' @param model an object of class \code{lm}
#' @return leverage of the fitted model
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' leverage(model)
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
#' @description Measure of influence based on the fact that influential observations in either
#' the response variable or in the predictors or both.
#' @param model an object of class \code{lm}
#' @return hadi's measure
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' hadi_plot(model)
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
#' @description PRESS tells you how well the model will predict new data.
#' @param model an object of class \code{lm}
#' @return PRESS Statistic
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' press(model)
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
#' @description Use predicted rsquared to determine how well the model predicts responses for new observations. 
#' Larger values of predicted R2 indicate models of greater predictive ability.
#' @param model an object of class \code{lm}
#' @return Predicted Rsquare
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' pred_rsq(model)
#' @export
#'
pred_rsq <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    tss <- model %>%
      anova() %>%
      `[[`(2) %>%
      sum()

    prts <- model %>%
      press() %>%
      `/`(tss)

    predrsq <- 1 %>%
      `-`(prts) %>%
      round(4)

    return(predrsq)

}
