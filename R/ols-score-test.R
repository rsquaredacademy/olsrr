#' @title Score Test for heteroskedasticity
#' @description Test for heteroskedasticity under the assumption that
#' the errors are independent and identically distributed (i.i.d.).
#' @param model an object of class \code{lm}
#' @param fitted_values logical; if TRUE, use fitted values of regression model
#' @param rhs logical; if TRUE, specifies that tests for heteroskedasticity be
#' performed for the right-hand-side (explanatory) variables of the fitted
#' regression model
#' @param vars variables to be used for for heteroskedasticity test
#' @return \code{ols_score_test} returns an object of class \code{"ols_score_test"}.
#' An object of class \code{"ols_score_test"} is a list containing the
#' following components:
#'
#' \item{score}{f statistic}
#' \item{p}{p value of \code{score}}
#' \item{df}{degrees of freedom}
#' \item{fv}{fitted values of the regression model}
#' \item{rhs}{names of explanatory variables of fitted regression model}
#' \item{resp}{response variable}
#' \item{preds}{predictors}
#' @references Breusch, T. S. and Pagan, A. R. (1979) A simple test for heteroscedasticity and random coefficient variation. Econometrica 47, 1287–1294.
#' 
#' Cook, R. D. and Weisberg, S. (1983) Diagnostics for heteroscedasticity in regression. Biometrika 70, 1–10.
#' 
#' Koenker, R. 1981. A note on studentizing a test for heteroskedasticity. Journal of Econometrics 17: 107–112.
#' 
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#'
#' # using fitted values of the model
#' ols_score_test(model)
#'
#' # using predictors from the model
#' ols_score_test(model, rhs = TRUE)
#'
#' # specify predictors from the model
#' ols_score_test(model, vars = c('disp', 'wt'))
#' @export
#'
ols_score_test <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL) UseMethod('ols_score_test')

#' @export
#'
ols_score_test.default <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS regression model.', call. = FALSE)
    }

    if (!is.logical(fitted_values)) {
        stop('fitted_values must be either TRUE or FALSE')
    }

    if (!is.logical(rhs)) {
        stop('rhs must be either TRUE or FALSE')
    }

    if (length(vars) > 0) {
        if (!all(vars %in% names(model$coefficients))) {
            stop('vars must be a subset of the predictors in the model')
        }
    	fitted_values <- FALSE
    }

    resp <- model %>% model.frame() %>% names() %>% `[`(1)

    if (rhs) {

    fitted_values <- FALSE
    d <- rhsout(model)

    } else {

    	if (fitted_values) {
		d <- fitout(model, resp)
    	} else {
        d <- varout(model, vars)
    	}
    }

    out <- list(score = d$score, 
    	          p     = d$p, 
    	          df    = d$np,
    					  fv    = fitted_values,
    					  rhs   = rhs,
    					  preds = d$preds,
    					  resp  = resp)

    class(out) <- 'ols_score_test'
    return(out)

}

#' @export
#'
print.ols_score_test <- function(x, ...) {
	print_score_test(x, ...)
}
