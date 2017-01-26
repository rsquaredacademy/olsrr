#' @title Score Test for heteroskedasticity
#' @description Test for heteroskedasticity
#' @param model an object of class \code{lm}
#' @param fitted_values logical; if TRUE, use fitted values of regression model
#' @param rhs logical; if TRUE, specifies that tests for heteroskedasticity be
#' performed for the right-hand-side (explanatory) variables of the fitted
#' regression model
#' @param vars variables to be used for for heteroskedasticity test
#' @return \code{score_test} returns an object of class \code{"score_test"}.
#' An object of class \code{"score_test"} is a list containing the
#' following components:
#'
#' \item{score}{f statistic}
#' \item{p}{p value of \code{score}}
#' \item{df}{degrees of freedom}
#' \item{fv}{fitted values of the regression model}
#' \item{rhs}{name of explanatory variables of fitted regression model}
#' \item{resp}{response variable}
#' \item{preds}{predictors}
#' @export
#'
score_test <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL) UseMethod('score_test')

#' @export
#'
score_test.default <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL) {

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

    out <- list(score = round(d$score, 3),
    	          p     = round(d$p, 3),
    	          df    = d$np,
    					  fv    = fitted_values,
    					  rhs   = rhs,
    					  preds = d$preds,
    					  resp  = resp)

    class(out) <- 'score_test'
    return(out)

}

#' @export
#'
print.score_test <- function(x, ...) {
	print_score_test(x, ...)
}
