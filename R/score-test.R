#' @title Score Test for heteroskedasticity
#' @description Test for heteroskedasticity
#' @param model an object of class \code{lm}
#' @param fitted.values logical; if TRUE, use fitted values of regression model
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
score_test <- function(model, fitted.values = TRUE, rhs = FALSE, vars = NULL) UseMethod('score_test')

#' @export
#'
score_test.default <- function(model, fitted.values = TRUE, rhs = FALSE, vars = NULL) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS regression model.', call. = FALSE)
    }

    if (!is.logical(fitted.values)) {
        stop('fitted.values must be either TRUE or FALSE')
    }

    if (!is.logical(rhs)) {
        stop('rhs must be either TRUE or FALSE')
    }

    if (length(vars) > 0) {
        if (!all(vars %in% names(model$coefficients))) {
            stop('vars must be a subset of the predictors in the model')
        }
    	fitted.values <- FALSE
    }

    l    <- model.frame(model)
    resp <- names(l)[1]
    n    <- nrow(l)

    if (rhs) {

    fitted.values <- FALSE
    nam           <- names(l)[-1]
    np            <- length(nam)
    var_resid     <- sum(residuals(model) ^ 2) / n
	ind           <- residuals(model) ^ 2 / var_resid - 1
	l             <- cbind(l, ind)
	mdata         <- l[-1]
	model1        <- lm(ind ~ ., data = mdata)
	score         <- summary(model1)$r.squared * n
	p             <- pchisq(score, np, lower.tail = F)
    preds         <- nam


    } else {

    	if (fitted.values) {

				pred         <- model$fitted.values
				resid        <- model$residuals ^ 2
				avg_resid    <- sum(resid) / length(pred)
				scaled_resid <- resid / avg_resid
				model1       <- lm(scaled_resid ~ pred)
				score        <- summary(model1)$r.squared * n
				np           <- 1
				p            <- pchisq(score, 1, lower.tail = F)
        preds        <- paste('fitted values of', resp)

    	} else {

				var_resid <- sum(residuals(model) ^ 2) / n
				ind       <- residuals(model) ^ 2 / var_resid - 1
				mdata     <- l[-1]
				dl        <- mdata[, vars]
				dk        <- as.data.frame(cbind(ind, dl))
				nd        <- ncol(dk) - 1
				model1    <- lm(ind ~ ., data = dk)
				score     <- summary(model1)$r.squared * n
				p         <- pchisq(score, nd, lower.tail = F)
				np        <- nd
        preds     <- vars

    	}

    }

    out <- list(score = round(score, 3),
    	          p     = round(p, 3),
    	          df    = np,
    					  fv    = fitted.values,
    					  rhs   = rhs,
    					  preds = preds,
    					  resp  = resp)

    class(out) <- 'score_test'

    return(out)


}

#' @export
#'
print.score_test <- function(x, ...) {
	print_score_test(x, ...)
}
