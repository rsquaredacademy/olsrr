#' @importFrom stats pf
#' @title f Test for heteroskedasticity
#' @description Test for heteroskedasticity
#' @param model an object of class \code{lm}
#' @param fitted.values logical; if TRUE, use fitted values of regression model
#' @param rhs logical; if TRUE, specifies that tests for heteroskedasticity be
#' performed for the right-hand-side (explanatory) variables of the fitted
#' regression model
#' @param vars variables to be used for for heteroskedasticity test
#' @param ... other arguments
#' @return \code{f_test} returns an object of class \code{"f_test"}.
#' An object of class \code{"f_test"} is a list containing the
#' following components:
#'
#' \item{f}{f statistic}
#' \item{p}{p-value of \code{f}}
#' \item{fv}{fitted values of the regression model}
#' \item{rhs}{name of explanatory variables of fitted regression model}
#' \item{numdf}{numerator degrees of freedom}
#' \item{dendf}{denominator degrees of freedom}
#' \item{vars}{variables to be used for heteroskedasticity test}
#' \item{resp}{response variable}
#' \item{preds}{predictors}
#' @export
#'
f_test <- function(model, fitted.values = TRUE, rhs = FALSE, vars = NULL, ...) UseMethod('f_test')

f_test.default <- function(model, fitted.values = TRUE, rhs = FALSE, vars = NULL, ...) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
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
    nam  <- names(l)[-1]
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
			k             <- summary(model1)
			f             <- as.vector(k$fstatistic[1])
			numdf         <- as.vector(k$fstatistic[2])
			dendf         <- as.vector(k$fstatistic[3])
			p             <- pf(f, numdf, dendf, lower.tail = F)

    } else {

    	if (fitted.values) {

				pred         <- model$fitted.values
				resid        <- model$residuals ^ 2
				avg_resid    <- sum(resid) / length(pred)
				scaled_resid <- resid / avg_resid
				model1       <- lm(scaled_resid ~ pred)
				k            <- summary(model1)
				f            <- as.vector(k$fstatistic[1])
				numdf        <- as.vector(k$fstatistic[2])
				dendf        <- as.vector(k$fstatistic[3])
				p            <- pf(f, numdf, dendf, lower.tail = F)


    	} else {

				var_resid <- sum(residuals(model) ^ 2) / n
				ind       <- residuals(model) ^ 2 / var_resid - 1
				mdata     <- l[-1]
				dl        <- mdata[, vars]
				dk        <- as.data.frame(cbind(ind, dl))
				nd        <- ncol(dk) - 1
				model1    <- lm(ind ~ ., data = dk)
				k         <- summary(model1)
				f         <- as.vector(k$fstatistic[1])
				numdf     <- as.vector(k$fstatistic[2])
				dendf     <- as.vector(k$fstatistic[3])
				p         <- pf(f, numdf, dendf, lower.tail = F)

    	}

    }

    out <- list(f     = round(f, 3),
    	          p     = round(p, 3),
    	          numdf = numdf,
    	          dendf = dendf,
    	          fv    = fitted.values,
    	          rhs   = rhs,
    	          vars  = vars,
    	          resp  = resp,
    	          preds = nam)

    class(out) <- 'f_test'

    return(out)

}

#' @export
#'
print.f_test <- function(x, ...) {
	print_ftest(x)
}
