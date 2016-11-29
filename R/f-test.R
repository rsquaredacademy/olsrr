f_test <- function(model, fitted.values = TRUE, rhs = FALSE, vars = NULL) UseMethod('f_test')

f_test.default <- function(model, fitted.values = TRUE, rhs = FALSE, vars = NULL) {

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


print.f_test <- function(data) {

	print_ftest(data)

}