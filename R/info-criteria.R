# information criteria for model selection
# 1. AIC: Akaike Information Criteria
# 2. SBIC: Sawa Bayesian Information Criteria
# 3. SBC: Schwarz Bayes Criteria
# 4. Mallow's Cp
# 5. PC: Amemiya Prediction Criteria
# 6. GMSEP: Estimated MSE of prediction, assuming multivariate normality
# 7. Jp: Final prediction error
# 8. Sp: Hocking 1976


# AIC: Akaike Information Criteria
aic <- function(model, method = c("R", "STATA", "SAS")) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	method <- match.arg(method)
	n      <- nrow(model.frame(model))
	p      <- length(model$coefficients) 

	if (method == "R") {

		p      <- p + 1
		lk     <- logLik(model)
		akaike <- -2 * lk[1] + 2 * p 		

	} else if (method == "STATA") {

		lk     <- logLik(model)
		akaike <- -2 * lk[1] + 2 * p 

	} else if (method == 'SAS') {

		sse    <- sum(residuals(model) ^ 2)
		akaike <- n * log(sse / n) + 2 * p

	} else {

		message("Please specify a valid method.")

	}
    
	akaike <- round(akaike, 4)
	return(akaike)
	
}


# bayesian information criterion (sbc in sas)
sbc <- function(model, method = c("R", "STATA", "SAS")) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	n      <- nrow(model.frame(model))
	p      <- length(model$coefficients) 
	method <- match.arg(method)

	if (method == "R") {

		p        <- p + 1
		lk       <- logLik(model)
		bayesian <- -2 * lk[1] + log(n) * p

	} else if (method == "STATA") {
  	
		lk       <- logLik(model)
		bayesian <- -2 * lk[1] + log(n) * p

	} else if (method == 'SAS') {

		sse      <- sum(residuals(model) ^ 2)
		bayesian <- n * log(sse / n) + p * log(n)

	} else {

		message("Please specify a valid method.")

	}
	
	bayesian <- round(bayesian, 4)
	return(bayesian)
}


# sawa's bayesian information criteria
sbic <- function(model, full_model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

  if (!all(class(fullmodel) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

  # if (!all(names(model$coefficients) %in% names(fullmodel$coefficients))) {
  # 	stop('model must be a subset of full model')
  # } 

	n   <- nrow(model.frame(model))
	p   <- length(model$coefficients)
	r   <- length(full_model$coefficients)
	q   <- n * (anova(fullmodel)$`Mean Sq`[r] / anova(model)$`Sum Sq`[p])
	sbc <- n * log(sum(residuals(model) ^ 2) / n) +
	     (2 * (p + 2) * q) - (2 * (q ^ 2))
	
	return(sbc)

}


# mallow's cp
mallow_cp <- function(model, fullmodel) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

  if (!all(class(fullmodel) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

  if (!all(names(model$coefficients) %in% names(fullmodel$coefficients))) {
  	stop('model must be a subset of full model')
  }

	n   <- nrow(model.frame(model))
	p   <- length(model$coefficients) 
	q   <- length(fullmodel$coefficients) 
	sse <- sum(residuals(model) ^ 2)
	mse <- anova(fullmodel)$`Mean Sq`[q]
	mcp <- round((sse / mse) - (n - (2 * p)), 4)

	return(mcp)

}



# gmsep : estimated error of prediction, assuming multivariate normality
gmsep <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	n      <- nrow(model.frame(model))
	p      <- length(model$coefficients)
	mse 	 <- anova(model)$`Mean Sq`[p]
	result <- round((mse * (n + 1) * (n - 2)) / (n * (n - p -1)), 5)
	
	return(result)

}

# jp: final prediction error
jp <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	n <- nrow(model.frame(model))
	p <- length(model$coefficients)
	mse <- anova(model)$`Mean Sq`[p]
	fpe <- round(((n + p) / n) * mse, 5)

	return(fpe)

}

# pc: amemiya's prediction error
pc <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	n   <- nrow(model.frame(model))
	p   <- length(model$coefficients)
	rsq <- summary(model)$r.square
	ape <- round(((n + p) / (n - p)) * (1 - rsq), 5)

	return(ape)

}

# pc2: based on formula ((n + p) / (n - p)) * (SSE / n)
pc2 <- function(model) {

	# n: number of observations
	# p: number of predictors including intercept
	n <- nrow(model.frame(model))
	p <- length(model$coefficients)

	# pc = ((n + p) / (n - p)) * (SSE / n)
	sse <- sum(residuals(model) ^ 2)
	sst <- sum(anova(model)$`Sum Sq`)
	ape <- ((n + p) / (n * (n - p))) * (sse / sst)
	
	ape <- round(ape, 5)
	return(ape)

}


# sp: average prediction mean squared error
sp <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	n     <- nrow(model.frame(model))
	p     <- length(model$coefficients)
	mse 	<- anova(model)$`Mean Sq`[p] 
	apmse <- round(mse / (n - p - 1), 5)
	
	return(apmse)
	
}

