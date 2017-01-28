# information criteria for model selection
# 1. AIC: Akaike Information Criteria
# 2. SBIC: Sawa Bayesian Information Criteria
# 3. SBC: Schwarz Bayes Criteria
# 4. Mallow's Cp
# 5. PC: Amemiya Prediction Criteria
# 6. GMSEP: Estimated MSE of prediction, assuming multivariate normality
# 7. Jp: Final prediction error
# 8. Sp: Hocking 1976


#' @importFrom stats logLik
#' @title AIC
#' @description Akaike Information Criteria
#' @param model an object of class \code{lm}
#' @param method a character vector; specify the method to compute AIC. Valid
#' options include R, STATA and SAS
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return Akaike Information Criteria
#' @export
#'
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
#' @title SBC
#' @description Bayesian Information Criteria
#' @param model an object of class \code{lm}
#' @param method a character vector; specify the method to compute AIC. Valid
#' options include R, STATA and SAS
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return Bayesian Information Criteria
#' @export
#'
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
#' @title SBIC
#' @description Sawa's Bayesian Information Criteria
#' @param model an object of class \code{lm}
#' @param full_model an object of class \code{lm}
#' options include R, STATA and SAS
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return Sawa's Bayesian Information Criteria
#' @export
#'
sbic <- function(model, full_model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

  if (!all(class(full_model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

  # if (!all(names(model$coefficients) %in% names(fullmodel$coefficients))) {
  # 	stop('model must be a subset of full model')
  # }

	n <- model %>% model.frame() %>% nrow()
	p <- model %>% coefficients() %>% length()
	r <- full_model %>% coefficients() %>% length()
	q <- n * (q1(full_model, r) / q2(model, p))
	result <- sbicout(model, n, p, q)

	return(result)

}


# mallow's cp
#' @title Mallow's Cp
#' @description Mallow's Cp
#' @param model an object of class \code{lm}
#' @param fullmodel an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return Mallow's Cp
#' @export
#'
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

		n <- model %>% model.frame() %>% nrow()
		p <- model %>% coefficients() %>% length()
		q <- fullmodel %>% model.frame() %>% length()
	mcp <- mcpout(model, fullmodel, n, p, q)
	return(mcp)

}



# gmsep : estimated error of prediction, assuming multivariate normality
#' @title GMSEP
#' @description Estimated error of prediction, assuming multivariate normality
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return GMSEP
#' @export
#'
gmsep <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	result <- sepout(model)
	return(result)
}

# jp: final prediction error
#' @title JP
#' @description Final prediction error
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return JP
#' @export
#'
jp <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	return(jpout(model))

}

# pc: amemiya's prediction error
#' @title PC
#' @description Amemiya's prediction error
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return PC
#' @export
#'
pc <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	return(pcout(model))

}

# pc2: based on formula ((n + p) / (n - p)) * (SSE / n)
pc2 <- function(model) {

	return(pc2out(model))

}


# sp: average prediction mean squared error
#' @title SP
#' @description Average prediction mean squared error
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return SP
#' @export
#'
sp <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	return(spout(model))

}
