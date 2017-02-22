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
#' @return Akaike Information Criteria
#' @examples 
#' # using R computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' aic(model)
#' 
#' # using STATA computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' aic(model, method = 'STATA')
#' 
#' # using SAS computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' aic(model, method = 'SAS')
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
#' @return Bayesian Information Criteria
#' @examples 
#' # using R computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' sbc(model)
#' 
#' # using STATA computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' sbc(model, method = 'STATA')
#' 
#' # using SAS computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' sbc(model, method = 'SAS')
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
#' @return Sawa's Bayesian Information Criteria
#' @examples
#' full_model <- lm(mpg ~ ., data = mtcars)
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' sbic(model, full_model)
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
	# p <- model %>% coefficients() %>% length()
	p <- model %>% anova() %>% `[[`(1) %>% length()
	r <- full_model %>% model.frame() %>% length()
	q <- n * (q1(full_model, r) / q2(model, p))
	result <- sbicout(model, n, p, q)

	return(result)

}


# mallow's cp
#' @title Mallow's Cp
#' @description Mallow's Cp
#' @param model an object of class \code{lm}
#' @param fullmodel an object of class \code{lm}
#' @return Mallow's Cp
#' @examples
#' full_model <- lm(mpg ~ ., data = mtcars)
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' mallow_cp(model, full_model)
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
		# p <- model %>% coefficients() %>% length()
		p <- model %>% anova() %>% `[[`(1) %>% length()
		q <- fullmodel %>% model.frame() %>% length()
	mcp <- mcpout(model, fullmodel, n, p, q)
	return(mcp)

}



# gmsep : estimated error of prediction, assuming multivariate normality
#' @title GMSEP
#' @description Estimated error of prediction, assuming multivariate normality
#' @param model an object of class \code{lm}
#' @return GMSEP
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' gmsep(model)
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
#' @return JP
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' jp(model)
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
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' pc(model)
#' @return PC
#' @export
#'
pc <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	return(pcout(model))

}

# # pc2: based on formula ((n + p) / (n - p)) * (SSE / n)
# pc2 <- function(model) {

# 	return(pc2out(model))

# }


# sp: average prediction mean squared error
#' @title SP
#' @description Average prediction mean squared error
#' @param model an object of class \code{lm}
#' @return SP
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' sp(model)
#' @export
#'
sp <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	return(spout(model))

}
