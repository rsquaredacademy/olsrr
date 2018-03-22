#' Akaike information criterion
#'
#' @description Akaike information criterion for model selection.
#'
#' @param model An object of class \code{lm}.
#'
#' @param method A character vector; specify the method to compute AIC. Valid
#'   options include R, STATA and SAS.
#'
#' @details
#' AIC provides a means for model selection. Given a collection of models for
#' the data, AIC estimates the quality of each model, relative to each of the
#' other models. R and STATA use loglikelihood to compute AIC. SAS uses residual
#' sum of squares. Below is the formula in each case:
#'
#' \emph{R & STATA}
#' \deqn{AIC = -2(loglikelihood) + 2p}
#'
#' \emph{SAS}
#' \deqn{AIC = n * ln(SSE / n) + 2p}
#'
#' where \emph{n} is the sample size and \emph{p} is the number of model parameters including intercept.
#'
#' @return Akaike information criterion of the model.
#'
#' @references
#' Akaike, H. (1969). “Fitting Autoregressive Models for Prediction.” Annals of the Institute of Statistical
#' Mathematics 21:243–247.
#'
#' Judge, G. G., Griffiths, W. E., Hill, R. C., and Lee, T.-C. (1980). The Theory and Practice of Econometrics.
#' New York: John Wiley & Sons.
#'
#' @examples
#' # using R computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_aic(model)
#'
#' # using STATA computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_aic(model, method = 'STATA')
#'
#' # using SAS computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_aic(model, method = 'SAS')
#'
#' @family model selection criteria
#'
#' @importFrom stats logLik
#'
#' @export
#'
ols_aic <- function(model, method = c("R", "STATA", "SAS")) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  method <- match.arg(method)
  n      <- model_rows(model)
  p      <- model_n_coeffs(model)

  if (method == "R") {

    lk <- logLik(model)
    -2 * lk[1] + 2 * (p + 1)

  } else if (method == "STATA") {

    lk <- logLik(model)
    -2 * lk[1] + 2 * p

  } else if (method == "SAS") {

    sse <- model_rss(model)
    n * log(sse / n) + 2 * p

  } else {

    message("Please specify a valid method.")

  }

}


#' Bayesian information criterion
#'
#' Bayesian information criterion for model selection.
#'
#' @param model An object of class \code{lm}.
#' @param method A character vector; specify the method to compute BIC. Valid
#'   options include R, STATA and SAS.
#'
#' @details
#' SBC provides a means for model selection. Given a collection of models for
#' the data, SBC estimates the quality of each model, relative to each of the
#' other models. R and STATA use loglikelihood to compute SBC. SAS uses residual
#' sum of squares. Below is the formula in each case:
#'
#' \emph{R & STATA}
#' \deqn{AIC = -2(loglikelihood) + ln(n) * 2p}
#'
#' \emph{SAS}
#' \deqn{AIC = n * ln(SSE / n) + p * ln(n)}
#'
#' where \emph{n} is the sample size and \emph{p} is the number of model parameters including intercept.
#'
#' @return The bayesian information criterion of the model.
#'
#' @references
#' Schwarz, G. (1978). “Estimating the Dimension of a Model.” Annals of Statistics 6:461–464.
#'
#' Judge, G. G., Griffiths, W. E., Hill, R. C., and Lee, T.-C. (1980). The Theory and Practice of Econometrics.
#' New York: John Wiley & Sons.
#'
#' @examples
#' # using R computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_sbc(model)
#'
#' # using STATA computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_sbc(model, method = 'STATA')
#'
#' # using SAS computation method
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_sbc(model, method = 'SAS')
#'
#' @family model selection criteria
#'
#' @export
#'
ols_sbc <- function(model, method = c("R", "STATA", "SAS")) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  method <- match.arg(method)
  n      <- model_rows(model)
  p      <- model_n_coeffs(model)

  if (method == "R") {

    lk <- logLik(model)
    -2 * lk[1] + log(n) * (p + 1)

  } else if (method == "STATA") {

    lk <- logLik(model)
    -2 * lk[1] + log(n) * p

  } else if (method == "SAS") {

    sse <- model_rss(model)
    n * log(sse / n) + p * log(n)

  } else {

    message("Please specify a valid method.")

  }
}



#' Sawa's bayesian information criterion
#'
#' @description Sawa's bayesian information criterion for model selection.
#'
#' @param model An object of class \code{lm}.
#' @param full_model An object of class \code{lm}.
#'
#' @details
#' Sawa (1978) developed a model selection criterion that was derived from a
#' Bayesian modification of the AIC criterion. Sawa's Bayesian Information
#' Criterion (BIC) is a function of the number of observations n, the SSE, the
#' pure error variance fitting the full model, and the number of independent
#' variables including the intercept.
#'
#' \deqn{SBIC = n * ln(SSE / n) + 2(p + 2)q - 2(q^2)}
#'
#' where \eqn{q = n(\sigma^2)/SSE}, \emph{n} is the sample size, \emph{p} is the number of model parameters including intercept
#' \emph{SSE} is the residual sum of squares.
#' @return Sawa's Bayesian Information Criterion
#'
#' @references
#' Sawa, T. (1978). “Information Criteria for Discriminating among Alternative Regression Models.” Econometrica
#' 46:1273–1282.
#'
#' Judge, G. G., Griffiths, W. E., Hill, R. C., and Lee, T.-C. (1980). The Theory and Practice of Econometrics.
#' New York: John Wiley & Sons.
#'
#' @examples
#' full_model <- lm(mpg ~ ., data = mtcars)
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_sbic(model, full_model)
#'
#' @family model selection criteria
#'
#' @export
#'
ols_sbic <- function(model, full_model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  if (!all(class(full_model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  n <- model_rows(model)
  p <- anova_coeffs(model)
  r <- full_model_coeffs(full_model)

  q <-
    q1(full_model, r) %>%
    divide_by(q2(model, p)) %>%
    multiply_by(n)

  sbicout(model, n, p, q)

}

#' Extract mean square
#'
#' Extracts the mean square from \code{anova()}.
#'
#' @param full_model An object of class \code{lm}.
#' @param r A vector of length 1.
#'
#' @keywords internal
#'
#' @noRd
#'
q1 <- function(full_model, r) {

  full_model %>%
    anova() %>%
    extract2(3) %>%
    extract(r)

}

#' Extract sum of squares
#'
#' Extracts the sum of squares from \code{anova()}.
#'
#' @param full_model An object of class \code{lm}.
#' @param p A vector of length 1.
#'
#' @keywords internal
#'
#' @noRd
#'
q2 <- function(model, p) {

  model %>%
    anova() %>%
    extract2(2) %>%
    extract(p)

}

#' SBIC internal
#'
#' Returns sawa's bayesian information criterion.
#'
#' @param model An object of class \code{lm}.
#' @param n A vector of length 1.
#' @param p A vector of length 1.
#' @param q A vector of length 1.
#'
#' @keywords internal
#'
#' @noRd
#'
sbicout <- function(model, n, p, q) {

  a <- (2 * (p + 2) * q)
  b <- (2 * (q ^ 2))

  model %>%
    model_rss() %>%
    divide_by(n) %>%
    log() %>%
    multiply_by(n) %>%
    add(a) %>%
    subtract(b)

}



#' Mallow's Cp
#'
#' @description Mallow's Cp.
#'
#' @param model An object of class \code{lm}.
#' @param fullmodel An object of class \code{lm}.
#'
#' @details
#' Mallows' Cp statistic estimates the size of the bias that is introduced into
#' the predicted responses by having an underspecified model. Use Mallows' Cp
#' to choose between multiple regression models. Look for models where
#' Mallows' Cp is small and close to the number of predictors in the model plus
#' the constant (p).
#'
#' @return Mallow's Cp of the model.
#'
#' @references
#' Hocking, R. R. (1976). “The Analysis and Selection of Variables in a Linear Regression.” Biometrics
#' 32:1–50.
#'
#' Mallows, C. L. (1973). “Some Comments on Cp.” Technometrics 15:661–675.
#'
#' @examples
#' full_model <- lm(mpg ~ ., data = mtcars)
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_mallows_cp(model, full_model)
#'
#' @family model selection criteria
#'
#' @export
#'
ols_mallows_cp <- function(model, fullmodel) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  if (!all(class(fullmodel) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  n <- model_rows(model)
  p <- anova_coeffs(model)
  q <- full_model_coeffs(fullmodel)

  mcpout(model, fullmodel, n, p, q)

}

#' Mallow's Cp internal
#'
#' Computes Mallow's Cp.
#'
#' @param model An object of class \code{lm}.
#' @param fullmodel An object of class \code{lm}.
#' @param n A numeric vector of length 1.
#' @param p A numeric vector of length 1.
#' @param q A numeric vector of length 1.
#'
#' @keywords internal
#'
#' @noRd
#'
mcpout <- function(model, fullmodel, n, p, q) {

  sse <- model_rss(model)
  sec <- (n - (2 * p))

  mse <-
    fullmodel %>%
    anova() %>%
    extract2(3) %>%
    rev() %>%
    extract(1)

  sse %>%
    divide_by(mse) %>%
    subtract(sec)

}



#' MSEP
#'
#' Estimated error of prediction, assuming multivariate normality.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' Computes the estimated mean square error of prediction assuming that both
#' independent and dependent variables are multivariate normal.
#'
#' \deqn{MSE(n + 1)(n - 2) / n(n - p - 1)}
#'
#' where \eqn{MSE = SSE / (n - p)}, n is the sample size and p is the number of
#' predictors including the intercept
#'
#' @return Estimated error of prediction of the model.
#'
#' @references
#' Stein, C. (1960). “Multiple Regression.” In Contributions to Probability and Statistics: Essays in Honor
#' of Harold Hotelling, edited by I. Olkin, S. G. Ghurye, W. Hoeffding, W. G. Madow, and H. B. Mann,
#' 264–305. Stanford, CA: Stanford University Press.
#'
#' Darlington, R. B. (1968). “Multiple Regression in Psychological Research and Practice.” Psychological
#' Bulletin 69:161–182.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_msep(model)
#'
#' @family model selection criteria
#'
#' @export
#'
ols_msep <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  sepout(model)

}

#' MSEP internal
#'
#' Computes the estimated mean square error of prediction.
#'
#' @param model An object of class \code{lm}.
#'
#' @keywords internal
#'
#' @noRd
#'
sepout <- function(model) {

  n <- model_rows(model)
  p <- anova_coeffs(model)

  mse <-
    model %>%
    anova() %>%
    extract2(3) %>%
    extract(p)

  num <-
    (n + 1) %>%
    multiply_by((n - 2)) %>%
    multiply_by(mse)

  den <- n * (n - p - 1)

  num / den

}

#' Final prediction error
#'
#' @description Estimated mean square error of prediction.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' Computes the estimated mean square error of prediction for each model
#' selected assuming that the values of the regressors are fixed and that the
#' model is correct.
#'
#' \deqn{MSE((n + p) / n)}
#'
#' where \eqn{MSE = SSE / (n - p)}, n is the sample size and p is the number of predictors including the intercept
#'
#' @return Final prediction error of the model.
#'
#' @references
#' Akaike, H. (1969). “Fitting Autoregressive Models for Prediction.” Annals of the Institute of Statistical
#' Mathematics 21:243–247.
#'
#' Judge, G. G., Griffiths, W. E., Hill, R. C., and Lee, T.-C. (1980). The Theory and Practice of Econometrics.
#' New York: John Wiley & Sons.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_fpe(model)
#'
#' @family model selection criteria
#'
#' @export
#'
ols_fpe <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  jpout(model)

}

#' Final prediction error internal
#'
#' Computes the final prediction error.
#'
#' @param model An object of class \code{lm}.
#'
#' @keywords internal
#'
#' @noRd
#'
jpout <- function(model) {

  n <- model_rows(model)
  p <- anova_coeffs(model)

  mse <-
    model %>%
    anova() %>%
    extract2(3) %>%
    extract(p)

  n %>%
    add(p) %>%
    divide_by(n) %>%
    multiply_by(mse)

}


#' @title Amemiya's prediction criterion
#'
#' @description Amemiya's prediction error.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' Amemiya's Prediction Criterion penalizes R-squared more heavily than does
#' adjusted R-squared for each addition degree of freedom used on the
#' right-hand-side of the equation.  The higher the better for this criterion.
#'
#' \deqn{((n + p) / (n - p))(1 - (R^2))}
#'
#' where \emph{n} is the sample size, \emph{p} is the number of predictors including the intercept and
#' \emph{R^2} is the coefficient of determination.
#'
#' @return Amemiya's prediction error of the model.
#'
#' @references
#' Amemiya, T. (1976). Selection of Regressors. Technical Report 225, Stanford University, Stanford, CA.
#'
#' Judge, G. G., Griffiths, W. E., Hill, R. C., and Lee, T.-C. (1980). The Theory and Practice of Econometrics.
#' New York: John Wiley & Sons.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_apc(model)
#'
#' @family model selection criteria
#'
#' @export
#'
ols_apc <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  pcout(model)

}

#' Amemiya internal
#'
#' Computes the Amemiya prediction error.
#'
#' @param model An object of class \code{lm}.
#'
#' @importFrom magrittr multiply_by
#'
#' @keywords internal
#'
#' @noRd
#'
pcout <- function(model) {

  n <- model_rows(model)
  p <- anova_coeffs(model)

  rse <-
    model %>%
    summary() %>%
    extract2(8)

  (n + p) %>%
    divide_by((n - p)) %>%
    multiply_by((1 - rse))

}


#' @title Hocking's Sp
#'
#' @description Average prediction mean squared error.
#'
#' @param model An object of class \code{lm}.
#'
#' @details Hocking's Sp criterion is an adjustment of the residual sum of
#' Squares. Minimize this criterion.
#'
#' \deqn{MSE / (n - p - 1)}
#'
#' where \eqn{MSE = SSE / (n - p)}, n is the sample size and p is the number of predictors including the intercept
#'
#' @return Hocking's Sp of the model.
#'
#' @references
#' Hocking, R. R. (1976). “The Analysis and Selection of Variables in a Linear Regression.” Biometrics
#' 32:1–50.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_hsp(model)
#'
#' @family model selection criteria
#'
#' @export
#'
ols_hsp <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  spout(model)

}

#' Hocking's internal
#'
#' Computes the Hocking's Sp statistic.
#'
#' @param model An object of class \code{lm}.
#'
#' @importFrom magrittr divide_by
#'
#' @keywords internal
#'
#' @noRd
#'
spout <- function(model) {

  n <- model_rows(model)
  p <- anova_coeffs(model)

  mse <-
    model %>%
    anova() %>%
    extract2(3) %>%
    extract(p)

  mse / (n - p - 1)

}

#' Model data rows
#'
#' Returns the number of rows in the data used in the model.
#'
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
model_rows <- function(model) {

  model %>%
    model.frame() %>%
    nrow()

}

#' Model Coefficients
#'
#' Returns the number of coefficients in the model.
#'
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
model_n_coeffs <- function(model) {

  model %>%
    use_series(coefficients) %>%
    length()

}

#' Residual sum of squares
#'
#' Returns the residual sum of squares.
#'
#' @importFrom magrittr raise_to_power
#'
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
model_rss <- function(model) {

  model %>%
    residuals() %>%
    raise_to_power(2) %>%
    sum()

}

#' Coefficients
#'
#' Returns the number of coefficients in the model
#'   using `anova()` including the residual sum of squares.
#'
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
anova_coeffs <- function(model) {

  model %>%
    anova() %>%
    extract2(1) %>%
    length()

}

#' Number of columns
#'
#' Returns the number of columns in the data.
#'
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
full_model_coeffs <- function(model) {

  model %>%
    model.frame() %>%
    length()

}

