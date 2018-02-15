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
#'
#' @references
#' Breusch, T. S. and Pagan, A. R. (1979) A simple test for heteroscedasticity and random coefficient variation. Econometrica 47, 1287–1294.
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
#'
#' @export
#'
ols_score_test <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL) UseMethod("ols_score_test")

#' @export
#'
ols_score_test.default <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS regression model.", call. = FALSE)
  }

  if (!is.logical(fitted_values)) {
    stop("fitted_values must be either TRUE or FALSE")
  }

  if (!is.logical(rhs)) {
    stop("rhs must be either TRUE or FALSE")
  }

  if (length(vars) > 0) {
    if (!all(vars %in% names(model$coefficients))) {
      stop("vars must be a subset of the predictors in the model")
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

  out <- list(
    score = d$score,
    p = d$p,
    df = d$np,
    fv = fitted_values,
    rhs = rhs,
    preds = d$preds,
    resp = resp
  )

  class(out) <- "ols_score_test"
  return(out)
}

#' @export
#'
print.ols_score_test <- function(x, ...) {
  print_score_test(x, ...)
}

rhsout <- function(model) {
  # l <- model.frame(model)
  m1 <- tibble::as_data_frame(model.frame(model))
  m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
  l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
  n <- nrow(l)
  nam <- names(l)[-1]
  np <- length(nam)
  var_resid <- sum(residuals(model) ^ 2) / n
  ind <- residuals(model) ^ 2 / var_resid - 1
  l <- cbind(l, ind)
  mdata <- l[-1]
  model1 <- lm(ind ~ ., data = mdata)
  score <- summary(model1)$r.squared * n
  p <- pchisq(score, np, lower.tail = F)
  preds <- nam
  result <- list(score = score, p = p, np = np, preds = preds)
  return(result)
}

fitout <- function(model, resp) {

  # l <- model.frame(model)
  m1 <- tibble::as_data_frame(model.frame(model))
  m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
  l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
  n <- nrow(l)
  pred <- model$fitted.values
  resid <- model$residuals ^ 2
  avg_resid <- sum(resid) / length(pred)
  scaled_resid <- resid / avg_resid
  model1 <- lm(scaled_resid ~ pred)
  score <- summary(model1)$r.squared * n
  np <- 1
  p <- pchisq(score, 1, lower.tail = F)
  preds <- paste("fitted values of", resp)
  result <- list(score = score, p = p, np = np, preds = preds)
  return(result)
}

varout <- function(model, vars) {
  # l <- model.frame(model)
  m1 <- tibble::as_data_frame(model.frame(model))
  m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
  l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
  n <- nrow(l)
  var_resid <- sum(residuals(model) ^ 2) / n
  ind <- residuals(model) ^ 2 / var_resid - 1
  mdata <- l[-1]
  dl <- mdata[, vars]
  dk <- as.data.frame(cbind(ind, dl))
  nd <- ncol(dk) - 1
  model1 <- lm(ind ~ ., data = dk)
  score <- summary(model1)$r.squared * n
  p <- pchisq(score, nd, lower.tail = F)
  np <- nd
  preds <- vars
  result <- list(score = score, p = p, np = np, preds = preds)
  return(result)
}
