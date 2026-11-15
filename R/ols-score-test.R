#' Score test
#'
#' @description
#' Test for heteroskedasticity under the assumption that the errors are
#' independent and identically distributed (i.i.d.).
#'
#' @param model An object of class \code{lm}.
#' @param fitted_values Logical; if TRUE, use fitted values of regression model.
#' @param rhs Logical; if TRUE, specifies that tests for heteroskedasticity be
#'   performed for the right-hand-side (explanatory) variables of the fitted
#'   regression model.
#' @param vars Variables to be used for for heteroskedasticity test.
#'
#' @return \code{ols_test_score} returns an object of class \code{"ols_test_score"}.
#' An object of class \code{"ols_test_score"} is a list containing the
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
#' @details The test is not available for weighted models, because it refits
#' auxiliary regressions that would ignore the model weights.
#'
#' A constant or fully aliased regressor supplies no direction to test beyond
#' the auxiliary intercept, so the statistic has zero degrees of freedom and is
#' undefined; the function then stops with an informative error instead of
#' reporting a degenerate p value.
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#'
#' # using fitted values of the model
#' ols_test_score(model)
#'
#' # using predictors from the model
#' ols_test_score(model, rhs = TRUE)
#'
#' # specify predictors from the model
#' ols_test_score(model, vars = c('disp', 'wt'))
#'
#' @family heteroskedasticity tests
#'
#' @export
#'
ols_test_score <- function(model, fitted_values = TRUE, rhs = FALSE,
                           vars = NULL) UseMethod("ols_test_score")

#' @export
#'
ols_test_score.default <- function(model, fitted_values = TRUE, rhs = FALSE,
                                   vars = NULL) {

  check_model(model)
  check_weights_diagnostic(model, "ols_test_score")
  check_logic(fitted_values)
  check_logic(rhs)

  if (length(vars) > 0) {
    check_modelvars(model, vars)
    vars          <- unique(vars)
    fitted_values <- FALSE
  }

  resp <- names(model.frame(model))[1]

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

  out <- list(df = d$np, fv = fitted_values, p = d$p, preds = d$preds,
              resp = resp, rhs = rhs, score = d$score)

  class(out) <- "ols_test_score"
  return(out)

}

#' @export
#'
print.ols_test_score <- function(x, ...) {
  print_score_test(x, ...)
}

rhsout <- function(model) {

  l         <- ols_prep_avplot_data(model)
  nam       <- names(l)[-1]
  var_resid <- residual_var(model, nrow(l))
  ind       <- ind_score(model, var_resid)
  aux       <- rhs_score(l, ind, nrow(l))
  p         <- pchisq(aux$score, aux$df, lower.tail = F)

  list(score = aux$score, preds = nam, np = aux$df, p = p)

}

fitout <- function(model, resp) {

  score <- fit_score(model)
  p     <- pchisq(score, 1, lower.tail = F)

  list(score = score, preds = paste("fitted values of", resp), np = 1, p = p)

}

varout <- function(model, vars) {

  aux <- var_score(model, vars)
  p   <- pchisq(aux$score, aux$df, lower.tail = F)

  list(score = aux$score, preds = vars, np = aux$df, p = p)

}

residual_var <- function(model, n) {
  sum(model_residuals_used(model) ^ 2) / n
}

ind_score <- function(model, var_resid) {

  vresid <- var_resid - 1
  (model_residuals_used(model) ^ 2) / vresid

}

rhs_score <- function(l, ind, n) {

  ind <- data.frame(ind = ind)
  fit <- lm(ind ~ ., data = cbind(l, ind)[, -1])

  # tested directions: auxiliary columns independent of the intercept, so that
  # aliased predictors do not inflate the degrees of freedom
  check_aux_rank(fit$rank, "ols_test_score")

  list(score = summary(fit)$r.squared * n, df = fit$rank - 1L)

}

fit_score <- function(model) {

  pred         <- model_fitted_used(model)
  scaled_resid <- resid_scaled(model, pred)
  l            <- ols_prep_avplot_data(model)
  n            <- nrow(l)
  fit          <- lm(scaled_resid ~ pred)

  # a constant `pred` (for example an intercept-only model) leaves no tested
  # direction beyond the intercept
  check_aux_rank(fit$rank, "ols_test_score")

  (summary(fit)$r.squared) * n

}

resid_scaled <- function(model, pred) {

  resid     <- model_residuals_used(model) ^ 2
  avg_resid <- sum(resid) / length(pred)
  resid / avg_resid

}

var_score <- function(model, vars) {

  n   <- nrow(ols_prep_avplot_data(model))
  fit <- lm(ind ~ ., data = score_data(model, vars))

  check_aux_rank(fit$rank, "ols_test_score")

  list(score = summary(fit)$r.squared * n, df = fit$rank - 1L)

}

score_data <- function(model, vars) {

  l              <- ols_prep_avplot_data(model)
  var_resid      <- residual_var(model, nrow(l))
  ind            <- as.data.frame(ind_score(model, var_resid)) 
  colnames(ind)  <- c("ind")

  cbind(l[, vars_columns(model, vars), drop = FALSE], ind)
}