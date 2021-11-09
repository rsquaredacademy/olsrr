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
  check_logic(fitted_values)
  check_logic(rhs)

  if (length(vars) > 0) {
    check_modelvars(model, vars)
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

  out <- list(df    = d$np,
              fv    = fitted_values,
              p     = d$p,
              preds = d$preds,
              resp  = resp,
              rhs   = rhs,
              score = d$score)

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
  score     <- rhs_score(l, ind, nrow(l))
  p         <- pchisq(score, length(nam), lower.tail = F)

  list(score = score,
       preds = nam,
       np    = length(nam),
       p     = p)

}

fitout <- function(model, resp) {

  score <- fit_score(model)
  p     <- pchisq(score, 1, lower.tail = F)

  list(score = score,
       preds = paste("fitted values of", resp),
       np    = 1,
       p     = p)

}

varout <- function(model, vars) {

  score <- var_score(model, vars)
  nd    <- ncol(score_data(model, vars)) - 1
  p     <- pchisq(score, nd, lower.tail = F)

  list(score = score,
       preds = vars,
       np    = nd,
       p     = p)

}

residual_var <- function(model, n) {
  sum(residuals(model) ^ 2) / n
}

ind_score <- function(model, var_resid) {

  vresid <- var_resid - 1
  (residuals(model) ^ 2) / vresid

}

rhs_score <- function(l, ind, n) {

  r.squared <- NULL
  ind <- data.frame(ind = ind)
  (summary(lm(ind ~ ., data = cbind(l, ind)[, -1]))$r.squared) * n

}

fit_score <- function(model) {

  r.squared    <- NULL
  pred         <- fitted(model)
  scaled_resid <- resid_scaled(model, pred)
  l            <- ols_prep_avplot_data(model)
  n            <- nrow(l)

  (summary(lm(scaled_resid ~ pred))$r.squared) * n

}

resid_scaled <- function(model, pred) {

  resid     <- residuals(model) ^ 2
  avg_resid <- sum(resid) / length(pred)
  resid / avg_resid

}

var_score <- function(model, vars) {

  r.squared <- NULL
  n <- nrow(ols_prep_avplot_data(model)) 
  (summary(lm(ind ~ ., data = score_data(model, vars)))$r.squared) * n
    
}

score_data <- function(model, vars) {

  l              <- ols_prep_avplot_data(model)
  var_resid      <- residual_var(model, nrow(l))
  ind            <- as.data.frame(ind_score(model, var_resid)) 
  colnames(ind)  <- c("ind")

  cbind(l[, vars], ind)
}