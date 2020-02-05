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
#' @section Deprecated Function:
#' \code{ols_score_test()} has been deprecated. Instead use \code{ols_test_score()}.
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

  resp <-
    model %>%
    model.frame() %>%
    names() %>%
    extract(1)

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

  out <- list(score = d$score,
              p     = d$p,
              df    = d$np,
              fv    = fitted_values,
              rhs   = rhs,
              preds = d$preds,
              resp  = resp
  )

  class(out) <- "ols_test_score"
  return(out)

}

#' @export
#'
print.ols_test_score <- function(x, ...) {
  print_score_test(x, ...)
}

rhsout <- function(model) {

  l <- ols_prep_avplot_data(model)
  n <- nrow(l)

  nam <-
    l %>%
    names() %>%
    extract(-1)

  np        <- length(nam)
  var_resid <- residual_var(model, n)
  ind       <- ind_score(model, var_resid)
  score     <- rhs_score(l, ind, n)
  p         <- pchisq(score, np, lower.tail = F)
  preds     <- nam

  list(score = score,
       preds = preds,
       np    = np,
       p     = p)

}

fitout <- function(model, resp) {

  score <- fit_score(model)
  np    <- 1
  p     <- pchisq(score, 1, lower.tail = F)
  preds <- paste("fitted values of", resp)

  list(score = score,
       preds = preds,
       np    = np,
       p     = p)

}

varout <- function(model, vars) {

  score <- var_score(model, vars)

  nd <-
    score_data(model, vars) %>%
    ncol() %>%
    subtract(1)

  p     <- pchisq(score, nd, lower.tail = F)
  np    <- nd
  preds <- vars

  list(score = score,
       preds = preds,
       np    = np,
       p     = p)

}

residual_var <- function(model, n) {

  model %>%
    residuals() %>%
    raise_to_power(2) %>%
    sum() %>%
    divide_by(n)

}

ind_score <- function(model, var_resid) {

  vresid <- var_resid - 1

  model %>%
    residuals() %>%
    raise_to_power(2) %>%
    divide_by(vresid)

}

rhs_score <- function(l, ind, n) {

  r.squared <- NULL

  ind <- data.frame(ind = ind)

  cbind(l, ind) %>%
    select(-1) %>%
    lm(ind ~ ., data = .) %>%
    summary() %>%
    use_series(r.squared) %>%
    multiply_by(n)

}

fit_score <- function(model) {

  r.squared <- NULL

  pred         <- fitted(model)
  scaled_resid <- resid_scaled(model, pred)
  l            <- ols_prep_avplot_data(model)
  n            <- nrow(l)

  lm(scaled_resid ~ pred) %>%
    summary() %>%
    use_series(r.squared) %>%
    multiply_by(n)

}

resid_scaled <- function(model, pred) {

  resid <-
    model %>%
    residuals() %>%
    raise_to_power(2)

  avg_resid <- sum(resid) / length(pred)

  resid / avg_resid

}

var_score <- function(model, vars) {

  r.squared <- NULL

  n <-
    model %>%
    ols_prep_avplot_data() %>%
    nrow()

  score_data(model, vars) %>%
    lm(ind ~ ., data = .) %>%
    summary() %>%
    use_series(r.squared) %>%
    multiply_by(n)

}

score_data <- function(model, vars) {

  l         <- ols_prep_avplot_data(model)
  n         <- nrow(l)
  var_resid <- residual_var(model, n)

  ind <-
    ind_score(model, var_resid) %>%
    as.data.frame() %>%
    set_colnames("ind")

  l %>%
    select(!!! syms(vars)) %>%
    cbind(ind)

}


#' @export
#' @rdname ols_test_score
#' @usage NULL
#'
ols_score_test <- function(model, fitted_values = TRUE, rhs = FALSE,
                           vars = NULL) {
  .Deprecated("ols_test_score()")
}
