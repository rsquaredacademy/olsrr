#' F test for constant variance
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
#' @param ... Other arguments.
#'
#' @return \code{ols_f_test} returns an object of class \code{"ols_f_test"}.
#' An object of class \code{"ols_f_test"} is a list containing the
#' following components:
#'
#' \item{f}{f statistic}
#' \item{p}{p-value of \code{f}}
#' \item{fv}{fitted values of the regression model}
#' \item{rhs}{names of explanatory variables of fitted regression model}
#' \item{numdf}{numerator degrees of freedom}
#' \item{dendf}{denominator degrees of freedom}
#' \item{vars}{variables to be used for heteroskedasticity test}
#' \item{resp}{response variable}
#' \item{preds}{predictors}
#'
#' @references
#' Wooldridge, J. M. 2013. Introductory Econometrics: A Modern Approach. 5th ed. Mason, OH: South-Western.
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#'
#' # using fitted values
#' ols_f_test(model)
#'
#' # using all predictors of the model
#' ols_f_test(model, rhs = TRUE)
#'
#' # using fitted values
#' ols_f_test(model, vars = c('disp', 'hp'))
#'
#' @family heteroskedasticity tests
#'
#' @importFrom stats pf
#'
#' @export
#'
ols_f_test <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL, ...) UseMethod("ols_f_test")

#' @export
#'
ols_f_test.default <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL, ...) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  if (!is.logical(fitted_values)) {
    stop("fitted.values must be either TRUE or FALSE")
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

  l <- avplots_data(model)

  nam <-
    l %>%
    names() %>%
    extract(-1)

  resp <-
    l %>%
    names() %>%
    extract(1)

  n <- nrow(l)

  if (rhs) {
    fitted_values <- FALSE
    k             <- frhs(nam, model, n, l)
    result        <- ftest_result(k)
  } else {
    if (fitted_values) {
      k      <- ffit(model)
      result <- ftest_result(k)
    } else {
      k      <- fvar(n, l, model, vars)
      result <- ftest_result(k)
    }
  }

  out <- list(
    f     = result$f,
    p     = result$p,
    numdf = result$numdf,
    dendf = result$dendf,
    fv    = fitted_values,
    rhs   = rhs,
    vars  = vars,
    resp  = resp,
    preds = nam
  )

  class(out) <- "ols_f_test"

  return(out)
}

#' @export
#'
print.ols_f_test <- function(x, ...) {
  print_ftest(x)
}

frhs <- function(nam, model, n, l) {

  fstatistic <- NULL

  np <- length(nam)

  var_resid <-
    model_rss(model) %>%
    divide_by(n) %>%
    subtract(1)

  ind <- model %>%
    residuals() %>%
    raise_to_power(2) %>%
    divide_by(var_resid)

  l     <- cbind(l, ind)
  mdata <- l[-1]

  lm(ind ~ ., data = mdata) %>%
    summary() %>%
    use_series(fstatistic)

}

fvar <- function(n, l, model, vars) {

  fstatistic <- NULL

  var_resid <-
    model_rss(model) %>%
    divide_by(n) %>%
    subtract(1)

  ind <- model %>%
    residuals() %>%
    raise_to_power(2) %>%
    divide_by(var_resid)

  mdata <- l[-1]
  dl    <- mdata[, vars]
  dk    <- as.data.frame(cbind(ind, dl))

  lm(ind ~ ., data = dk) %>%
    summary() %>%
    use_series(fstatistic)

}

ffit <- function(model) {

  fstatistic <- NULL

  pred     <- fitted(model)
  pred_len <- length(pred)

  resid <-
    model %>%
    use_series(residuals) %>%
    raise_to_power(2)

  avg_resid <-
    resid %>%
    sum() %>%
    divide_by(pred_len)

  scaled_resid <- resid / avg_resid

  lm(scaled_resid ~ pred) %>%
    summary() %>%
    use_series(fstatistic)

}

ftest_result <- function(k) {

  f     <- k[[1]]
  numdf <- k[[2]]
  dendf <- k[[3]]
  p     <- pf(f, numdf, dendf, lower.tail = F)

  list(f = f, numdf = numdf, dendf = dendf, p = p)

}
