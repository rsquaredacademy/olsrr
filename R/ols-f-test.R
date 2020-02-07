#' F test
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
#' @return \code{ols_test_f} returns an object of class \code{"ols_test_f"}.
#' An object of class \code{"ols_test_f"} is a list containing the
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
#' @section Deprecated Function:
#' \code{ols_f_test()} has been deprecated. Instead use \code{ols_test_f()}.
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#'
#' # using fitted values
#' ols_test_f(model)
#'
#' # using all predictors of the model
#' ols_test_f(model, rhs = TRUE)
#'
#' # using fitted values
#' ols_test_f(model, vars = c('disp', 'hp'))
#'
#' @family heteroskedasticity tests
#'
#' @importFrom stats pf
#'
#' @export
#'
ols_test_f <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL, ...) UseMethod("ols_test_f")

#' @export
#'
ols_test_f.default <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL, ...) {

  check_model(model)
  check_logic(fitted_values)
  check_logic(rhs)

  if (length(vars) > 0) {
    check_modelvars(model, vars)
    fitted_values <- FALSE
  }

  l    <- ols_prep_avplot_data(model)
  nam  <- names(l)[-1]
  resp <- names(l)[1]
  n    <- nrow(l)

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

  class(out) <- "ols_test_f"

  return(out)
}

#' @export
#' @rdname ols_test_f
#' @usage NULL
#'
ols_f_test <- function(model, fitted_values = TRUE, rhs = FALSE, vars = NULL, ...) {
  .Deprecated("ols_test_f()")
}

#' @export
#'
print.ols_test_f <- function(x, ...) {
  print_ftest(x)
}

frhs <- function(nam, model, n, l) {

  fstatistic <- NULL
  np         <- length(nam)
  var_resid  <- (model_rss(model) / n) - 1
  ind        <- (residuals(model) ^ 2) / var_resid
  l          <- cbind(l, ind)
  mdata      <- l[-1]

  summary(lm(ind ~ ., data = mdata))$fstatistic 

}

fvar <- function(n, l, model, vars) {

  fstatistic <- NULL
  var_resid  <- (model_rss(model) / n) - 1
  ind        <- (residuals(model) ^ 2) / var_resid
  mdata      <- l[-1]
  dl         <- mdata[, vars]
  dk         <- as.data.frame(cbind(ind, dl))

  summary(lm(ind ~ ., data = dk))$fstatistic

}

ffit <- function(model) {

  fstatistic   <- NULL
  pred         <- fitted(model)
  pred_len     <- length(pred)
  resid        <- model$residuals ^ 2
  avg_resid    <- sum(resid) / pred_len
  scaled_resid <- resid / avg_resid

  summary(lm(scaled_resid ~ pred))$fstatistic

}

ftest_result <- function(k) {

  f     <- k[[1]]
  numdf <- k[[2]]
  dendf <- k[[3]]
  p     <- pf(f, numdf, dendf, lower.tail = F)

  list(f = f, numdf = numdf, dendf = dendf, p = p)

}
