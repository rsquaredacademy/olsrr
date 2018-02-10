#' @importFrom stats pf
#' @title F Test for Constant Variance
#' @description Test for heteroskedasticity under the assumption that
#' the errors are independent and identically distributed (i.i.d.).
#' @param model an object of class \code{lm}
#' @param fitted_values logical; if TRUE, use fitted values of regression model
#' @param rhs logical; if TRUE, specifies that tests for heteroskedasticity be
#' performed for the right-hand-side (explanatory) variables of the fitted
#' regression model
#' @param vars variables to be used for for heteroskedasticity test
#' @param ... other arguments
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
#' @references Wooldridge, J. M. 2013. Introductory Econometrics: A Modern Approach. 5th ed. Mason, OH: South-Western.
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

  # l    <- model.frame(model)
  m1 <- tibble::as_data_frame(model.frame(model))
  m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
  l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
  nam <- names(l)[-1]
  resp <- names(l)[1]
  n <- nrow(l)

  if (rhs) {
    fitted_values <- FALSE
    k <- frhs(nam, model, n, l)
    f <- k[[1]]
    numdf <- k[[2]]
    dendf <- k[[3]]
    p <- pf(f, numdf, dendf, lower.tail = F)
  } else {
    if (fitted_values) {
      k <- ffit(model)
      f <- k[[1]]
      numdf <- k[[2]]
      dendf <- k[[3]]
      p <- pf(f, numdf, dendf, lower.tail = F)
    } else {
      k <- fvar(n, l, model, vars)
      f <- k[[1]]
      numdf <- k[[2]]
      dendf <- k[[3]]
      p <- pf(f, numdf, dendf, lower.tail = F)
    }
  }

  out <- list(
    f = f,
    p = p,
    numdf = numdf,
    dendf = dendf,
    fv = fitted_values,
    rhs = rhs,
    vars = vars,
    resp = resp,
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
