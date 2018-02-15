#' @importFrom stats cor
#' @title Part and Partial Correlations
#' @description Zero-order, part and partial correlations
#' @param model an object of class \code{lm}
#' @details \code{correlations} returns the relative importance of independent variables in determining response variable.
#' How much each variable uniquely contributes to rsquare over and above that which can be accounted for by the other predictors?
#' Zero order correlation is the Pearson correlation coefficient between the dependent variable and the
#' independent variables. Part correlations indicates how much rsquare will decrease if that variable is removed from the model
#' and partial correlations indicates amount of variance in response variable, which is not estimated by the other
#' independent variables in the model, but is estimated by the specific variable.
#'
#' @return \code{ols_correlations} returns an object of class \code{"ols_correlations"}.
#' An object of class \code{"ols_correlations"} is a data frame containing the
#' following components:
#'
#' \item{Zero-order}{zero order correlations}
#' \item{Partial}{partial correlations}
#' \item{Part}{part correlations}
#'
#' @references
#' Morrison, D. F. 1976. Multivariate statistical methods. New York: McGraw-Hill.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_correlations(model)
#' @export
#'
ols_correlations <- function(model) UseMethod("ols_correlations")

#' @export
#'
ols_correlations.default <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  result <- corout(model, corm2(model))
  class(result) <- c("ols_correlations", "data.frame")
  return(result)
}

#' @export
#'
print.ols_correlations <- function(x, ...) {
  print_correlations(x)
}

#' @importFrom tibble as_data_frame
#' @importFrom purrr map_df
cordata <- function(model) {
  m1 <- tibble::as_data_frame(model.frame(model))
  m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
  l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
  # d <- model %>%
  #   model.frame() %>%
  #   as_data_frame() %>%
  #   map_df(as.numeric)
  return(l)
}

cmdata <- function(mdata) {
  d <- mdata %>%
    cor() %>%
    `[`(-1, 1)
  return(d)
}

rtwo <- function(i, mdata) {
  dat <- mdata[, c(-1, -i)]
  out <- lm(mdata[[1]] ~ ., data = dat) %>%
    summary() %>%
    `[[`(8)
  return(out)
}

corsign <- function(model) {
  d <- model %>%
    summary() %>%
    use_series(coefficients) %>%
    `[`(, 1) %>%
    `[`(-1) %>%
    sign()
  return(d)
}

corout <- function(model, r2) {
  mdata <- cordata(model)
  cor_mdata <- cmdata(mdata)
  r1 <- summary(model)$r.squared
  n <- ncol(mdata)
  ksign <- corsign(model)
  n2 <- n - 1
  parts <- ksign * sqrt(r1 - r2)
  partials <- parts / sqrt(1 - r2)
  result <- data.frame(cor_mdata, partials, parts)
  rownames(result) <- names(ksign)
  colnames(result) <- c("Zero-order", "Partial", "Part")

  return(result)
}

corm2 <- function(model) {
  mdata <- cordata(model)
  n <- ncol(mdata)
  r2 <- c()

  for (i in 2:n) {
    out <- rtwo(i, mdata)
    r2 <- c(r2, out)
  }

  return(r2)
}
