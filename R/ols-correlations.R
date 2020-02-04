#' Part and partial correlations
#'
#' Zero-order, part and partial correlations.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' \code{ols_correlations()} returns the relative importance of independent
#' variables in determining response variable. How much each variable uniquely
#' contributes to rsquare over and above that which can be accounted for by the
#' other predictors? Zero order correlation is the Pearson correlation
#' coefficient between the dependent variable and the independent variables.
#' Part correlations indicates how much rsquare will decrease if that variable
#' is removed from the model and partial correlations indicates amount of
#' variance in response variable, which is not estimated by the other
#' independent variables in the model, but is estimated by the specific
#' variable.
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
#'
#' @importFrom stats cor
#'
#' @export
#'
ols_correlations <- function(model) UseMethod("ols_correlations")

#' @export
#'
ols_correlations.default <- function(model) {

  check_model(model)

  result <- corout(model, corm2(model))
  class(result) <- c("ols_correlations", "data.frame")
  return(result)

}

#' @export
#'
print.ols_correlations <- function(x, ...) {
  print_correlations(x)
}


#' @importFrom purrr map_df
cordata <- function(model) {

  ols_prep_avplot_data(model)

}

cmdata <- function(mdata) {

  mdata %>%
    cor() %>%
    `[`(-1, 1)

}

rtwo <- function(i, mdata) {

  dat <-
    mdata %>%
    select(-1, -i)

  lm(mdata[[1]] ~ ., data = dat) %>%
    summary() %>%
    extract2(8)

}

corsign <- function(model) {

  model %>%
    summary() %>%
    use_series(coefficients) %>%
    `[`(, 1) %>%
    extract(-1) %>%
    sign(.)

}

corout <- function(model, r2) {

  r.squared <- NULL

  r1  <-
    model %>%
    summary() %>%
    use_series(r.squared)

  mdata            <- cordata(model)
  cor_mdata        <- cmdata(mdata)
  n                <- ncol(mdata)
  ksign            <- corsign(model)
  n2               <- n - 1
  parts            <- ksign * sqrt(r1 - r2)
  partials         <- parts / sqrt(1 - r2)
  result           <- data.frame(cor_mdata, partials, parts)
  rownames(result) <- names(ksign)
  colnames(result) <- c("Zero-order", "Partial", "Part")

  return(result)

}

corm2 <- function(model) {

  mdata <- cordata(model)
  n     <- ncol(mdata)
  r2    <- c()

  for (i in 2:n) {
    out <- rtwo(i, mdata)
    r2  <- c(r2, out)
  }

  return(r2)
}
