#' Added variable plot data
#'
#' Data for generating the added variable plots.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_prep_avplot_data(model)
#'
#' @importFrom stats model.frame residuals as.formula
#' @importFrom dplyr bind_cols
#'
#' @export
#'
ols_prep_avplot_data <- function(model) {

  m1 <-
    model %>%
    model.frame() %>%
    as_data_frame()

  m2 <-
    model %>%
    model.matrix() %>%
    as_data_frame() %>%
    select(-1)

  m1 %>%
    select(1) %>%
    bind_cols(m2) %>%
    as_data_frame()

}

#' Regress predictor on other predictors
#'
#' Regress a predictor in the model on all the other predictors.
#'
#' @param data A \code{data.frame}.
#' @param i A numeric vector (indicates the predictor in the model).
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' data <- ols_prep_avplot_data(model)
#' ols_prep_regress_x(data, 1)
#'
#' @importFrom stats lsfit
#'
#' @export
#'
ols_prep_regress_x <- function(data, i) {

  x <- remove_columns(data, i)
  y <- select_columns(data, i)
  lsfit(x, y) %>%
    use_series(residuals)

}

#' Regress y on other predictors
#'
#' Regress y on all the predictors except the ith predictor.
#'
#' @param data A \code{data.frame}.
#' @param i A numeric vector (indicates the predictor in the model).
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' data <- ols_prep_avplot_data(model)
#' ols_prep_regress_y(data, 1)
#'
#' @export
#'
ols_prep_regress_y <- function(data, i) {

  x <- remove_columns(data, i)
  y <- select_columns(data)
  lsfit(x, y) %>%
    use_series(residuals)

}
