#' Model formula
#'
#' Returns the model formula.
#'
#' @param model An object of class \code{lm}.
#'
#' @return An object of class \code{formula}.
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_get_formula(model)
#'
#' @family model info
#'
#' @export
#'
ols_get_formula <- function(model) {
  formula(model)
}
