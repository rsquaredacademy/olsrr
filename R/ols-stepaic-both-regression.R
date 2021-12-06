#' Stepwise AIC regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering and removing predictors based on akaike information criteria, in a
#' stepwise manner until there is no variable left to enter or remove any more.
#'
#' @param model An object of class \code{lm}.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, details of variable selection will
#'   be printed on screen.
#' @param x An object of class \code{ols_step_both_aic}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#'
#' @return \code{ols_step_both_aic} returns an object of class \code{"ols_step_both_aic"}.
#' An object of class \code{"ols_step_both_aic"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' \dontrun{
#' # stepwise regression
#' model <- lm(y ~ ., data = stepdata)
#' ols_step_both_aic(model)
#'
#' # stepwise regression plot
#' model <- lm(y ~ ., data = stepdata)
#' k <- ols_step_both_aic(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' model <- lm(y ~ ., data = stepdata)
#'
#' ols_step_both_aic(model, include = c("x6"))
#'
#' # use index of variable instead of name
#' ols_step_both_aic(model, include = c(6))
#'
#' # force variable to be excluded from selection process
#' ols_step_both_aic(model, exclude = c("x2"))
#'
#' # use index of variable instead of name
#' ols_step_both_aic(model, exclude = c(2))
#'
#' # include & exclude variables in the selection process
#' ols_step_both_aic(model, include = c("x6"), exclude = c("x2"))
#'
#' # use index of variable instead of name
#' ols_step_both_aic(model, include = c(6), exclude = c(2))
#' }
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_both_aic <- function(model, ...) UseMethod("ols_step_both_aic")

#' @export
#' @rdname ols_step_both_aic
#'
ols_step_both_aic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_both(model, "aic", include, exclude, progress, details)  
  class(out) <- "ols_step_both_aic"
  return(out)
}

#' @export
#'
print.ols_step_both_aic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "both")
  } else {
    print("No variables have been added to or removed from the model.")
  }
}

#' @rdname ols_step_both_aic
#' @export
#'
plot.ols_step_both_aic <- function(x, print_plot = TRUE, details = TRUE, ...) {

  p <- ols_stepaic_plot(x, "both", details)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
