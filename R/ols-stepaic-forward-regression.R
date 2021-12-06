#' Stepwise AIC forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on akaike information criterion, in a stepwise
#' manner until there is no variable left to enter any more.
#'
#' @param model An object of class \code{lm}.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_forward_aic}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#' @return \code{ols_step_forward_aic} returns an object of class \code{"ols_step_forward_aic"}.
#' An object of class \code{"ols_step_forward_aic"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#' \item{others}{list; info used for plotting and printing}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_aic(model)
#'
#' # stepwise forward regression plot
#' k <- ols_step_forward_aic(model)
#' plot(k)
#'
#' # extract final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' ols_step_forward_aic(model, include = c("age"))
#'
#' # use index of variable instead of name
#' ols_step_forward_aic(model, include = c(5))
#'
#' # force variable to be excluded from selection process
#' ols_step_forward_aic(model, exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_aic(model, exclude = c(4))
#'
#' # include & exclude variables in the selection process
#' ols_step_forward_aic(model, include = c("age"), exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_aic(model, include = c(5), exclude = c(4))
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_forward_aic <- function(model, ...) UseMethod("ols_step_forward_aic")

#' @export
#' @rdname ols_step_forward_aic
#'
ols_step_forward_aic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_forward(model, "aic", include, exclude, progress, details)  
  class(out) <- "ols_step_forward_aic"
  return(out)
}

#' @export
#'
print.ols_step_forward_aic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "forward")
  } else {
    print("No variables have been added to the model.")
  }
}

#' @rdname ols_step_forward_aic
#' @export
#'
plot.ols_step_forward_aic <- function(x, print_plot = TRUE, details = TRUE, ...) {

  p <- ols_stepaic_plot(x, details)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
