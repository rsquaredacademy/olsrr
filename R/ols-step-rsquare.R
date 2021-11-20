#' Stepwise R-Square regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on r-square and adjusted r-square, in a stepwise
#' manner until there is no variable left to enter any more.
#'
#' @param model An object of class \code{lm}.
#' @param metric R-Squared or adjusted R-squared.
#' @param direction Stepwise direction; one among \code{forward}, \code{backward} or \code{both}.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_rsquared}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#' @return \code{ols_step_rsquared} returns an object of class \code{"ols_step_rsquared"}.
#' An object of class \code{"ols_step_rsquared"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#' \item{others}{list; info used for plotting and printing}
#'
#' @examples
#' \dontrun{
#' # stepwise forward r-squared regression
#' model <- lm(y ~ ., data = stepdata)
#' ols_step_rsquared(model)
#'
#' # stepwise regression plot
#' model <- lm(y ~ ., data = stepdata)
#' k <- ols_step_rsquared(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' # use adjusted r-squared
#' # force variable to be included in selection process
#' ols_step_rsquared(model, metric = "adj_r2")
#'
#' # stepwise backward regression
#' ols_step_rsquared(model, metric = "adj_r2", direction = "backward")
#'
#' # stepwise both direction regression
#' ols_step_rsquared(model, metric = "adj_r2", direction = "both")
#' }
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_rsquared <- function(model, ...) UseMethod("ols_step_rsquared")

#' @rdname ols_step_rsquared
#' @export
#'
ols_step_rsquared.default <- function(model, metric = c("r2", "adj_r2"), direction = c("forward", "backward", "both"), include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {

  method <- match.arg(direction)
  metric <- match.arg(metric)

  if (method == "forward") {
    ols_step_rsquared_forward(model, metric, include, exclude, progress, details)
  } else if (method == "backward") {
    ols_step_rsquared_backward(model, metric, include, exclude, progress, details)
  } else {
    ols_step_rsquared_both(model, metric, include, exclude, progress, details)
  }
}

#' @rdname ols_step_rsquared
#' @export
#'
print.ols_step_rsquared <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_rsquared(x)
  } else {
    if (x$others$direction == "forward") {
      print("No variables have been added to the model.")
    } else if (x$others$direction == "backward") {
      print("No variables have been removed from the model.")
    } else {
      print("No variables have been added to or removed from the model.")
    } 
    
  }
}

#' @rdname ols_step_rsquared
#' @export
#'
plot.ols_step_rsquared <- function(x, print_plot = TRUE, details = TRUE, ...) {
  ols_plot_rsquared(x, print_plot, details)
}
