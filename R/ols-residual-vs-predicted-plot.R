#' Residual vs fitted plot
#'
#' @description
#' Scatter plot of residuals on the y axis and fitted values on the
#' x axis to detect non-linearity, unequal error variances, and outliers.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details Characteristics of a well behaved residual vs fitted plot:
#'
#' \itemize{
#'   \item The residuals spread randomly around the 0 line indicating that the relationship is linear.
#'   \item The residuals form an approximate horizontal band around the 0 line indicating homogeneity of error variance.
#'   \item No one residual is visibly away from the random pattern of the residuals indicating that there are no outliers.
#' }
#'
#' @section Deprecated Function:
#' \code{ols_rvsp_plot()} has been deprecated. Instead use \code{ols_plot_resid_fit()}.
#'
#' @family residual diagnostics
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_resid_fit(model)
#'
#' @export
#'
ols_plot_resid_fit <- function(model, print_plot = TRUE) {

  check_model(model)

  predicted <- NULL
  resid     <- NULL

  d <- rvspdata(model)

  p <-
    ggplot(d, aes(x = predicted, y = resid)) +
    geom_point(shape = 1, colour = "blue") +
    xlab("Fitted Value") + ylab("Residual") +
    ggtitle("Residual vs Fitted Values") +
    geom_hline(yintercept = 0, colour = "red")

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}

#' @export
#' @rdname ols_plot_resid_fit
#' @usage NULL
#'
ols_rvsp_plot <- function(model) {
  .Deprecated("ols_plot_resid_fit()")
}


rvspdata <- function(model) {

  resid     <- residuals(model)
  predicted <- fitted(model)
  data.frame(predicted = predicted, resid = resid)

}
