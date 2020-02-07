#' Residual box plot
#'
#' Box plot of residuals to examine if residuals are normally distributed.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @section Deprecated Function:
#' \code{ols_rsd_boxplot()} has been deprecated. Instead use \code{ols_plot_resid_box()}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_resid_box(model)
#'
#' @family residual diagnostics
#'
#' @importFrom stats residuals
#' @importFrom ggplot2 geom_boxplot theme element_blank
#'
#' @export
#'
ols_plot_resid_box <- function(model, print_plot = TRUE) {

  check_model(model)

  resid <- residuals(model)
  d     <- data.frame(resid = resid)

  p <-
    ggplot(d, aes(x = factor(0), y = resid)) +
    geom_boxplot(outlier.color = "green", outlier.size = 3,
                 fill = "grey80", colour = "#3366FF") +
    xlab(" ") + ylab("Residuals") + ggtitle("Residual Box Plot") +
    theme(axis.text.x = element_blank())

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}


#' @export
#' @rdname ols_plot_resid_box
#' @usage NULL
#'
ols_rsd_boxplot <- function(model) {
  .Deprecated("ols_plot_resid_box()")
}
