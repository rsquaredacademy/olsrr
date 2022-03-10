#' Diagnostics panel
#'
#' Panel of plots for regression diagnostics.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_plot_diagnostics(model)
#'
#' @importFrom ggplot2 xlim stat_qq geom_histogram geom_line theme element_blank
#' @importFrom stats quantile
#'
#' @export
#'
ols_plot_diagnostics <- function(model, print_plot = TRUE) {

  check_model(model)

  p1  <- ols_plot_resid_fit(model)
  p2  <- ols_plot_resid_stud_fit(model)
  p3  <- ols_plot_resid_lev(model)
  p4  <- ols_plot_resid_qq(model)
  p5  <- ols_plot_obs_fit(model) 
  p6  <- ols_plot_cooksd_chart(model)
  p7  <- ols_plot_fm(model)
  p8  <- ols_plot_resid_spread(model)
  p9  <- ols_plot_resid_hist(model)
  p10 <- ols_plot_resid_box(model)

  result <- list(
    plot_1 = p1, plot_2 = p2, plot_3 = p3, plot_4 = p4,
    plot_5 = p5, plot_6 = p6, plot_7 = p7, plot_8 = p8,
    plot_9 = p9, plot_10 = p10
  )

  if (print_plot) {
    marrangeGrob(result, nrow = 2, ncol = 2, top = "Regression Diagnostics")
  } else {
    return(result)
  }

}
