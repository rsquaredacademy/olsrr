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

  p1  <- ols_plot_resid_fit(model, print_plot = FALSE)
  p2  <- ols_plot_resid_stud_fit(model, print_plot = FALSE)$plot
  p3  <- ols_plot_resid_lev(model, print_plot = FALSE)$plot
  p4  <- ols_plot_resid_qq(model, print_plot = FALSE)
  p5  <- ols_plot_obs_fit(model, print_plot = FALSE)
  p6  <- ols_plot_cooksd_chart(model, print_plot = FALSE)$plot
  p7  <- ols_plot_fm(model, print_plot = FALSE)
  p8  <- ols_plot_resid_spread(model, print_plot = FALSE)
  p9  <- ols_plot_resid_hist(model, print_plot = FALSE)
  p10 <- ols_plot_resid_box(model, print_plot = FALSE)

  result <- list(
    plot_1 = p1, plot_2 = p2, plot_3 = p3, plot_4 = p4,
    plot_5 = p5, plot_6 = p6, plot_7 = p7, plot_8 = p8,
    plot_9 = p9, plot_10 = p10
  )

  if (print_plot) {
    g <- marrangeGrob(result, nrow = 2, ncol = 2, top = "Regression Diagnostics")
    grid::grid.draw(g)
    invisible(g)
  } else {
    return(result)
  }

}
