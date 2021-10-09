#' Residual vs regressor plot
#'
#' @description
#' Graph to determine whether we should add a new predictor to the model already
#' containing other predictors. The residuals from the model is regressed on the
#' new predictor and if the plot shows non random pattern, you should consider
#' adding the new predictor to the model.
#'
#' @param model An object of class \code{lm}.
#' @param variable New predictor to be added to the \code{model}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_resid_regressor(model, 'drat')
#'
#' @seealso [ols_plot_added_variable()], [ols_plot_comp_plus_resid()]
#'
#' @export
#'
ols_plot_resid_regressor <- function(model, variable, print_plot = TRUE) {

  check_model(model)

  x     <- NULL
  y     <- NULL
  d     <- ols_prep_rvsrplot_data(model)
  inter <- eval(model$call$data)[variable]
  x     <- inter[[1]]
  y     <- residuals(model)
  v     <- names(inter)
  k     <- data.frame(x = x, y = y)

  p <-
    ggplot(k, aes(x = x, y = y)) +
    geom_point(shape = 1, colour = "blue") +
    xlab(paste(v)) + ylab("Residual") +
    ggtitle(paste("Residual vs", v)) +
    geom_hline(yintercept = 0, colour = "red")

  if (print_plot) {
    print(p)
  }

  return(p)

}