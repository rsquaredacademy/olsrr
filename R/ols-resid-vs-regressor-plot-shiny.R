#' @title Residual vs regressors plot for shiny app
#'
#' @description Graph to determine whether we should add a new predictor to the
#' model already containing other predictors. The residuals from the model is
#' regressed on the new predictor and if the plot shows non random pattern,
#' you should consider adding the new predictor to the model.
#'
#' @param model An object of class \code{lm}.
#' @param data A \code{data.frame} or \code{tibble}.
#' @param variable Character; new predictor to be added to the \code{model}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' rvsr_plot_shiny(model, mtcars, 'drat')
#'
#' @export
#'
rvsr_plot_shiny <- function(model, data, variable, print_plot = TRUE) {

  check_model(model)
  check_data(data)

  x <- NULL
  y <- NULL
  d <- ols_prep_rvsrplot_data(model)
  v <- l(deparse(substitute(variable)))

  xvar <-
    data %>%
    select(!! sym(variable))

  k <- data.frame(x = xvar, y = model$residuals)
  colnames(k) <- c("x", "y")

  p <-
    ggplot(k, aes(x = x, y = y)) +
    geom_point(shape = 1, colour = "blue") +
    xlab(paste(variable)) + ylab("Residual") +
    ggtitle(paste("Residual vs", variable)) +
    geom_hline(yintercept = 0, colour = "red")

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}

