#' @title Residual vs regressors plot for shiny app
#'
#' @description Graph to determine whether we should add a new predictor to the
#' model already containing other predictors. The residuals from the model is
#' regressed on the new predictor and if the plot shows non random pattern,
#' you should consider adding the new predictor to the model.
#'
#' @param model An object of class \code{lm}.
#' @param data A \code{dataframe} or \code{tibble}.
#' @param variable Character; new predictor to be added to the \code{model}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' rvsr_plot_shiny(model, mtcars, 'drat')
#'
#' @export
#'
rvsr_plot_shiny <- function(model, data, variable) {

  check_model(model)
  check_data(data)

  x <- NULL
  y <- NULL
  d <- rvsrdata(model)
  v <- l(deparse(substitute(variable)))

  xvar <-
    data %>%
    select(!! sym(variable))

  k <- data.frame(x = xvar, y = model$residuals)
  colnames(k) <- c("x", "y")

  p <- ggplot(k, aes(x = x, y = y)) +
    geom_point(shape = 1, colour = "blue") +
    xlab(paste(variable)) + ylab("Residual") +
    ggtitle(paste("Residual vs", variable)) +
    geom_hline(yintercept = 0, colour = "red")


  print(p)
}

