#' @title Residual vs Regressors Plot Shiny
#' @description Graph to determine whether we should add a new predictor to the model already containing other predictors.
#' The residuals from the model is regressed on the new predictor and if the plot shows non random pattern,
#' you should consider adding the new predictor to the model.
#' @param model an object of class \code{lm}
#' @param data dataframe
#' @param variable character; new predictor to be added to the \code{model}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' rvsr_plot_shiny(model, mtcars, 'drat')
#' @export
#'
rvsr_plot_shiny <- function(model, data, variable) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

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

