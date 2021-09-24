#' Simple linear regression line
#'
#' @description
#' Plot to demonstrate that the regression line always passes  through mean of
#' the response and predictor variables.
#'
#' @param response Response variable.
#' @param predictor Predictor variable.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' ols_plot_reg_line(mtcars$mpg, mtcars$disp)
#'
#' @importFrom ggplot2 labs geom_smooth
#'
#' @export
#'
ols_plot_reg_line <- function(response, predictor, print_plot = TRUE) {

  resp        <- l(deparse(substitute(response)))
  preds       <- l(deparse(substitute(predictor)))
  m_predictor <- round(mean(predictor), 2)
  m_response  <- round(mean(response), 2)
  x           <- NULL
  y           <- NULL
  d2          <- data.frame(x = m_predictor, y = m_response)
  d           <- data.frame(x = predictor, y = response)

  p <-
    ggplot(d, aes(x = x, y = y)) + geom_point(fill = "blue") +
    xlab(paste0(preds)) + ylab(paste0(resp)) + labs(title = "Regression Line") +
    geom_point(data = d2, aes(x = x, y = y), color = "red", shape = 2, size = 3) +
    geom_smooth(method = "lm", se = FALSE)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}