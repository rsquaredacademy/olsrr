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

  data <- ols_prep_regline_data(response, predictor)

  p <-
    ggplot(data$d, aes(x = x, y = y)) +
    geom_point(fill = "blue") +
    geom_point(data = data$d2, aes(x = x, y = y), color = "red", shape = 2, size = 3) +
    geom_smooth(method = "lm", se = FALSE)

  p <-
    p +
    labs(title = "Regression Line") +
    xlab(paste0(data$preds)) +
    ylab(paste0(data$resp))

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}

ols_prep_regline_data <- function(response, predictor) {
  resp        <- l(deparse(substitute(response)))
  preds       <- l(deparse(substitute(predictor)))
  m_predictor <- round(mean(predictor), 2)
  m_response  <- round(mean(response), 2)
  d2          <- data.frame(x = m_predictor, y = m_response)
  d           <- data.frame(x = predictor, y = response)

  list(d = d, d2 = d2, preds = preds, resp = resp)
}
