#'  Response variable profile
#'
#' Panel of plots to explore and visualize the response variable.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_response(model)
#'
#' @importFrom stats model.frame model.response
#' @importFrom ggplot2 geom_dotplot geom_histogram
#'
#' @export
#'
ols_plot_response <- function(model, print_plot = TRUE) {

  check_model(model)

  d  <- ols_response_data(model)
  p1 <- ols_response_dotplot(d$d1, d$nam)
  p2 <- ols_response_trend(d$d2, d$nam)
  p3 <- ols_response_hist(d$d1, d$nam)
  p4 <- ols_response_box(d$d1, d$nam)

  if (print_plot) {
    grid.arrange(p1, p2, p3, p4, ncol = 2, top = "Response Diagnostics")
  } else {
    return(
      list(dot_plot   = p1,
           trend_plot = p2,
           histogram  = p3,
           boxplot    = p4)
      )
  }

}

ols_response_data <- function(model) {
  nam  <- names(model.frame(model))
  pred <- model.response(model.frame(model))
  xval <- seq_len(length(pred))
  d1   <- data.frame(x = pred)
  d2   <- data.frame(x = xval, y = pred)

  list(nam = nam, d1 = d1, d2 = d2)
}

ols_response_dotplot <- function(d, nam) {
  ggplot(d, aes(x = x)) +
    geom_dotplot(binwidth = 1, fill = "blue") +
    xlab(nam[1]) +
    ggtitle(paste("Dot Plot of", nam[1]))
}

ols_response_trend <- function(d, nam) {
  ggplot(d, aes(x = x, y = y)) +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    xlab("Observation") +
    ylab(nam[1]) +
    ggtitle(paste("Trend Plot of", nam[1]))
}

ols_response_hist <- function(d, nam) {
  ggplot(d, aes(x = x)) +
    geom_histogram(bins = 5, color = "black", fill = "blue") +
    xlab(nam[1]) +
    ggtitle(paste("Histogram of", nam[1]))
}

ols_response_box <- function(d, nam) {
  ggplot(d, aes(x = factor(0), y = x)) +
    geom_boxplot(fill = "blue") +
    xlab("") +
    ylab(nam[1]) +
    ggtitle(paste("Boxplot of", nam[1])) +
    theme(axis.text.x = element_blank())
}
