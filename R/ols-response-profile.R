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

  x <- NULL
  y <- NULL

  nam       <- names(model.frame(model))
  predictor <- model.response(model.frame(model))
  xval      <- seq_len(length(predictor))
  d1        <- data.frame(x = predictor)

  p1 <- 
    ggplot(d1, aes(x = x)) +
    geom_dotplot(binwidth = 1, fill = "blue") +
    xlab(nam[1]) + 
    ggtitle(paste("Dot Plot of", nam[1]))

  d2 <- data.frame(x = xval, y = predictor)
  p2 <- 
    ggplot(d2, aes(x = x, y = y)) +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    xlab("Observation") + 
    ylab(nam[1]) +
    ggtitle(paste("Trend Plot of", nam[1]))


  d3 <- data.frame(x = predictor)
  p3 <- 
    ggplot(d3, aes(x = x)) +
    geom_histogram(bins = 5, color = "black", fill = "blue") +
    xlab(nam[1]) + 
    ggtitle(paste("Histogram of", nam[1]))


  d4 <- data.frame(x = predictor)
  p4 <- 
    ggplot(d4, aes(x = factor(0), y = x)) +
    geom_boxplot(fill = "blue") +
    xlab("") + 
    ylab(nam[1]) +
    ggtitle(paste("Boxplot of", nam[1])) +
    theme(axis.text.x = element_blank())

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