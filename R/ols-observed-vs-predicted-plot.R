#' Observed vs fitted values plot
#'
#' Plot of observed vs fitted values to assess the fit of the model.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' Ideally, all your points should be close to a regressed diagonal line. Draw
#' such a diagonal line within your graph and check out where the points lie. If
#' your model had a high R Square, all the points would be close to this
#' diagonal line. The lower the R Square, the weaker the Goodness of fit of your
#' model, the more foggy or dispersed your points are from this diagonal line.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_obs_fit(model)
#'
#' @importFrom stats fitted.values
#' @importFrom ggplot2 geom_abline geom_segment
#'
#' @export
#'
ols_plot_obs_fit <- function(model, print_plot = TRUE) {

  check_model(model)

  oname <- names(model.frame(model))[1]
  d     <- obspred(model)
  df    <- data.frame(x1 = min(d$x), x2 = max(d$x), y1 = min(d$y), y2 = max(d$y))

  p <-
    ggplot(d, aes(x = x, y = y)) +
    geom_point(color = "blue", shape = 1) +
    geom_segment(data = df, aes(x = x1, y = y1, xend = x2,
      yend = y2), colour = "red") +
    geom_abline(intercept = 0, slope = 1, color = "blue")

  p <-
    p +
    ylab("Fitted Value") +
    xlab(paste(oname)) +
    ggtitle(paste("Actual vs Fitted for", oname))

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}

obspred <- function(model) {

  y <- fitted(model)
  x <- model.frame(model)[[1]]
  data.frame(x, y)

}
