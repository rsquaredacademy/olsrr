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

  x     <- NULL
  y     <- NULL
  oname <- names(model.frame(model))[1]
  d     <- obspred(model)

  p <-
    ggplot(d, aes(x = x, y = y)) +
    geom_point(color = "blue", shape = 1) +
    ylab("Fitted Value") + 
    xlab(paste(oname)) +
    ggtitle(paste("Actual vs Fitted for", oname)) +
    geom_abline(intercept = 0, slope = 1, color = "blue") +
    geom_segment(data = d, 
                 aes(x = min(x), y = min(y), xend = max(x), yend = max(y)), 
                 colour = "red")

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
