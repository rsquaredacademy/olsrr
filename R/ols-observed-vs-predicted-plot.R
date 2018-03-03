#' Observed vs fitted values plot
#'
#' Plot of observed vs fitted values to assess the fit of the model.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' Ideally, all your points should be close to a regressed diagonal line. Draw
#' such a diagonal line within your graph and check out where the points lie. If
#' your model had a high R Square, all the points would be close to this
#' diagonal line. The lower the R Square, the weaker the Goodness of fit of your
#' model, the more foggy or dispersed your points are from this diagonal line.
#'
#' @section Deprecated Function:
#' \code{ols_ovsp_plot()} has been deprecated. Instead use \code{ols_plot_obs_fit()}.
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
ols_plot_obs_fit <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  x <- NULL
  y <- NULL

  oname <-
    model %>%
    model.frame() %>%
    names() %>%
    extract(1)

  d <- obspred(model)

  p <- ggplot(d, aes(x = x, y = y)) +
    geom_point(color = "blue", shape = 1) +
    ylab("Fitted Value") + xlab(paste(oname)) +
    ggtitle(paste("Actual vs Fitted for", oname)) +
    geom_abline(intercept = 0, slope = 1, color = "blue") +
    geom_segment(data = d, aes(x = min(x), y = min(y), xend = max(x),
                               yend = max(y)), colour = "red")

  print(p)

}

#' @export
#' @rdname ols_plot_obs_fit
#' @usage NULL
#'
ols_ovsp_plot <- function(model) {
  .Deprecated("ols_plot_obs_fit()")
}


obspred <- function(model) {

  y <- fitted(model)
  x <-
    model %>%
    model.frame() %>%
    extract2(1)

  tibble(x, y)

}
