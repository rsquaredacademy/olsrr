#' Residual fit spread plot
#'
#' Plot to detect non-linearity, influential observations and outliers.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' Consists of side-by-side quantile plots of the centered fit and the
#' residuals. It shows how much variation in the data is explained by the fit
#' and how much remains in the residuals. For inappropriate models, the spread
#' of the residuals in such a plot is often greater than the spread of the
#' centered fit.
#'
#' @references
#' Cleveland, W. S. (1993). Visualizing Data. Summit, NJ: Hobart Press.
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#'
#' # residual fit spread plot
#' ols_plot_resid_fit_spread(model)
#'
#' # fit mean plot
#' ols_plot_fm(model)
#'
#' # residual spread plot
#' ols_plot_resid_spread(model)
#'
#' @importFrom stats ecdf
#' @importFrom gridExtra grid.arrange
#'
#' @export
#'
ols_plot_resid_fit_spread <- function(model, print_plot = TRUE) {

  p1 <- ols_plot_fm(model, FALSE)
  p2 <- ols_plot_resid_spread(model, FALSE)

  if (print_plot) {
    grid.arrange(p1, p2, ncol = 2)
  } else {
    return(list(fm_plot = p1, rsd_plot = p2))
  }

}

#' @rdname ols_plot_resid_fit_spread
#' @export
#'
ols_plot_fm <- function(model, print_plot = TRUE) {

  check_model(model)

  p <- ols_plot_rfs(model)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}


#' @rdname ols_plot_resid_fit_spread
#' @export
#'
ols_plot_resid_spread <- function(model, print_plot = TRUE) {

  check_model(model)

  p <- ols_plot_rfs(model, fit_mean = FALSE)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}


ols_plot_rfs <- function(model, fit_mean = TRUE) {

  if (fit_mean) {
    d <- ols_prep_rfsplot_fmdata(model)  
    y_lab <- "Fit - Mean"
  } else {
    d <- ols_prep_rfsplot_rsdata(model)
    y_lab <- "Residual"
  }

  ymin <- min(d$y) + (0.25 * min(d$y))
  ymax <- max(d$y) + (0.25 * max(d$y))

  p <-
    ggplot(d, aes(x = x, y = y)) +
    geom_point(shape = 1, color = "blue") +
    xlab("Proportion Less") +
    ylab(y_lab) +
    ggtitle("Residual Fit Spread Plot") +
    xlim(c(-0.2, 1.2)) +
    ylim(c(ymin, ymax))

  return(p)

}