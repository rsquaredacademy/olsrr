#' Residual fit spread plot
#'
#' Plot to detect non-linearity, influential observations and outliers.
#'
#' @param model An object of class \code{lm}.
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
#' @section Deprecated Function:
#' \code{ols_rfs_plot()}, \code{ols_fm_plot()} and \code{ols_rsd_plot()}
#'   has been deprecated. Instead use \code{ols_plot_resid_fit_spread()},
#'   \code{ols_plot_fm()} and \code{ols_plot_resid_spread()}.
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
ols_plot_resid_fit_spread <- function(model) {

  check_model(model)

  x <- NULL
  y <- NULL

  d1    <- ols_prep_rfsplot_fmdata(model)
  ymin1 <- min(d1$y) + (0.25 * min(d1$y))
  ymax1 <- max(d1$y) + (0.25 * max(d1$y))

  p1 <- ggplot(d1, aes(x = x, y = y)) + geom_point(shape = 1, color = "blue") +
    xlim(c(-0.2, 1.2)) + ylim(c(ymin1, ymax1)) + xlab("Proportion Less") +
    ylab("") + ggtitle("Fit - Mean")

  d2    <- ols_prep_rfsplot_rsdata(model)
  ymin2 <- min(d2$y) + (0.25 * min(d2$y))
  ymax2 <- max(d2$y) + (0.25 * max(d2$y))

  p2 <- ggplot(d2, aes(x = x, y = y)) + geom_point(color = "blue", shape = 1) +
    ylim(c(ymin2, ymax2)) + xlim(c(-0.2, 1.2)) + xlab("Proportion Less") +
    ylab("") + ggtitle("Residual")

  grid.arrange(p1, p2, ncol = 2)

  result <- list(fm_plot = p1, rsd_plot = p2)
  invisible(result)

}

#' @rdname ols_plot_resid_fit_spread
#' @export
#'
ols_plot_fm <- function(model) {

  check_model(model)

  x <- NULL
  y <- NULL

  d    <- ols_prep_rfsplot_fmdata(model)
  ymin <- min(d$y) + (0.25 * min(d$y))
  ymax <- max(d$y) + (0.25 * max(d$y))

  p <- ggplot(d, aes(x = x, y = y)) + geom_point(shape = 1, color = "blue") +
    xlim(c(-0.2, 1.2)) + ylim(c(ymin, ymax)) + xlab("Proportion Less") +
    ylab("Fit - Mean") + ggtitle("Residual Fit Spread Plot")

  print(p)

}


#' @rdname ols_plot_resid_fit_spread
#' @export
#'
ols_plot_resid_spread <- function(model) {

  check_model(model)

  x <- NULL
  y <- NULL

  d    <- ols_prep_rfsplot_rsdata(model)
  ymin <- min(d$y) + (0.25 * min(d$y))
  ymax <- max(d$y) + (0.25 * max(d$y))

  p <- ggplot(d, aes(x = x, y = y)) + geom_point(color = "blue", shape = 1) +
    ylim(c(ymin, ymax)) + xlim(c(-0.2, 1.2)) + ylab("Residual") +
    xlab("Proportion Less") + ggtitle("Residual Fit Spread Plot")

  print(p)

}


#' @export
#' @rdname ols_plot_resid_fit_spread
#' @usage NULL
#'
ols_rfs_plot <- function(model) {
  .Deprecated("ols_plot_resid_fit_spread()")
}


#' @export
#' @rdname ols_plot_resid_fit_spread
#' @usage NULL
#'
ols_rsd_plot <- function(model) {
  .Deprecated("ols_plot_resid_spread()")
}

#' @export
#' @rdname ols_plot_resid_fit_spread
#' @usage NULL
#'
ols_fm_plot <- function(model) {
  .Deprecated("ols_plot_fm()")
}
