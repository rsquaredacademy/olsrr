#' @importFrom stats ecdf
#' @importFrom gridExtra grid.arrange
#' @title Residual Fit Spread Plot
#' @description Plot to detect non-linearity, influential observations and outliers.
#' @param model an object of class \code{lm}
#' @details Consists of side-by-side quantile plots of the centered fit and the residuals. It shows how
#' much variation in the data is explained by the fit and how much remains in the residuals. For
#' inappropriate models, the spread of the residuals in such a plot is often greater than the spread of the
#' centered fit.
#' @references Cleveland, W. S. (1993). Visualizing Data. Summit, NJ: Hobart Press.
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#'
#' # residual fit spread plot
#' ols_rfs_plot(model)
#'
#' # fit mean plot
#' ols_fm_plot(model)
#'
#' # residual spread plot
#' ols_rsd_plot(model)
#'
#' @export
#'
ols_rfs_plot <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }
  x <- NULL
  y <- NULL
  d1 <- fmdata(model)
  ymin1 <- min(d1$y) + (0.25 * min(d1$y))
  ymax1 <- max(d1$y) + (0.25 * max(d1$y))

  p1 <- ggplot(d1, aes(x = x, y = y))
  p1 <- p1 + geom_point(shape = 1, color = "blue")
  p1 <- p1 + xlim(c(-0.2, 1.2)) + ylim(c(ymin1, ymax1))
  p1 <- p1 + xlab("Proportion Less") + ylab("")
  p1 <- p1 + ggtitle("Fit - Mean")

  d2 <- rsdata(model)
  ymin2 <- min(d2$y) + (0.25 * min(d2$y))
  ymax2 <- max(d2$y) + (0.25 * max(d2$y))

  p2 <- ggplot(d2, aes(x = x, y = y))
  p2 <- p2 + geom_point(color = "blue", shape = 1)
  p2 <- p2 + ylim(c(ymin2, ymax2)) + xlim(c(-0.2, 1.2))
  p2 <- p2 + xlab("Proportion Less") + ylab("")
  p2 <- p2 + ggtitle("Residual")

  grid.arrange(p1, p2, ncol = 2)

  result <- list(fm_plot = p1, rsd_plot = p2)
  invisible(result)
}

#' @rdname ols_rfs_plot
#' @export
#'
ols_fm_plot <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }
  x <- NULL
  y <- NULL
  d <- fmdata(model)
  ymin <- min(d$y) + (0.25 * min(d$y))
  ymax <- max(d$y) + (0.25 * max(d$y))

  p <- ggplot(d, aes(x = x, y = y))
  p <- p + geom_point(shape = 1, color = "blue")
  p <- p + xlim(c(-0.2, 1.2)) + ylim(c(ymin, ymax))
  p <- p + xlab("Proportion Less") + ylab("Fit - Mean")
  p <- p + ggtitle("Residual Fit Spread Plot")
  print(p)
}


#' @rdname ols_rfs_plot
#' @export
#'
ols_rsd_plot <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  x <- NULL
  y <- NULL
  d <- rsdata(model)
  ymin <- min(d$y) + (0.25 * min(d$y))
  ymax <- max(d$y) + (0.25 * max(d$y))

  p <- ggplot(d, aes(x = x, y = y))
  p <- p + geom_point(color = "blue", shape = 1)
  p <- p + ylim(c(ymin, ymax)) + xlim(c(-0.2, 1.2))
  p <- p + ylab("Residual") + xlab("Proportion Less")
  p <- p + ggtitle("Residual Fit Spread Plot")
  print(p)
}
