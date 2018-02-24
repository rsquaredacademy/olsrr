#' @importFrom stats dnorm sd
#' @importFrom graphics hist
#' @title Residual Histogram
#' @description Histogram of residuals for detecting violation of normality assumption.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_rsd_hist(model)
#' @export
#'
ols_rsd_hist <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  x <- NULL
  y <- NULL

  k  <- histdata(model)
  h  <- hist(k$resid, plot = FALSE)
  l  <- histn(k$resid, h)
  d2 <- tibble(x = l$xfit, y = l$yfit)
  d  <- tibble(x = k$resid)

  p <- ggplot(d, aes(x = x)) +
    geom_histogram(bins = 6, color = "black", fill = "#ADD8E6") +
    geom_line(data = d2, aes(x = x, y = y), color = "#0000A0", size = 1.2) +
    xlab("Residuals") + ggtitle("Residual Histogram")

  print(p)

}

histdata <- function(model) {

  resid <- residuals(model)
  minx  <- min(resid) - 1
  maxx  <- max(resid) + 1
  list(resid = resid, minx = minx, maxx = maxx)

}

histn <- function(resid, h) {

  xfit  <- seq(min(resid), max(resid), length = 80)
  yfit  <- dnorm(xfit, mean = mean(resid), sd = sd(resid))
  yfit1 <- yfit * diff(h$mids[1:2]) * length(resid)
  list(xfit = xfit, yfit = yfit1)

}
