#' Residual QQ plot
#'
#' Graph for detecting violation of normality assumption.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_resid_qq(model)
#'
#' @family residual diagnostics
#'
#' @importFrom stats qqnorm qqline
#'
#' @export
#'
ols_plot_resid_qq <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  resid <- residuals(model)
  y     <- quantile(resid[!is.na(resid)], c(0.25, 0.75))
  x     <- qnorm(c(0.25, 0.75))
  slope <- diff(y) / diff(x)
  int   <- y[1L] - slope * x[1L]
  d     <- tibble(x = resid)

  p <- ggplot(d, aes(sample = x)) + stat_qq(color = "blue") +
    geom_abline(slope = slope, intercept = int, color = "red") +
    xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
    ggtitle("Normal Q-Q Plot")

  print(p)

}


#' @export
#' @rdname ols_plot_resid_qq
#' @usage NULL
#'
ols_rsd_qqplot <- function(model) {
  .Deprecated("ols_plot_resid_qq()")
}
