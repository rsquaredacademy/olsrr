#' @importFrom stats residuals
#' @importFrom ggplot2 geom_boxplot theme element_blank
#' @title Residual Box Plot
#' @description Box plot of residuals to examine if residuals
#' are normally distributed.
#' @param model an object of class \code{lm}
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_rsd_boxplot(model)
#' @export
#'
ols_rsd_boxplot <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  resid <-
    model %>%
    residuals()

  d <- tibble(resid = resid)

  p <- ggplot(d, aes(x = factor(0), y = resid)) +
    geom_boxplot(
      outlier.color = "green", outlier.size = 3,
      fill = "grey80", colour = "#3366FF"
    ) +
    xlab(" ") + ylab("Residuals") + ggtitle("Residual Box Plot") +
    theme(axis.text.x = element_blank())

  print(p)

}
