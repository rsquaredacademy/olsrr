#' @title Hadi Plot
#' @description Hadi's measure of influence based on the fact that influential observations can be present in either
#' the response variable or in the predictors or both. The plot is used to detect influential observations
#' based on Hadi's measure.
#' @param model an object of class \code{lm}
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_hadi_plot(model)
#'
#' @export
#'
ols_hadi_plot <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  hadi <- NULL

  hdi <-
    model %>%
    ols_hadi() %>%
    use_series(hadi) %>%
    unname()

  obs <-
    hdi %>%
    length() %>%
    seq_len()

  d <- tibble(obs = obs, hdi = hdi)

  p <- ggplot(d, aes(obs, hdi, ymin = min(hdi), ymax = hdi)) +
    geom_linerange(colour = "blue") +
    geom_point(shape = 1, colour = "blue") +
    xlab("Observation") + ylab("Hadi's Measure") +
    ggtitle("Hadi's Influence Measure")

  print(p)

}

