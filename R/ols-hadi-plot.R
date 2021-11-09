#' Hadi plot
#'
#' @description
#' Hadi's measure of influence based on the fact that influential observations
#' can be present in either the response variable or in the predictors or both.
#' The plot is used to detect influential observations based on Hadi's measure.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_hadi(model)
#'
#' @seealso [ols_plot_resid_pot()]
#'
#' @export
#'
ols_plot_hadi <- function(model, print_plot = TRUE) {

  check_model(model)

  hadi <- NULL
  hdi  <- unname(ols_hadi(model)$hadi)
  obs  <- seq_len(length(hdi))
  d    <- data.frame(obs = obs, hdi = hdi)

  p <-
    ggplot(d, aes(obs, hdi, ymin = min(hdi), ymax = hdi)) +
    geom_linerange(colour = "blue") +
    geom_point(shape = 1, colour = "blue") +
    xlab("Observation") + 
    ylab("Hadi's Measure") +
    ggtitle("Hadi's Influence Measure")

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}