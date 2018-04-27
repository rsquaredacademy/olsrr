#' Hadi plot
#'
#' @description
#' Hadi's measure of influence based on the fact that influential observations
#' can be present in either the response variable or in the predictors or both.
#' The plot is used to detect influential observations based on Hadi's measure.
#'
#' @param model An object of class \code{lm}.
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @section Deprecated Function:
#' \code{ols_hadi_plot()} has been deprecated. Instead use \code{ols_plot_hadi()}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_hadi(model)
#'
#' @seealso [ols_plot_resid_pot()]
#'
#' @export
#'
ols_plot_hadi <- function(model) {

  check_model(model)

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

#' @export
#' @rdname ols_plot_hadi
#' @usage NULL
#'
ols_hadi_plot <- function(model) {
  .Deprecated("ols_plot_hadi()")
}
