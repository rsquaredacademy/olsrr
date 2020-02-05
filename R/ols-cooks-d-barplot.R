#' Cooks' D bar plot
#'
#' @description
#' Bar Plot of cook's distance to detect observations that strongly influence
#' fitted values of the model.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' Cook's distance was introduced by American statistician R Dennis Cook in
#' 1977. It is used to identify influential data points. It depends on both the
#' residual and leverage i.e it takes it account both the \emph{x} value and
#' \emph{y} value of the observation.
#'
#' Steps to compute Cook's distance:
#'
#' \itemize{
#'   \item Delete observations one at a time.
#'   \item Refit the regression model on remaining \eqn{n - 1} observations
#'   \item examine how much all of the fitted values change when the ith observation is deleted.
#' }
#'
#' A data point having a large cook's d indicates that the data point strongly influences the fitted values.
#'
#' @return \code{ols_plot_cooksd_bar} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a \code{data.frame} with observation number and \code{cooks distance} that exceed \code{threshold}}
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @section Deprecated Function:
#' \code{ols_cooksd_barplot()} has been deprecated. Instead use \code{ols_plot_cooksd_bar()}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_cooksd_bar(model)
#'
#' @importFrom magrittr set_colnames
#' @importFrom stats cooks.distance
#' @importFrom dplyr filter select
#' @importFrom ggplot2 geom_bar coord_flip ylim geom_hline geom_label
#'
#' @seealso [ols_plot_cooksd_chart()]
#'
#' @export
#'
ols_plot_cooksd_bar <- function(model, print_plot = TRUE) {

  check_model(model)

  fct_color <- NULL
  obs       <- NULL
  txt       <- NULL
  cd        <- NULL

  k <- ols_prep_cdplot_data(model)
  d <- ols_prep_outlier_obs(k)
  f <- ols_prep_cdplot_outliers(k)

  p <- 
    ggplot(d, aes(x = obs, y = cd, label = txt)) +
    geom_bar(width = 0.5, stat = "identity", aes(fill = fct_color)) +
    scale_fill_manual(values = c("blue", "red")) + labs(fill = "Observation") +
    ylim(0, k$maxx) + ylab("Cook's D") + xlab("Observation") +
    ggtitle("Cook's D Bar Plot") + geom_hline(yintercept = 0) +
    geom_hline(yintercept = k$ts, colour = "red") +
    geom_text(hjust = -0.2, nudge_x = 0.05, size = 2, na.rm = TRUE) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste("Threshold:", round(k$ts, 3))
    )

  if (print_plot) {
    suppressWarnings(print(p))
  } else {
    return(list(plot = p, outliers = f, threshold = k$ts))
  }

}

#' @export
#' @rdname ols_plot_cooksd_bar
#' @usage NULL
#'
ols_cooksd_barplot <- function(model) {
  .Deprecated("ols_plot_cooksd_bar()")
}

