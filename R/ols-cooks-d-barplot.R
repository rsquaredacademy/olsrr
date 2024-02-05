#' Cooks' D bar plot
#'
#' @description
#' Bar Plot of cook's distance to detect observations that strongly influence
#' fitted values of the model.
#'
#' @param model An object of class \code{lm}.
#' @param type An integer between 1 and 5 selecting one of the 5 methods for
#'     computing the threshold.
#' @param threshold Threshold for detecting outliers.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a
#'     plot object.
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
#' A data point having a large cook's d indicates that the data point strongly
#' influences the fitted values. There are several methods/formulas to compute
#' the threshold used for detecting or classifying observations as outliers and
#' we list them below.
#'
#' \itemize{
#'   \item \strong{Type 1} : 4 / n
#'   \item \strong{Type 2} : 4 / (n - k - 1)
#'   \item \strong{Type 3} : ~1
#'   \item \strong{Type 4} : 1 / (n - k - 1)
#'   \item \strong{Type 5} : 3 * mean(Vector of cook's distance values)
#' }
#'
#' where \strong{n} and \strong{k} stand for
#'
#' \itemize{
#'   \item \strong{n}: Number of observations
#'   \item \strong{k}: Number of predictors
#' }
#'
#' @return \code{ols_plot_cooksd_bar} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a \code{data.frame} with observation number and \code{cooks distance} that exceed \code{threshold}}
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_cooksd_bar(model)
#' ols_plot_cooksd_bar(model, type = 4)
#' ols_plot_cooksd_bar(model, threshold = 0.2)
#'
#' @importFrom stats cooks.distance
#' @importFrom ggplot2 geom_bar coord_flip ylim geom_hline geom_label
#'
#' @seealso [ols_plot_cooksd_chart()]
#'
#' @export
#'
ols_plot_cooksd_bar <- function(model, type = 1, threshold = NULL, print_plot = TRUE) {

  check_model(model)

  k <- ols_prep_cdplot_data(model, type)
  d <- ols_prep_outlier_obs(k)
  f <- ols_prep_cdplot_outliers(k)
  
  if (is.null(threshold)) {
      threshold <- k$ts
  }

  y_max <- max(k$maxx, threshold)
  y_lim <- y_max + (y_max * 0.1)

  # geoms
  p <-
    ggplot(d, aes(x = obs, y = cd, label = txt)) +
    geom_bar(width = 0.5, stat = "identity", aes(fill = fct_color)) +
    geom_text(hjust = -0.2, nudge_x = 0.05, size = 2, na.rm = TRUE) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = threshold, colour = "red")

  # annotations
  p <-
    p +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste("Threshold:", round(threshold, 3)))

  # scales
  p <-
    p +
    scale_fill_manual(values = c("blue", "red"))

  # guides
  p <-
    p +
    labs(fill = "Observation") +
    xlab("Observation") +
    ylab("Cook's D") +
    ggtitle("Cook's D Bar Plot") +
    ylim(0, y_lim)

  if (print_plot) {
    suppressWarnings(print(p))
  } else {
    return(list(plot = p, outliers = f, threshold = threshold))
  }

}
