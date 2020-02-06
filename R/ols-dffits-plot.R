#' DFFITS plot
#'
#' Plot for detecting influential observations using DFFITs.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' DFFIT - difference in fits, is used to identify influential data points. It
#' quantifies the number of standard deviations that the fitted value changes
#' when the ith data point is omitted.
#'
#' Steps to compute DFFITs:
#'
#' \itemize{
#'   \item Delete observations one at a time.
#'   \item Refit the regression model on remaining \eqn{n - 1} observations
#'   \item examine how much all of the fitted values change when the ith observation is deleted.
#' }
#'
#' An observation is deemed influential if the absolute value of its DFFITS value is greater than:
#' \deqn{2\sqrt(p + 1) / (n - p -1)}
#'
#' where n is the number of observations and p is the number of predictors including intercept.
#'
#' @return \code{ols_plot_dffits} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a \code{data.frame} with observation number and \code{DFFITs} that exceed \code{threshold}}
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @references
#' Belsley, David A.; Kuh, Edwin; Welsh, Roy E. (1980). Regression
#' Diagnostics: Identifying Influential Data and Sources of Collinearity.
#'
#' Wiley Series in Probability and Mathematical Statistics.
#' New York: John Wiley & Sons. ISBN 0-471-05856-4.
#'
#' @section Deprecated Function:
#' \code{ols_dffits_plot()} has been deprecated. Instead use \code{ols_plot_dffits()}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_plot_dffits(model)
#'
#' @seealso [ols_plot_dfbetas()]
#'
#' @importFrom stats dffits
#'
#' @export
#'
ols_plot_dffits <- function(model, print_plot = TRUE) {

  check_model(model)

  dbetas     <- NULL
  obs        <- NULL
  txt        <- NULL
  dffitsm    <- unlist(dffits(model))
  k          <- model_n_coeffs(model)
  n          <- model_rows(model)
  dffits_t   <- sqrt(k / n) * 2
  title      <- names(model.frame(model))[1]
  dfits_data <- data.frame(obs = seq_len(n), dbetas = dffitsm)
  d          <- ols_prep_dfbeta_data(dfits_data, dffits_t)
  f          <- ols_prep_dfbeta_outliers(d)

  p <- ggplot(d, aes(x = obs, y = dbetas, label = txt, ymin = 0, ymax = dffitsm)) +
    geom_linerange(colour = "blue") +
    geom_hline(yintercept = c(0, dffits_t, -dffits_t), colour = "red") +
    geom_point(colour = "blue", shape = 1) +
    xlab("Observation") + ylab("DFFITS") +
    ggtitle(paste("Influence Diagnostics for", title)) +
    geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family = "serif",
              fontface = "italic", colour = "darkred", na.rm = TRUE) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.5, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste("Threshold:", round(dffits_t, 2))
    )

  if (print_plot) {
    suppressWarnings(print(p))
  } else {
    colnames(f) <- c("observation", "dffits")
    return(list(plot = p, outliers = f, threshold = round(dffits_t, 2)))
  }

}


#' @export
#' @rdname ols_plot_dffits
#' @usage NULL
#'
ols_dffits_plot <- function(model) {
  .Deprecated("ols_plot_dffits()")
}
