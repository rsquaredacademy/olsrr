#' Standardized residual chart
#'
#' Chart for identifying outliers.
#'
#' @param model An object of class \code{lm}.
#' @param threshold Threshold for detecting outliers. Default is 2.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' Standardized residual (internally studentized) is the residual divided by
#' estimated standard deviation.
#'
#' @return \code{ols_plot_resid_stand} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a \code{data.frame} with observation number and \code{standardized resiudals} that
#' exceed \code{threshold}} for classifying an observation as an outlier
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_resid_stand(model)
#' ols_plot_resid_stand(model, threshold = 3)
#'
#' @importFrom stats rstandard
#'
#' @seealso [ols_plot_resid_stud()]
#'
#' @export
#'
ols_plot_resid_stand <- function(model, threshold = NULL, print_plot = TRUE) {

  check_model(model)

  if (is.null(threshold)) {
    threshold <- 2
  }

  d <- ols_prep_srchart_data(model, threshold)
  f <- d[color == "outlier", c("obs", "sdres")]
  colnames(f) <- c("observation", "stand_resid")

  p <-
    ggplot(d, aes(x = obs, y = sdres, label = txt, ymin = 0, ymax = sdres)) +
    geom_linerange(colour = "blue") + 
    geom_point(shape = 1, colour = "blue") +
    geom_hline(yintercept = 0, colour = "gray") +
    geom_hline(yintercept = c(threshold, -threshold), colour = "red") +
    xlab("Observation") + 
    ylab("Standardized Residuals") +
    ggtitle("Standardized Residuals Chart") +
    geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family = "serif",
              fontface = "italic", colour = "darkred", na.rm = TRUE) +
    annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2, family = "serif",
             fontface = "italic", colour = "darkred",
             label = paste0("Threshold: abs(", threshold, ")"))

  if (print_plot) {
    suppressWarnings(print(p))
  } else {
    return(
      list(plot      = p,
           outliers  = f,
           threshold = threshold)
      )
  }

}