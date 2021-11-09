#' Studentized residual plot
#'
#' Graph for identifying outliers.
#'
#' @param model An object of class \code{lm}.
#' @param threshold Threshold for detecting outliers. Default is 3.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' Studentized deleted residuals (or externally studentized residuals) is the
#' deleted residual divided by its estimated standard deviation. Studentized
#' residuals are going to be more effective for detecting outlying Y
#' observations than standardized residuals. If an observation has an externally
#' studentized residual that is larger than 3 (in absolute value) we can call
#' it an outlier.
#'
#' @return \code{ols_plot_resid_stud} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a \code{data.frame} with observation number and \code{studentized residuals} that
#' exceed \code{threshold}} for classifying an observation as an outlier
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_resid_stud(model)
#' ols_plot_resid_stud(model, threshold = 2)
#'
#' @importFrom ggplot2 scale_fill_manual annotate
#'
#' @seealso [ols_plot_resid_stand()]
#'
#' @export
#'
ols_plot_resid_stud <- function(model, threshold = NULL, print_plot = TRUE) {

  check_model(model)

  fct_color <- NULL
  color     <- NULL
  obs       <- NULL
  dsr       <- NULL
  txt       <- NULL

  if (is.null(threshold)) {
    threshold <- 3
  }

  g           <- ols_prep_srplot_data(model, threshold)
  d           <- g$dsr
  d$txt       <- ifelse(d$color == "outlier", d$obs, NA)
  f           <- d[color == "outlier", c("obs", "dsr")] 
  colnames(f) <- c("observation", "stud_resid")

  p <-
    ggplot(d, aes(x = obs, y = dsr, label = txt)) +
    geom_bar(width = 0.5, stat = "identity", aes(fill = fct_color)) +
    scale_fill_manual(values = c("blue", "red")) + 
    xlab("Observation") +
    ylab("Deleted Studentized Residuals") + 
    labs(fill = "Observation") +
    ggtitle("Studentized Residuals Plot") + 
    ylim(g$cminx, g$cmaxx) +
    geom_hline(yintercept = c(0, g$nseq, g$pseq)) +
    geom_hline(yintercept = c(-threshold, threshold), color = "red") +
    geom_text(hjust = -0.2, nudge_x = 0.05, size = 2, na.rm = TRUE) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste0("Threshold: abs(", threshold, ")")
    )

  if (print_plot) {
    suppressWarnings(print(p))
  } else {
    return(
      list(plot = p,
           outliers = f,
           threshold = threshold)
      )
  }

}
