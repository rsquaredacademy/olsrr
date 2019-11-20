#' Studentized residual plot
#'
#' Graph for identifying outliers.
#'
#' @param model An object of class \code{lm}.
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
#' \item{outliers}{a tibble with observation number and \code{studentized residuals} that
#' exceed \code{threshold}} for classifying an observation as an outlier
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @section Deprecated Function:
#' \code{ols_srsd_plot()} has been deprecated. Instead use \code{ols_plot_resid_stud()}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_resid_stud(model)
#'
#' @importFrom ggplot2 scale_fill_manual annotate
#'
#' @seealso [ols_plot_resid_stand()]
#'
#' @export
#'
ols_plot_resid_stud <- function(model, print_plot = TRUE) {

  check_model(model)

  fct_color <- NULL
  color     <- NULL
  obs       <- NULL
  dsr       <- NULL
  txt       <- NULL

  g <- ols_prep_srplot_data(model)

  d <-
    g %>%
    use_series(dsr) %>%
    mutate(
      txt = ifelse(color == "outlier", obs, NA)
    )

  f <-
    d %>%
    filter(color == "outlier") %>%
    select(obs, dsr) %>%
    set_colnames(c("observation", "stud_resid"))

  p <-
    ggplot(d, aes(x = obs, y = dsr, label = txt)) +
    geom_bar(width = 0.5, stat = "identity", aes(fill = fct_color)) +
    scale_fill_manual(values = c("blue", "red")) + xlab("Observation") +
    ylab("Deleted Studentized Residuals") + labs(fill = "Observation") +
    ggtitle("Studentized Residuals Plot") + ylim(g$cminx, g$cmaxx) +
    geom_hline(yintercept = c(0, g$nseq, g$pseq)) +
    geom_hline(yintercept = c(-3, 3), color = "red") +
    geom_text(hjust = -0.2, nudge_x = 0.05, size = 2, na.rm = TRUE) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste0("Threshold: abs(", 3, ")")
    )

  if (print_plot) {
    suppressWarnings(print(p))
  } else {
    return(
      list(plot = p,
           outliers = f,
           threshold = 3)
      )
  }

}

#' @export
#' @rdname ols_plot_resid_stud
#' @usage NULL
#'
ols_srsd_plot <- function(model) {
  .Deprecated("ols_plot_resid_stud()")
}
