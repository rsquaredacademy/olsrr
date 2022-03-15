#' Studentized residuals vs leverage plot
#'
#' Graph for detecting outliers and/or observations with high leverage.
#'
#' @param model An object of class \code{lm}.
#' @param threshold Threshold for detecting outliers. Default is 2.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @examples
#' model <- lm(read ~ write + math + science, data = hsb)
#' ols_plot_resid_lev(model)
#' ols_plot_resid_lev(model, threshold = 3)
#'
#' @importFrom ggplot2 geom_vline
#'
#' @seealso [ols_plot_resid_stud_fit()], [ols_plot_resid_lev()]
#'
#' @export
#'
ols_plot_resid_lev <- function(model, threshold = NULL, print_plot = TRUE) {

  check_model(model)

  if (is.null(threshold)) {
    threshold <- 2
  }

  data <- ols_prep_residlev_data(model, threshold)

  p <-
    ggplot(data$d, aes(leverage, rstudent, label = txt)) +
    geom_point(shape = 1, aes(colour = fct_color)) +
    geom_text(data = data$d2, vjust = -1, size = 3, family = "serif",
              fontface = "italic", colour = "darkred") +
    geom_hline(yintercept = c(threshold, -threshold), colour = "maroon") +
    geom_vline(xintercept = data$g$lev_thrsh, colour = "maroon")

  p <-
    p +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
             family = "serif", fontface = "italic", colour = "darkred",
             label = data$ann_lev) +
    annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 4.5,
             family = "serif", fontface = "italic", colour = "darkred",
             label = data$ann_out)

  p <-
    p +
    scale_color_manual(values = c("blue", "red", "green", "violet"))

  p <-
    p +
    labs(colour = "Observation") +
    xlab("Leverage") +
    ylab("RStudent") +
    ggtitle(data$title) +
    xlim(data$g$minx, data$g$maxx) +
    ylim(data$g$miny, data$g$maxy)

  if (print_plot) {
    suppressWarnings(print(p))
  } else {
    return(list(plot = p, leverage  = data$f, threshold = data$g$lev_thrsh))
  }

}

ols_prep_residlev_data <- function(model, threshold) {
  resp        <- names(model.frame(model))[1]
  title       <- paste("Outlier and Leverage Diagnostics for", resp)
  g           <- ols_prep_rstudlev_data(model, threshold)
  ann_paste   <- round(g$lev_thrsh, 3)
  ann_lev     <- paste("Leverage Threshold:", ann_paste)
  ann_out     <- paste("Outlier Threshold:", threshold)
  d           <- g$levrstud
  d$txt       <- ifelse(d$color == "normal", NA, d$obs)
  f           <- d[d$color == "outlier", c("obs", "leverage", "rstudent")]
  colnames(f) <- c("observation", "leverage", "stud_resid")
  d2          <- d[!is.na(d$txt), ]

  list(d = d, d2 = d2, f = f, g = g, title = title, ann_lev = ann_lev, ann_out = ann_out)
}