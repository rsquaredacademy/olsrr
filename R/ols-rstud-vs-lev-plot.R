#' Studentized residuals vs leverage plot
#'
#' Graph for detecting outliers and/or observations with high leverage.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @section Deprecated Function:
#' \code{ols_rsdlev_plot()} has been deprecated. Instead use \code{ols_plot_resid_lev()}.
#'
#' @examples
#' model <- lm(read ~ write + math + science, data = hsb)
#' ols_plot_resid_lev(model)
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 geom_vline
#'
#' @seealso [ols_plot_resid_stud_fit()], [ols_plot_resid_lev()]
#'
#' @export
#'
ols_plot_resid_lev <- function(model, print_plot = TRUE) {

  check_model(model)

  lev_thrsh <- NULL
  fct_color <- NULL
  leverage  <- NULL
  levrstud  <- NULL
  txt       <- NULL
  obs       <- NULL
  color     <- NULL

  resp <-
    model %>%
    model.frame() %>%
    names() %>%
    extract(1)

  title <- paste("Outlier and Leverage Diagnostics for", resp)
  g     <- ols_prep_rstudlev_data(model)

  ann_paste <-
    g %>%
    use_series(lev_thrsh) %>%
    round(3)

  ann_label <- paste("Threshold:", ann_paste)

  d <-
    g %>%
    use_series(levrstud) %>%
    mutate(
      txt = ifelse(color == "normal", NA, obs)
    )

  f <-
    d %>%
    filter(color == "outlier") %>%
    select(obs, leverage, rstudent) %>%
    set_colnames(c("observation", "leverage", "stud_resid"))

  p <-
    ggplot(d, aes(leverage, rstudent, label = txt)) +
    geom_point(shape = 1, aes(colour = fct_color)) +
    scale_color_manual(values = c("blue", "red", "green", "violet")) +
    xlim(g$minx, g$maxx) + ylim(g$miny, g$maxy) + labs(colour = "Observation") +
    xlab("Leverage") + ylab("RStudent") + ggtitle(title) +
    geom_hline(yintercept = c(2, -2), colour = "maroon") +
    geom_vline(xintercept = g$lev_thrsh, colour = "maroon") +
    geom_text(vjust = -1, size = 3, family = "serif", fontface = "italic",
              colour = "darkred") +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = ann_label)

  if (print_plot) {
    suppressWarnings(print(p))
  } else {
    result(
      list(plot      = p,
           leverage  = f,
           threshold = g$lev_thrsh)
      )
  }

}

#' @export
#' @rdname ols_plot_resid_lev
#' @usage NULL
#'
ols_rsdlev_plot <- function(model) {
  .Deprecated("ols_plot_resid_lev()")
}
