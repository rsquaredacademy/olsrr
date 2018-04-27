#' Standardized residual chart
#'
#' Chart for identifying outliers.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' Standardized residual (internally studentized) is the residual divided by
#' estimated standard deviation.
#'
#' @return \code{ols_plot_resid_stand} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a tibble with observation number and \code{standardized resiudals} that
#' exceed \code{threshold}} for classifying an observation as an outlier
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @section Deprecated Function:
#' \code{ols_srsd_chart()} has been deprecated. Instead use \code{ols_plot_resid_stand()}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_resid_stand(model)
#'
#' @importFrom stats rstandard
#'
#' @seealso [ols_plot_resid_stud()]
#'
#' @export
#'
ols_plot_resid_stand <- function(model) {

  check_model(model)

  color <- NULL
  obs   <- NULL
  sdres <- NULL
  txt   <- NULL

  d <- srchart_data(model)

  f <-
    d %>%
    filter(color == "outlier") %>%
    select(obs, sdres) %>%
    set_colnames(c("observation", "stand_resid"))

  p <- ggplot(d, aes(x = obs, y = sdres, label = txt, ymin = 0, ymax = sdres)) +
    geom_linerange(colour = "blue") + geom_point(shape = 1, colour = "blue") +
    geom_hline(yintercept = 0, colour = "gray") +
    geom_hline(yintercept = c(2, -2), colour = "red") +
    xlab("Observation") + ylab("Standardized Residuals") +
    ggtitle("Standardized Residuals Chart") +
    geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family = "serif",
              fontface = "italic", colour = "darkred", na.rm = TRUE) +
    annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2, family = "serif",
             fontface = "italic", colour = "darkred",
             label = paste0("Threshold: abs(", 2, ")"))

  suppressWarnings(print(p))
  result <- list(outliers = f, threshold = 2, plot = p)
  invisible(result)

}

#' @importFrom magrittr is_greater_than
srchart_data <- function(model) {

  color <- NULL

  sdres <- rstandard(model)

  sdres_out <-
    sdres %>%
    abs() %>%
    is_greater_than(2)

  outlier <-
    sdres %>%
    extract(sdres_out)

  obs <-
    sdres %>%
    length() %>%
    seq_len()

  tibble(obs = obs, sdres = sdres) %>%
    mutate(
      color = ifelse(((sdres >= 2) | (sdres <= -2)), c("outlier"), c("normal")),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier")),
      txt = ifelse(color == "outlier", obs, NA)
    )

}


#' @export
#' @rdname ols_plot_resid_stand
#' @usage NULL
#'
ols_srsd_chart <- function(model) {
  .Deprecated("ols_plot_resid_stand()")
}
