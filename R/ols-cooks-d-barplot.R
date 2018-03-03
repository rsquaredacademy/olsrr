#' Cooks' D bar plot
#'
#' @description
#' Bar Plot of cook's distance to detect observations that strongly influence
#' fitted values of the model.
#'
#' @param model An object of class \code{lm}.
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
#' \item{outliers}{a tibble with observation number and \code{cooks distance} that exceed \code{threshold}}
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
ols_plot_cooksd_bar <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  fct_color <- NULL
  obs       <- NULL
  txt       <- NULL
  cd        <- NULL

  k <- cdplot(model)
  d <- plot_data(k)
  f <- outlier_data(k)

  p <- ggplot(d, aes(x = obs, y = cd, label = txt)) +
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

  suppressWarnings(print(p))
  result <- list(outliers = f, threshold = k$ts, plot = p)
  invisible(result)

}

#' @export
#' @rdname ols_plot_cooksd_bar
#' @usage NULL
#'
ols_cooksd_barplot <- function(model) {
  .Deprecated("ols_plot_cooksd_bar()")
}

#' @description Prepare data for cook's d bar plot
#'
#' @importFrom dplyr if_else
#'
#' @noRd
#'
cdplot <- function(model) {

  cd        <- NULL
  color     <- NULL
  cooksd    <- cooks.distance(model)
  n         <- length(cooksd)
  obs       <- seq_len(n)
  ckd       <- tibble(obs = obs, cd = cooksd)
  ts        <- 4 / n
  cooks_max <- max(cooksd)

  ckd %<>%
    mutate(
      color = if_else(cd >= ts, "outlier", "normal"),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier"))
    )

  maxx <-
    cooks_max %>%
    multiply_by(0.01) %>%
    add(cooks_max)

  list(ckd = ckd, maxx = maxx, ts = ts)

}

#' @description Data for identifying outliers in cook's d bar plot
#'
#' @noRd
#'
plot_data <- function(k) {

  ckd   <- NULL
  color <- NULL
  obs   <- NULL

  k %>%
    use_series(ckd) %>%
    mutate(
      txt = ifelse(color == "outlier", obs, NA)
    )

}

#' @description Outlier data for cook's d bar plot
#'
#' @noRd
#'
outlier_data <- function(k) {

  color <- NULL
  ckd   <- NULL
  obs   <- NULL
  cd    <- NULL

  k %>%
    use_series(ckd) %>%
    filter(
      color == "outlier"
    ) %>%
    select(obs, cd) %>%
    set_colnames(c("Observation", "Cook's Distance"))

}
