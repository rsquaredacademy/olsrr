#' @importFrom stats cooks.distance
#' @importFrom dplyr filter select
#' @importFrom ggplot2 geom_bar coord_flip ylim geom_hline geom_label
#' @title Cooks' D Bar Plot
#' @description Bar Plot of cook's distance to detect observations that strongly influence fitted values of the model.
#' @param model an object of class \code{lm}
#' @details Cook's distance was introduced by American statistician R Dennis Cook in 1977. It is used
#' to identify influential data points. It depends on both the residual and leverage i.e it takes it account
#' both the \emph{x} value and \emph{y} value of the observation.
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
#' @return \code{ols_cooksd_barplot} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a tibble with observation number and \code{cooks distance} that exceed \code{threshold}}
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_cooksd_barplot(model)
#' @export
#'
ols_cooksd_barplot <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  obs <- NULL
  txt <- NULL
  cd <- NULL
  Observation <- NULL
  k <- cdplot(model)
  d <- k$ckd
  d <- d %>% mutate(txt = ifelse(Observation == "outlier", obs, NA))
  f <- d %>% filter(., Observation == "outlier") %>% select(obs, cd)

  p <- ggplot(d, aes(x = obs, y = cd, label = txt)) +
    geom_bar(width = 0.5, stat = "identity", aes(fill = Observation)) +
    scale_fill_manual(values = c("blue", "red")) +
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
  colnames(f) <- c("Observation", "Cook's Distance")
  result <- list(outliers = f, threshold = k$ts, plot = p)
  invisible(result)

}

cdplot <- function(model) {
  cooksd <- cooks.distance(model)
  n <- length(cooksd)
  ckd <- tibble(obs = seq_len(n), cd = cooksd)
  ts <- 4 / length(ckd$cd)
  ckd$color <- ifelse(ckd$cd >= ts, c("outlier"), c("normal"))
  ckd$color1 <- factor(ckd$color)
  ckd$Observation <- ordered(ckd$color1, levels = c("normal", "outlier"))
  ts <- 4 / length(ckd$cd)
  maxx <- max(ckd$cd) + max(ckd$cd) * 0.01
  result <- list(ckd = ckd, maxx = maxx, ts = ts)
  return(result)
}
