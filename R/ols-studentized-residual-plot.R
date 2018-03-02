#' Studentized residual plot
#'
#' Graph for identifying outliers.
#'
#' @param model An object of class \code{lm}.
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
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_resid_stud(model)
#'
#' @importFrom ggplot2 scale_fill_manual annotate
#'
#' @seealso [ols_srsd_chart()]
#'
#' @export
#'
ols_plot_resid_stud <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  fct_color <- NULL
  color     <- NULL
  obs       <- NULL
  dsr       <- NULL
  txt       <- NULL

  g <- srdata(model)

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
    set_colnames(c("Observation", "Studentized Residuals"))

  p <- ggplot(d, aes(x = obs, y = dsr, label = txt)) +
    geom_bar(width = 0.5, stat = "identity", aes(fill = fct_color)) +
    scale_fill_manual(values = c("blue", "red")) + xlab("Observation") +
    ylab("Deleted Studentized Residuals") + ggtitle("Studentized Residuals Plot") +
    ylim(g$cminx, g$cmaxx) + geom_hline(yintercept = c(0, g$nseq, g$pseq)) +
    geom_hline(yintercept = c(-3, 3), color = "red") + labs("Observation") +
    geom_text(hjust = -0.2, nudge_x = 0.05, size = 2, na.rm = TRUE) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste0("Threshold: abs(", 3, ")")
    )

  suppressWarnings(print(p))
  result <- list(outliers = f, threshold = 3, plot = p)
  invisible(result)

}

srdata <- function(model) {

  color <- NULL

  dstud <-
    model %>%
    rstudent() %>%
    unname()

  obs <-
    dstud %>%
    length() %>%
    seq_len()

  dsr <-
    tibble(obs = obs, dsr = dstud) %>%
    mutate(
      color = ifelse((abs(dsr) >= 3), "outlier", "normal"),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier"))
    )

  cminxx <-
    dsr %>%
    use_series(dsr) %>%
    min() %>%
    subtract(1) %>%
    floor()

  cmaxxx <-
    dsr %>%
    use_series(dsr) %>%
    max() %>%
    subtract(1) %>%
    floor()

  cminx <- ifelse(cminxx > -3, -3, cminxx)
  cmaxx <- ifelse(cmaxxx < 3, 3, cmaxxx)

  nseq <-
    0 %>%
    add(cminx) %>%
    add(1) %>%
    abs() %>%
    seq_len() %>%
    multiply_by(-1)

  pseq <-
    0 %>%
    add(cmaxx) %>%
    subtract(1) %>%
    seq_len()

  list(cminx = cminx,
       cmaxx = cmaxx,
       nseq  = nseq,
       pseq  = pseq,
       dsr   = dsr)

}

#' @export
#' @rdname ols_plot_resid_stud
#' @usage NULL
#'
ols_srsd_plot <- function(model) {
  .Deprecated("ols_plot_resid_stud()")
}
