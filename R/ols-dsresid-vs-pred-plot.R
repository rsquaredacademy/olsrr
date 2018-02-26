#' Deleted studentized residual vs fitted values plot
#'
#' @description
#' Plot for detecting violation of assumptions about residuals such as
#' non-linearity, constant variances and outliers. It can also be used to
#' examine model fit.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' Studentized deleted residuals (or externally studentized residuals) is the
#' deleted residual divided by its estimated standard deviation. Studentized
#' residuals are going to be more effective for detecting outlying Y
#' observations than standardized residuals. If an observation has an externally
#' studentized residual that is larger than 2 (in absolute value) we can call
#' it an outlier.
#'
#' @return \code{ols_dsrvsp_plot} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a tibble with observation number, fitted values and deleted studentized
#' residuals that exceed the \code{threshold} for classifying observations as
#' outliers/influential observations}
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier/influential observation}
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_dsrvsp_plot(model)
#'
#' @seealso [ols_rsdlev_plot()], [ols_srsd_plot()], [ols_srsd_chart()]
#'
#' @importFrom stats fitted rstudent
#' @importFrom dplyr mutate
#'
#' @export
#'
ols_dsrvsp_plot <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  fct_color <- NULL
  color     <- NULL
  pred      <- NULL
  dsr       <- NULL
  txt       <- NULL
  obs       <- NULL
  ds        <- NULL

  k <- dpred(model)

  d <-
    k %>%
    use_series(ds) %>%
    mutate(
      txt = ifelse(color == "outlier", obs, NA)
    )

  f <-
    d %>%
    filter(color == "outlier") %>%
    select(obs, pred, dsr) %>%
    set_colnames(c("Observation", "Fitted Values", "Deleted Studentized Residual"))

  p <- ggplot(d, aes(x = pred, y = dsr, label = txt)) +
    geom_point(aes(colour = fct_color)) +
    scale_color_manual(values = c("blue", "red")) +
    ylim(k$cminx, k$cmaxx) + xlab("Predicted Value") +
    ylab("Deleted Studentized Residual") + labs(color = "Observation") +
    ggtitle("Deleted Studentized Residual vs Predicted Values") +
    geom_hline(yintercept = c(-2, 2), colour = "red") +
    geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family = "serif",
              fontface = "italic", colour = "darkred", na.rm = TRUE) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.5, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste0("Threshold: abs(", 2, ")")
    )

  suppressWarnings(print(p))
  result <- list(outliers = f, threshold = 2, plot = p)
  invisible(result)

}

#' @importFrom magrittr %<>%
dpred <- function(model) {

  dsr   <- NULL
  color <- NULL
  pred  <- fitted(model)

  dsresid <-
    model %>%
    rstudent() %>%
    unname()

  n  <- length(dsresid)
  ds <- tibble(obs = seq_len(n), dsr = dsresid)

  ds %<>%
    mutate(
      color = ifelse((abs(dsr) >= 2), "outlier", "normal"),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier"))
    )

  ds2 <- tibble(obs       = seq_len(n),
                pred      = pred,
                dsr       = ds$dsr,
                color     = ds$color,
                fct_color = ds$fct_color)

  minx <-
    ds2 %>%
    use_series(dsr) %>%
    min() %>%
    subtract(1)

  maxx <-
    ds2 %>%
    use_series(dsr) %>%
    max() %>%
    add(1)

  cminx <- ifelse(minx < -2, minx, -2.5)
  cmaxx <- ifelse(maxx > 2, maxx, 2.5)

  list(ds = ds2, cminx = cminx, cmaxx = cmaxx)

}
