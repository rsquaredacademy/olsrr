#' Deleted studentized residual vs fitted values plot
#'
#' @description
#' Plot for detecting violation of assumptions about residuals such as
#' non-linearity, constant variances and outliers. It can also be used to
#' examine model fit.
#'
#' @param model An object of class \code{lm}.
#' @param threshold Threshold for detecting outliers. Default is 2.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' Studentized deleted residuals (or externally studentized residuals) is the
#' deleted residual divided by its estimated standard deviation. Studentized
#' residuals are going to be more effective for detecting outlying Y
#' observations than standardized residuals. If an observation has an externally
#' studentized residual that is larger than 2 (in absolute value) we can call
#' it an outlier.
#'
#' @return \code{ols_plot_resid_stud_fit} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a \code{data.frame} with observation number, fitted values and deleted studentized
#' residuals that exceed the \code{threshold} for classifying observations as
#' outliers/influential observations}
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier/influential observation}
#'
#' @section Deprecated Function:
#' \code{ols_dsrvsp_plot()} has been deprecated. Instead use \code{ols_plot_resid_stud_fit()}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_plot_resid_stud_fit(model)
#' ols_plot_resid_stud_fit(model, threshold = 3)
#'
#' @seealso [ols_plot_resid_lev()], [ols_plot_resid_stand()],
#'   [ols_plot_resid_stud()]
#'
#' @importFrom stats fitted rstudent
#'
#' @export
#'
ols_plot_resid_stud_fit <- function(model, threshold = NULL, print_plot = TRUE) {

  check_model(model)

  if (is.null(threshold)) {
    threshold <- 2
  }

  fct_color   <- NULL
  color       <- NULL
  pred        <- NULL
  dsr         <- NULL
  txt         <- NULL
  obs         <- NULL
  ds          <- NULL
  k           <- ols_prep_dsrvf_data(model, threshold)
  d           <- k$ds
  d$txt       <- ifelse(d$color == "outlier", d$obs, NA)
  f           <- d[color == "outlier", c("obs", "pred", "dsr")]
  colnames(f) <- c("observation", "fitted_values", "del_stud_resid")
    
  p <- ggplot(d, aes(x = pred, y = dsr, label = txt)) +
    geom_point(aes(colour = fct_color)) +
    scale_color_manual(values = c("blue", "red")) +
    ylim(k$cminx, k$cmaxx) + xlab("Predicted Value") +
    ylab("Deleted Studentized Residual") + labs(color = "Observation") +
    ggtitle("Deleted Studentized Residual vs Predicted Values") +
    geom_hline(yintercept = c(-threshold, threshold), colour = "red") +
    geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family = "serif",
              fontface = "italic", colour = "darkred", na.rm = TRUE) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.5, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste0("Threshold: abs(", threshold, ")")
    )

  if (print_plot) {
    suppressWarnings(print(p))
  } else {
    return(list(plot = p, outliers = f, threshold = threshold))
  }

}

#' @export
#' @rdname ols_plot_resid_stud_fit
#' @usage NULL
#'
ols_dsrvsp_plot <- function(model) {
  .Deprecated("ols_plot_resid_stud_fit()")
}



