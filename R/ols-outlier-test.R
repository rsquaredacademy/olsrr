#' Bonferroni Outlier Test
#'
#' Detect outliers using Bonferroni p values.
#'
#' @param model An object of class \code{lm}.
#' @param cut_off Bonferroni p-values cut off for reporting observations.
#' @param n_max Maximum number of observations to report, default is 10.
#' @param ... Other arguments.
#'
#' @examples
#' # model
#' model <- lm(y ~ ., data = surgical)
#' ols_test_outlier(model)
#'
#' @importFrom stats df.residual pt na.omit
#' @export
#'
ols_test_outlier <- function(model, cut_off = 0.05, n_max = 10, ...) {

  stud_resid <- rstudent(model)
  df_resid   <- df.residual(model) - 1
  n          <- length(stud_resid)
  p_val      <- pt(q = abs(stud_resid), df = df_resid, lower.tail = FALSE) * 2
  p_bon      <- n * p_val
  data_bon   <- na.omit(data.frame(stud_resid, p_val, p_bon))
  data_bonf  <- data_bon[data_bon$p_bon <= cut_off, ]
  data_co    <- data_bonf[order(-data_bonf$p_bon), ]
  nf         <- nrow(data_co)

  if (nf == 0) {
    out <- data_bon[data_bon$stud_resid == max(data_bon$stud_resid), ]
  } else {
    if (nf < n_max) {
      out <- data_co
    } else {
      out <- data_co[1:n_max, ]
    }
  }

  out_names <- c("studentized_residual", "unadjusted_p_val", "bonferroni_p_val")
  colnames(out) <- out_names
  return(out)

}

