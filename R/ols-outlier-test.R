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
#' @importFrom stats df.residual pt
#' @export
#'
ols_test_outlier <- function(model, cut_off = 0.05, n_max = 10, ...) {

  stud_resid <-
    model %>%
    rstudent()

  df_resid <-
    model %>%
    df.residual() %>%
    subtract(1)

  n  <- length(stud_resid)

  p_val  <-
    stud_resid %>%
    abs() %>%
    pt(df = df_resid, lower.tail = FALSE) %>%
    multiply_by(2)

  p_bon <- n * p_val

  data_bon <-
    data.frame(stud_resid, p_val, p_bon) %>%
    tidyr::drop_na()

  data_co <-
    data_bon %>%
    filter(p_bon <= cut_off) %>%
    arrange(desc(p_bon))

  nf <- nrow(data_co)

  if (nf == 0) {
    out <-
      data_bon %>%
      filter(stud_resid == max(stud_resid))
  } else {
    if (nf < n_max) {
      out <- data_co
    } else {
      out <-
        data_co %>%
        dplyr::slice(1:n_max)
    }
  }

  out %>%
    dplyr::rename(studentized_residual = stud_resid,
           unadjusted_p_val     = p_val,
           bonferroni_p_val     = p_bon)

}

