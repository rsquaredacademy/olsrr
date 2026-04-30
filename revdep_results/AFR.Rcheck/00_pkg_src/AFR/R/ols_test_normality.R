#' Test for normality
#' Test for detecting violation of normality assumption.
#'
#' @param model an object of class \code{lm}.
#' @param ... Other arguments.
#'
#' @return \code{ols_test_normality} is a list containing the
#' following components:
#'
#' \item{kolmogorv}{kolmogorov smirnov statistic}
#' \item{shapiro}{shapiro wilk statistic}
#' \item{cramer}{cramer von mises statistic}
#' \item{anderson}{anderson darling statistic}
#'
#' @examples
#' data(macroKZ)
#' model <- lm(real_gdp ~ imp + exp + usdkzt + poil, data = macroKZ)
#' ols_test_normality(model)
#'
#' @import olsrr
#' @importFrom stats ks.test shapiro.test
#' @importFrom goftest cvm.test
#' @importFrom nortest ad.test
#'
#' @export
#'
ols_test_normality <- function(model, ...){

  if (!inherits(model, "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }
  residuals <- residuals(model)

  ks  <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
  sw  <- shapiro.test(residuals)
  cvm <- cvm.test(residuals)
  ad  <- ad.test(residuals)

  result <- list(kolmogorv = ks,
                 shapiro   = sw,
                 cramer    = cvm,
                 anderson  = ad)

  class(result) <- "ols_test_normality"

  print_norm_test(result)

  invisible(result)

}

print_norm_test <- function(data) {

  # width
  w1 <- 18
  w2 <- 14
  w3 <- 7
  w <- sum(w1, w2, w3, 8)

  # vectors
  tests <- c(
    "Shapiro-Wilk", "Kolmogorov-Smirnov", "Cramer-von Mises",
    "Anderson-Darling"
  )
  stats <- c(
    data$shapiro$statistic, data$kolmogorv$statistic,
    data$cramer$statistic, data$anderson$statistic
  )
  pvals <- c(
    data$shapiro$p.value, data$kolmogorv$p.value,
    data$cramer$p.value, data$anderson$p.value
  )
  n <- length(stats)

  # print

  cat(paste("Hint:", "If p-value > 0.05, data is normally distributed.", sep="\n", "\n"))

  cat(rep("-", w), sep = "", "\n")
  cat(
    format("Test", width = w1, justify = "centre"), fs(), format("Statistic", width = w2, justify = "centre"),
    fs(), format("pvalue", width = 7, justify = "centre"), "\n"
  )
  cat(rep("-", w), sep = "", "\n")
  for (i in seq_len(n)) {
    cat(
      format(tests[i], width = w1), fs(), format(as.character(round(stats[i], 4)), width = w2, justify = "centre"),
      fs(), format(round(pvals[i], 4), nsmall = 4, width = 7, justify = "centre"), "\n"
    )
  }
  cat(rep("-", w), sep = "", "\n")
}

