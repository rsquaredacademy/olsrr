#' Test for detecting violation of Gauss-Markov assumptions.
#'
#' @param y A numeric vector or an object of class \code{lm}.
#'
#' @return \code{reg_test} returns an object of class \code{"reg_test"}.
#' An object of class \code{"reg_test"} is a list containing the
#' following components:
#'
#' \item{bp}{Breusch-Pagan statistic}
#' \item{bg}{Breusch-Godfrey statistic}
#' \item{dw}{Durbin-Watson statistic}
#' \item{gq}{Godfrey-Quandt statistic}
#' @examples
#' data(macroKZ)
#' model <- lm(real_gdp~ imp + exp + poil + eurkzt + usdkzt, macroKZ)
#' reg_test(model)
#' @importFrom lmtest bgtest dwtest
#' @importFrom lmtest bptest gqtest
#' @importFrom cli console_width
#' @export
#'

reg_test <- function(y) {

  w1 <- 18
  w2 <- 14
  w3 <- 7
  w4 <- console_width()
  w <- sum(w1, w2, w3, 8)

  # vectors
  tests <- c( "Breusch-Pagan","Breusch-Godfrey",
              "Durbin-Watson", "Goldfeld-Quandt"
  )

  stats <- c(
    bptest(y)$statistic, bgtest(y)$statistic,
    dwtest(y)$statistic, gqtest(y)$statistic
  )

  pvals <- c(
    bptest(y)$p.value, bgtest(y)$p.value,
    dwtest(y)$p.value, gqtest(y)$p.value
  )

  n <- length(stats)

  # print

  cat(format(as.character("Gauss-Markov assumptions tests"), width=w4, justify="centre"), "\n\n")

  cat(rep("-", w), sep = "", "\n")
  cat(
    format("Test", width = w1, justify = "centre"), fs(), format("Statistic", width = w2, justify = "centre"),
    fs(), format("p-value", width = 7, justify = "centre"), "\n"
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

