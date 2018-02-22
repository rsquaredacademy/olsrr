#' \code{olsrr} package
#'
#' Tools for teaching and learning OLS regression
#'
#' See the README on
#' \href{https://github.com/rsquaredacademy/olsrr}{GitHub}
#'
#' @docType package
#' @name olsrr
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "owner", "repo", "tag_name"
  ))
}
