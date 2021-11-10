#' \code{olsrr} package
#'
#' Tools for teaching and learning OLS regression
#'
#' See the README on
#' \href{https://github.com/rsquaredacademy/olsrr}{GitHub}
#'
#' @docType package
#' @name olsrr
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(".", "owner", "repo", "tag_name", "result",
    "a", "b", "tx", "mindex", "n", "x", "y", "k", "size", "shape", "rsquare",
    "cp", "adjr", "cps", "aic", "sbic", "sbc", "index", "betas", "rsq",
    "lpreds", "terms", "pvdata", "values", "d", "v", "r.squared", "obs", 
    "txt", "cd", "fct_color", "ckd", "dbetas", "color", "pred", "ds", "dsr",
    "fstatistic", "hadi", "Df", "res", "pot", "lfit", "rerror", "ybar", 
    "yhat", "predicted", "resid", "lev_thrsh", "leverage", "levrstud",
    "sdres"))
}
