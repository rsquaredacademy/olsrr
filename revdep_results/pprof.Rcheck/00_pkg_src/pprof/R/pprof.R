#' pprof package
#'
#' Description
#'
#'
#' @useDynLib pprof, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom globals globalsByName
#'
#' @noRd

# This function is never intended to be called by the user.
# It exists solely to satisfy R CMD check's requirement that
# declared imports must be used in the code.
.fix_unused_globals <- function() {
  globals::globalsByName
}
