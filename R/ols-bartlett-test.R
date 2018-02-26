#' Bartlett test
#'
#' Test if k samples are from populations with equal variances.
#'
#' @param data A \code{data.frame} or \code{tibble}.
#' @param ... Columns in \code{data}.
#' @param group_var Grouping variable.
#'
#' @details
#' Bartlett's test is used to test if variances across samples is equal.
#' It is sensitive to departures from normality. The Levene test
#' is an alternative test that is less sensitive to departures from normality.
#'
#' @return \code{ols_bartlett_test} returns an object of class \code{"ols_bartlett_test"}.
#' An object of class \code{"ols_bartlett_test"} is a list containing the
#' following components:
#'
#' \item{fstat}{f statistic}
#' \item{pval}{p-value of \code{fstat}}
#' \item{df}{degrees of freedom}
#'
#' @references
#' Snedecor, George W. and Cochran, William G. (1989), Statistical Methods,
#' Eighth Edition, Iowa State University Press.
#'
#' @family heteroskedasticity tests
#'
#' @examples
#' \dontrun{
#' # using grouping variable
#' library(descriptr)
#' ols_bartlett_test(mtcarz, mpg, group_var = cyl)
#' }
#'
#' \dontrun{
#' # using variables
#' ols_bartlett_test(hsb, read, write)
#' }
#'
#' @importFrom rlang quo_is_null quos
#' @importFrom stats pchisq formula
#' @useDynLib olsrr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#'
#' @export
#'
ols_bartlett_test <- function(data, ...) UseMethod("ols_bartlett_test")

#' @export
#' @rdname ols_bartlett_test
#'
ols_bartlett_test.default <- function(data, ..., group_var = NULL) {

  groupvar  <- enquo(group_var)
  varyables <- quos(...)

  fdata <-
    data %>%
    select(!!! varyables)

  var_c <- names(fdata)

  if (quo_is_null(groupvar)) {

    z  <- as.list(fdata)
    ln <- map_int(z, length)

    ly <-
      z %>%
      length() %>%
      seq_len()

    if (length(z) < 2) {
      stop("Please specify at least two variables.", call. = FALSE)
    }

    out   <- gvar(ln, ly)
    fdata <- unlist(z)

    groupvars <-
      out %>%
      unlist() %>%
      as.factor()

    g_var <- NULL

  } else {

    fdata <-
      fdata %>%
      pull(1)

    groupvars <-
      data %>%
      pull(!! groupvar)

    g_var <-
      data %>%
      select(!! groupvar) %>%
      names()

    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }
  }

  df    <- nlevels(groupvars) - 1
  fstat <- bartlett_fstat(fdata, groupvars)
  pval  <- pchisq(fstat, df, lower.tail = FALSE)

  out <- list(
    fstat = fstat,
    pval  = pval,
    df    = df,
    var_c = var_c,
    g_var = g_var
  )

  class(out) <- "ols_bartlett_test"

  return(out)

}

#' @export
#'
print.ols_bartlett_test <- function(x, ...) {
  print_bartlett_test(x)
}


#' Bartlett internal
#'
#' Computes the f statistic for bartlett test.
#'
#' @importFrom stats complete.cases var
#'
#' @noRd
#'
bartlett_fstat <- function(variable, grp_var) {

  n     <- length(variable)
  k     <- nlevels(grp_var)
  comp  <- complete.cases(variable, grp_var)
  vars  <- tapply(variable[comp], grp_var[comp], var)
  lens  <- tapply(variable[comp], grp_var[comp], length)
  v     <- lens - 1
  sumv  <- sum(v)
  isumv <- sum(1 / v)
  c     <- 1 + (1 / (3 * (k - 1))) * (isumv - (1 / sumv))
  n2    <- sum(v * log10(vars))
  l     <- length(vars)
  ps    <- ((lens - 1) * vars) / (n - k)
  pvar  <- sum(ps)
  ((1 / c) * (sumv * log10(pvar) - n2)) * 2.3026

}
