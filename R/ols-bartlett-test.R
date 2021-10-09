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
#' @return \code{ols_test_bartlett} returns an object of class \code{"ols_test_bartlett"}.
#' An object of class \code{"ols_test_bartlett"} is a list containing the
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
#' # using grouping variable
#' library(descriptr)
#' ols_test_bartlett(mtcarz, 'mpg', group_var = 'cyl')
#'
#' # using variables
#' ols_test_bartlett(hsb, 'read', 'write')
#'
#' @importFrom stats pchisq formula
#'
#' @export
#'
ols_test_bartlett <- function(data, ...) UseMethod("ols_test_bartlett")

#' @export
#' @rdname ols_test_bartlett
#'
ols_test_bartlett.default <- function(data, ..., group_var = NULL) {

  groupvar  <- group_var
  varyables <- unlist(list(...))
  fdata     <- data[varyables]
  var_c     <- names(fdata)

  if (is.null(groupvar)) {

    z  <- as.list(fdata)
    ln <- unname(unlist(lapply(z, length)))
    ly <- seq_len(length(z))
  
    if (length(z) < 2) {
      stop("Please specify at least two variables.", call. = FALSE)
    }

    out       <- rep(ly, times = ln)
    # out       <- gvar(ln, ly)
    fdata     <- unlist(z)
    groupvars <- as.factor(unlist(out))
  
    g_var <- NULL

  } else {

    fdata     <- fdata[[1]]
    groupvars <- data[[groupvar]]
    g_var     <- names(data[groupvar])
     
    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }
  }

  df    <- nlevels(groupvars) - 1
  fstat <- bartlett_fstat(fdata, groupvars)
  pval  <- pchisq(fstat, df, lower.tail = FALSE)

  out <- list(
    df    = df,
    fstat = fstat,
    g_var = g_var,
    pval  = pval,
    var_c = var_c)

  class(out) <- "ols_test_bartlett"

  return(out)

}

#' @export
#'
print.ols_test_bartlett <- function(x, ...) {
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
