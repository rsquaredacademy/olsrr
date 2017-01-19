#' @importFrom stats cor
#' @title Part and Partial Correlations
#' @description Zero-order, part and partial correlations
#' @param model an object of class \code{lm}
#' @return \code{correlations} returns an object of class \code{"correlations"}.
#' An object of class \code{"correlations"} is a data frame containing the
#' following components:
#'
#' \item{Zero-order}{zero order correlations}
#' \item{Partial}{partial correlations}
#' \item{Part}{part correlations}
#' @export
#'
correlations <- function(model) UseMethod('correlations')

#' @export
#'
correlations.default <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    result <- corout(model, corm2(model))
    class(result) <- c('correlations', 'data.frame')
    return(result)

}

#' @export
#'
print.correlations <- function(x, ...) {
    print_correlations(x)
}
