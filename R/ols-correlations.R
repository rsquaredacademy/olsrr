#' @importFrom stats cor
#' @title Part and Partial Correlations
#' @description Zero-order, part and partial correlations
#' @param model an object of class \code{lm}
#' @return \code{ols_correlations} returns an object of class \code{"ols_correlations"}.
#' An object of class \code{"ols_correlations"} is a data frame containing the
#' following components:
#'
#' \item{Zero-order}{zero order correlations}
#' \item{Partial}{partial correlations}
#' \item{Part}{part correlations}
#' @details \code{correlations} returns the relative importance of independent variables in determining response variable. 
#' How much each variable uniquely contributes to rsquare over and above that which can be accounted for by the other predictors?
#' Zero order correlation is the Pearson correlation coefficient between the dependent variable and the
#' independent variables. Part correlations indicates how much rsquare will decrease if that variable is removed from the model
#' and partial correlations indicates amount of variance in response variable, which is not estimated by the other
#' independent variables in the model, but is estimated by the specific variable.
#'
#' @references Morrison, D. F. 1976. Multivariate statistical methods. New York: McGraw-Hill.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_correlations(model)
#' @export
#'
ols_correlations <- function(model) UseMethod('ols_correlations')

#' @export
#'
ols_correlations.default <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    result <- corout(model, corm2(model))
    class(result) <- c('ols_correlations', 'data.frame')
    return(result)

}

#' @export
#'
print.ols_correlations <- function(x, ...) {
    print_correlations(x)
}
