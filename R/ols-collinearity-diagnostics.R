#' @title Collinearity Diagnostics
#' @description Variance inflation factor, tolerance, eigenvalues and condition indices. 
#' @param model an object of class \code{lm}
#' @return \code{ols_coll_diag} returns an object of class \code{"ols_coll_diag"}.
#' An object of class \code{"ols_coll_diag"} is a list containing the
#' following components:
#'
#' \item{vif_t}{tolerance and variance inflation factors}
#' \item{eig_cindex}{eigen values and condition index}
#' @references Belsley, D. A., Kuh, E., and Welsch, R. E. (1980). Regression Diagnostics: Identifying Influential Data and
#' Sources of Collinearity. New York: John Wiley & Sons.
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#' 
#' # vif and tolerance
#' ols_vif_tol(model)
#' 
#' # eigenvalues and condition indices
#' ols_eigen_cindex(model)
#' 
#' # collinearity diagnostics
#' ols_coll_diag(model)
#' 
#' @export
#'
ols_coll_diag <- function(model) UseMethod('ols_coll_diag')

#' @export
#'
ols_coll_diag.default <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	vift          <- ols_vif_tol(model)
	eig_ind       <- ols_eigen_cindex(model)
	result        <- list(vif_t = vift, eig_cindex = eig_ind)
	class(result) <- 'ols_coll_diag'

	return(result)
}

#' @export
#'
print.ols_coll_diag <- function(x, ...) {

	cat('Tolerance and Variance Inflation Factor\n')
	cat('---------------------------------------\n')
	print(x$vif_t)
	cat('\n\n')
	cat('Eigenvalue and Condition Index\n')
	cat('------------------------------\n')
	print(x$eig_cindex)

}

#' @rdname ols_coll_diag
#' @export
#'
ols_vif_tol <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

			vt <- viftol(model)
	result <- tibble(Variables = vt$nam,
		               Tolerance = round(vt$tol, 3),
	                       VIF = round(vt$vifs, 3))

	return(result)
}

#' @rdname ols_coll_diag
#' @export
#'
ols_eigen_cindex <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	x <- tibble::as_data_frame(model.matrix(model))
	e <- evalue(x)$e
	cindex <- cindx(e)
	pv <- pveindex(evalue(x)$pvdata)
	out <- data.frame(Eigenvalue = cbind(round(e, 3), round(cindex, 3),
															round(pv, 2)))
	colnames(out) <- c("Eigenvalue", "Condition Index", colnames(evalue(x)$pvdata))
	return(out)

}



