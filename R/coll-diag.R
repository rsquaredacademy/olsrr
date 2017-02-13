#' @title Collinearity Diagnostics
#' @description Collinearity implies two variables are near perfect linear combinations of one
#' another. Multicollinearity involves more than two variables. In the presence of multicollinearity, 
#' regression estimates are unstable and have high standard errors. \code{coll_diag}  
#' returns variance inflation factor, tolerance and condition indices. Collinearity is spotted by 
#' finding 2 or more variables that have large proportions of variance (.50 or more) that correspond 
#' to large condition indices. A rule of thumb is to label as large those condition indices in the range of 30 or larger.
#' Big values of VIF and small values of Tolerance indicate multicollinearity.
#' @param model an object of class \code{lm}
#' @return \code{coll_diag} returns an object of class \code{"coll_diag"}.
#' An object of class \code{"coll_diag"} is a list containing the
#' following components:
#'
#' \item{vif_t}{tolerance and variance inflation factors}
#' \item{eig_cindex}{eigen values and condition index}
#' @references Belsley, D. A., Kuh, E., and Welsch, R. E. (1980). Regression Diagnostics: Identifying Influential Data and
#' Sources of Collinearity. New York: John Wiley & Sons.
#' @export
#'
coll_diag <- function(model) UseMethod('coll_diag')

#' @export
#'
coll_diag.default <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	vift          <- vif_tol(model)
	eig_ind       <- eigen_cindex(model)
	result        <- list(vif_t = vift, eig_cindex = eig_ind)
	class(result) <- 'coll_diag'

	return(result)
}

#' @export
#'
print.coll_diag <- function(x, ...) {

	cat('Tolerance and Variance Inflation Factor\n')
	cat('---------------------------------------\n')
	print(x$vif_t)
	cat('\n\n')
	cat('Eigenvalue and Condition Index\n')
	cat('------------------------------\n')
	print(x$eig_cindex)

}

#' @rdname coll_diag
#' @export
#'
vif_tol <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

			vt <- viftol(model)
	result <- tibble(Variables = vt$nam,
		               Tolerance = round(vt$tol, 3),
	                       VIF = round(vt$vifs, 3))

	return(result)
}

#' @rdname coll_diag
#' @export
#'
eigen_cindex <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	x <- model$model[, -1]
	e <- evalue(x)$e
	cindex <- cindx(e)
	pv <- pveindex(evalue(x)$pvdata)
	out <- data.frame(Eigenvalue = cbind(round(e, 3), round(cindex, 3),
															round(pv, 2)))
	colnames(out) <- c("Eigenvalue", "Condition Index", colnames(evalue(x)$pvdata))
	return(out)

}


# x          		 <- cbind(1,x)
# colnames(x)[1] <- "intercept"
# x              <- scale(x, scale = T, center = F)
# tu             <- t(x) %*% x
# e              <- eigen(tu / diag(tu))$values
# cindex         <- sqrt(e[1] / e)
# svdx           <- svd(x)
# phi            <- svdx$v %*% diag(1/svdx$d)
# phi            <- t(phi ^ 2)
# pv             <- prop.table(phi %*% diag(rowSums(phi, 1)), 2)
