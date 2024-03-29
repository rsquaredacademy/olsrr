#' Collinearity diagnostics
#'
#' @description
#' Variance inflation factor, tolerance, eigenvalues and condition indices.
#'
#' @param model An object of class \code{lm}.
#'
#' @details
#' Collinearity implies two variables are near perfect linear combinations of
#' one another. Multicollinearity involves more than two variables. In the
#' presence of multicollinearity, regression estimates are unstable and have
#' high standard errors.
#'
#' \emph{Tolerance}
#'
#' Percent of variance in the predictor that cannot be accounted for by other predictors.
#'
#' Steps to calculate tolerance:
#'
#' \itemize{
#'   \item Regress the kth predictor on rest of the predictors in the model.
#'   \item Compute \eqn{R^2} - the coefficient of determination from the regression in the above step.
#'   \item \eqn{Tolerance = 1 - R^2}
#' }
#'
#' \emph{Variance Inflation Factor}
#'
#' Variance inflation factors measure the inflation in the variances of the parameter estimates due to
#' collinearities that exist among the predictors. It is a measure of how much the variance of the estimated
#' regression coefficient \eqn{\beta_k}  is inflated by the existence of correlation among the predictor variables
#' in the model. A VIF of 1 means that there is no correlation among the kth predictor and the remaining predictor
#' variables, and hence the variance of \eqn{\beta_k} is not inflated at all. The general rule of thumb is that VIFs
#' exceeding 4 warrant further investigation, while VIFs exceeding 10 are signs of serious multicollinearity
#' requiring correction.
#'
#' Steps to calculate VIF:
#'
#' \itemize{
#'   \item Regress the kth predictor on rest of the predictors in the model.
#'   \item Compute \eqn{R^2} - the coefficient of determination from the regression in the above step.
#'   \item \eqn{Tolerance = 1 / 1 - R^2 = 1 / Tolerance}
#' }
#'
#' \emph{Condition Index}
#'
#' Most multivariate statistical approaches involve decomposing a correlation matrix into linear
#' combinations of variables. The linear combinations are chosen so that the first combination has
#' the largest possible variance (subject to some restrictions), the second combination
#' has the next largest variance, subject to being uncorrelated with the first, the third has the largest
#' possible variance, subject to being uncorrelated with the first and second, and so forth. The variance
#' of each of these linear combinations is called an eigenvalue. Collinearity is spotted by finding 2 or
#' more variables that have large proportions of variance (.50 or more) that correspond to large condition
#' indices. A rule of thumb is to label as large those condition indices in the range of 30 or larger.
#'
#'
#' @return \code{ols_coll_diag} returns an object of class \code{"ols_coll_diag"}.
#' An object of class \code{"ols_coll_diag"} is a list containing the
#' following components:
#'
#' \item{vif_t}{tolerance and variance inflation factors}
#' \item{eig_cindex}{eigen values and condition index}
#'
#' @references
#' Belsley, D. A., Kuh, E., and Welsch, R. E. (1980). Regression Diagnostics: Identifying Influential Data and
#' Sources of Collinearity. New York: John Wiley & Sons.
#'
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
ols_coll_diag <- function(model) UseMethod("ols_coll_diag")

#' @export
#'
ols_coll_diag.default <- function(model) {

  check_model(model)

  vift    <- ols_vif_tol(model)
  eig_ind <- ols_eigen_cindex(model)
  result  <- list(eig_cindex = eig_ind, vif_t = vift)

  class(result) <- "ols_coll_diag"
  return(result)

}

#' @export
#'
print.ols_coll_diag <- function(x, ...) {
  cat("Tolerance and Variance Inflation Factor\n")
  cat("---------------------------------------\n")
  print(x$vif_t)
  cat("\n\n")
  cat("Eigenvalue and Condition Index\n")
  cat("------------------------------\n")
  print(x$eig_cindex)
}

#' @rdname ols_coll_diag
#' @export
#'
ols_vif_tol <- function(model) {

  check_model(model)
  vt <- viftol(model)
  data.frame(Variables = vt$nam, Tolerance = vt$tol, VIF = vt$vifs)

}

#' @rdname ols_coll_diag
#' @export
#'
ols_eigen_cindex <- function(model) {

  check_model(model)

  x      <- as.data.frame(model.matrix(model))
  e      <- evalue(x)$e
  cindex <- cindx(e)
  pv     <- pveindex(evalue(x)$pvdata)
  out    <- data.frame(Eigenvalue = cbind(e, cindex, pv))
  
  colnames(out) <- c("Eigenvalue", "Condition Index", colnames(evalue(x)$pvdata))
  return(out)

}


evalue <- function(x) {

  y              <- x
  colnames(y)[1] <- "intercept"
  z              <- scale(y, scale = T, center = F)
  tu             <- t(z) %*% z

  e <- eigen(tu / diag(tu))$values
  list(e = e, pvdata = z)

}


cindx <- function(e) {
  sqrt(e[1] / e) 
}

pveindex <- function(z) {

  svdx  <- svd(z)
  svdxd <- svdx$d

  phi_diag <- diag(1 / svdxd)
  phi      <- svdx$v %*% phi_diag
  ph       <- t(phi ^ 2)
  diag_sum <- diag(rowSums(ph, dims = 1))
  
  prop.table(ph %*% diag_sum, margin = 2)

}


fmrsq <- function(nam, data, i) {
  
  fm <- as.formula(paste0("`", nam[i], "` ", "~ ."))
  m1 <- summary(lm(fm, data = data))$r.squared

  1 - m1

}


#' @description Computes vif and tolerance
#'
#' @noRd
#'
viftol <- function(model) {

  m   <- as.data.frame(model.matrix(model))[, -1]
  nam <- names(m)
  p   <- length(model$coefficients) - 1
  tol <- c()

  for (i in seq_len(p)) {
    tol[i] <- fmrsq(nam, m, i)
  }

  vifs <- 1 / tol
  list(nam = names(m), tol = tol, vifs = vifs)

}
