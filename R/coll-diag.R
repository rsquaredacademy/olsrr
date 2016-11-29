coll_diag <- function(model) UseMethod('coll_diag')

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


print.coll_diag <- function(data) {

	cat('Tolerance and Variance Inflation Factor\n')
	cat('---------------------------------------\n')
	print(data$vif_t)
	cat('\n\n')
	cat('Eigenvalue and Condition Index\n')
	cat('------------------------------\n')
	print(data$eig_cindex)

}


vif_tol <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	m    <- model.frame(model)[-1]
	nam  <- names(m)
	p    <- length(model$coefficients) - 1
	vifs <- c()
	tol  <- c()

	for (i in seq_len(p)) {
		fm      <- as.formula(paste(nam[i], "~ ."))
		m1      <- lm(fm, data = m)
		rsq     <- summary(m1)$r.squared
		tol[i]  <- 1 - rsq
		vifs[i] <- 1 / tol[i]
		
	}

	viftol <- data.frame(Variables = names(m), 
		                   Tolerance = round(tol, 3),
	                     VIF       = round(vifs, 3))

	return(viftol)
}


eigen_cindex <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	x          		 <- model$model[, -1]
	x          		 <- cbind(1,x) 
	colnames(x)[1] <- "intercept"
	x              <- scale(x, scale = T, center = F)
	tu             <- t(x) %*% x 
	e              <- eigen(tu / diag(tu))$ values
	cindex         <- sqrt(e[1] / e)
	svdx           <- svd(x)
	phi            <- svdx$v %*% diag(1/svdx$d)
	phi            <- t(phi ^ 2)
	pv             <- prop.table(phi %*% diag(rowSums(phi, 1)), 2)
	out            <- data.frame(cbind(round(e, 3), round(cindex, 3), round(pv, 2)))
	names(out)     <- c("Eigenvalue", "Condition Index", colnames(x))
	return(out)
}

