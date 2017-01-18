# helper functions: regress
fs <- function() {
    x <- rep("  ")
    return(x)
}

fg <- function(x, w) {
    z <- as.character(x)
    y <- format(z, width = w, justify = 'right')
    return(y)
}

fl <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "left")
    return(ret)
}

fc <- function(x, w) {
    x <- as.character(x)
    ret <- format(x, width = w, justify = "centre")
    return(ret)
}

# var-test
fw <- function(x, w) {
    z <- format(as.character(x), width = w, justify = 'right')
    return(z)
}

# one-samp-var-test
formatter_t <- function(x, w) {
  ret <- format(x, width = w, justify = "centre")
  return(ret)
}

formatter_n <- function(x, w) {
  ret <- format(x, nsmall = 3)
  ret1 <- format(ret, width = w, justify = "centre")
  return(ret1)
}

format_cil <- function(x, w) {
  ret <- format(x, nsmall = 3)
  ret1 <- format(ret, width = w, justify = "centre")
  return(ret1)
}

format_ciu <- function(x, w) {
  ret <- format(x, nsmall = 3)
  ret1 <- format(ret, width = w, justify = "centre")
  return(ret1)
}

formats_t <- function() {
    x <- rep("  ")
    return(x)
}


# helper functions: fitted line properties
l <- function(x) {
    x <- as.character(x)
    k <- grep("\\$", x)
    if (length(k) == 1) {
        temp <- strsplit(x, "\\$")
        out <- temp[[1]][2]
    } else {
        out <- x
    }
    return(out)
}

# best-subsets
# combinations
combinations <- function(n, r) {
    factorial(n) / (factorial(n - r) * factorial(r))
}

# added variable Plot
advarx <- function(data , i) {
    j <- i - 1
    k <- names(data)
  fla <- as.formula(paste(k[j], "~ ."))
  out <- residuals(lm(fla, data = data))
  return(out)
}

advary <- function(data, i) {
  dat <- data[-i]
    k <- names(dat)
  fla <- as.formula(paste(k[1], "~ ."))
  out <- residuals(lm(fla, data = dat))
  return(out)
}

# bartlett test
#' @importFrom stats complete.cases var
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
  result <- ((1 / c) * (sumv * log10(pvar) - n2)) * 2.3026
  return(result)
}


# eigen cindex
evalue <- function(x) {

               y <- cbind(1,x)
	colnames(y)[1] <- "intercept"
	             z <- scale(y, scale = T, center = F)
	            tu <- t(z) %*% z
	             e <- eigen(tu / diag(tu))$values

          result <- list(e = e, pvdata = z)

  return(result)

}


cindx <- function(e) {
  return(sqrt(e[1] / e))
}


pveindex <- function(z) {
  svdx <- svd(z)
	 phi <- svdx$v %*% diag(1/svdx$d)
	  ph <- t(phi ^ 2)
	  pv <- prop.table(ph %*% diag(rowSums(ph, 1)), 2)
    return(pv)
}
