bartlett_test <- function(variable, ..., group_var = NA) UseMethod('bartlett_test')

bartlett_test.default <- function(variable, ..., group_var = NA) {

	var_c <- deparse(substitute(variable))
	suppressWarnings(
		if (is.na(group_var)) {
			dots  <- substitute(list(...))[-1]
	    var_c <- c(var_c, sapply(dots, deparse))
	  	g_var <- NA	
		} else {
			g_var <- deparse(substitute(group_var))	
		}
	)

	grp_var <- group_var

  suppressWarnings(
    if (is.na(group_var)) {
    
    	z   <- list(variable, ...)
    	ln  <- lapply(z, length)
    	ly  <- length(z)

    	if (ly < 2) {
    		stop('Please specify at least two variables.', call. = FALSE)
    	}

    	out <- list()
    	
    	for (i in seq_len(ly)) {
    	  out[[i]] <- as.factor(rep(i, ln[i]))
    	}
    	    
    	variable <- unlist(z)
    	grp_var  <- unlist(out)
    
    } else {

    	if (length(variable) != length(group_var)) {
    		stop('Length of variable and group_var do not match.', call. = FALSE)
    	}

    }
	)

	if (!is.factor(grp_var)) {
		grp_var <- as.factor(grp_var)
	}

	n     <- length(variable)
	k     <- nlevels(grp_var)
	df    <- k - 1
	comp  <- complete.cases(variable, grp_var)
	vars  <- tapply(variable[comp], grp_var[comp], var)
	lens  <- tapply(variable[comp], grp_var[comp], length)
	v     <- lens - 1
	sumv  <- sum(v)
	isumv <- sum(1 / v)
	c     <- 1 + (1 / (3 * (k - 1))) * (isumv - (1 / sumv))
	n2    <- sum(v * log10(vars))
	l     <- length(vars)
	ps    <- c()

	for (i in seq_len(l)) {
	    ps[i] <- ((lens[i] - 1) * vars[i]) / (n - k)
	}

	pvar  <- sum(ps) 
	fstat <- ((1 / c) * (sumv * log10(pvar) - n2)) * 2.3026
	pval  <- pchisq(fstat, df, lower.tail = FALSE)

	out <- list(fstat = round(fstat, 3), 
		          pval  = round(pval, 3), 
		          df    = df, 
		          var_c = var_c, 
			        g_var = g_var)	

	class(out) <- 'bartlett_test'

	return(out)

}


bartlett_test.lm <- function(model) {
	
	bartlett_test.formula(formula(model), data=model.frame(model), ...)

}


bartlett_test.formula <- function(formula, data) {
	
	dat       <- model.frame(formula, data)
	variable  <- dat[, 1]
	group_var <- dat[, 2]
	bartlett_test.default(variable = variable, group_var = group_var)

}


print.bartlett_test <- function(data) {

	print_bartlett_test(data)

}