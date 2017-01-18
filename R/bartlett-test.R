#' @importFrom stats complete.cases pchisq formula var
#' @title Bartlett Test
#' @description Test if k samples have equal variances
#' @param variable a numeric vector
#' @param ... numeric vectors
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return \code{bartlett_test} returns an object of class \code{"bartlett_test"}.
#' An object of class \code{"bartlett_test"} is a list containing the
#' following components:
#'
#' \item{fstat}{f statistic}
#' \item{pval}{p-value of \code{fstat}}
#' \item{df}{degrees of freedom}
#' \item{var_c}{name(s) of \code{variable}}
#' \item{g_var}{name of \code{group_var}}
#' @export
#'
bartlett_test <- function(variable, ...) UseMethod('bartlett_test')

#' @export
#'
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

	df    <- nlevels(grp_var) - 1
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

#' @export
#'
bartlett_test.lm <- function(variable, ...) {
	bartlett_test.formula(formula(variable), data=model.frame(variable), ...)
}

#' @export
#'
bartlett_test.formula <- function(variable, data, ...) {

				dat <- model.frame(variable, data)
				var <- dat[, 1]
	group_var <- dat[, 2]
	bartlett_test.default(variable = var, group_var = group_var)

}

#' @export
#'
print.bartlett_test <- function(x, ...) {
	print_bartlett_test(x)
}
