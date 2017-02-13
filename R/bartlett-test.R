#' @importFrom stats pchisq formula
#' @useDynLib olsrr
#' @importFrom Rcpp sourceCpp
#' @title Bartlett Test
#' @description Test if k samples have equal variances
#' @param variable a numeric vector
#' @param ... numeric vectors
#' @details Bartlett's test is used to test if variances across samples is equal. 
#' It is sensitive to departures from normality. The Levene test
#' is an alternative test that is less sensitive to departures from normality.
#' @return \code{bartlett_test} returns an object of class \code{"bartlett_test"}.
#' An object of class \code{"bartlett_test"} is a list containing the
#' following components:
#'
#' \item{fstat}{f statistic}
#' \item{pval}{p-value of \code{fstat}}
#' \item{df}{degrees of freedom}
#' \item{var_c}{name(s) of \code{variable}}
#' \item{g_var}{name of \code{group_var}}
#'
#' @references Snedecor, George W. and Cochran, William G. (1989), Statistical Methods, 
#' Eighth Edition, Iowa State University Press.
#' @examples
#' # using grouping variable
#' model <- lm(mpg ~ disp + hp, data = mtcars)
#' resid <- residuals(model)
#' cyl <- as.factor(mtcars$cyl)
#' bartlett_test(resid, group_var = cyl)
#' 
#' # using variables
#' bartlett_test(hsb$read, hsb$write)
#' 
#' # using formula
#' mt <- mtcars
#' mt$cyl <- as.factor(mt$cyl)
#' bartlett_test(mpg ~ cyl, data = mt)
#' 
#' # using model
#' model <- lm(mpg ~ cyl, data = mt)
#' bartlett_test(model)
#' @export
#'
bartlett_test <- function(variable, ...) UseMethod('bartlett_test')

#' @export
#'
bartlett_test.default <- function(variable, ..., group_var = NA) {

	var_c <- deparse(substitute(variable))
	suppressWarnings(
		if (is.na(group_var)) {
			if (is.data.frame(variable)) {
				var_c <- names(variable)
			} else {
				dots  <- substitute(list(...))[-1]
		    var_c <- c(var_c, sapply(dots, deparse))
			}
	  	g_var <- NA
		} else {
			g_var <- deparse(substitute(group_var))
		}
	)

	grp_var <- group_var

  suppressWarnings(
    if (is.na(group_var)) {

			if(is.data.frame(variable)) {
				z <- as.list(variable)
			} else {
				z <- list(variable, ...)
			}
			ln <- z %>% map_int(length)
			ly <- seq_len(length(z))

    	if (length(z) < 2) {
    		stop('Please specify at least two variables.', call. = FALSE)
    	}

    	     out <- gvar(ln, ly)
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
	fstat <- bartlett_fstat(variable, grp_var)
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
