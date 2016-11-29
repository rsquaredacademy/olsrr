osvar_test <- function(data, hyp_sd = 0.5, conf.level = 0.95,
	alternative = c("two.sided", "less", "greater", "all")) UseMethod("osvar_test")

osvar_test.default <- function(data, hyp_sd = 0.5, conf.level = 0.95,
	alternative = c("two.sided", "less", "greater", "all")) {

	if (!is.numeric(data)) {
		stop('data must be numeric', call. = FALSE)
	}

	if (!is.numeric(hyp_sd)) {
		stop('standard deviation must be numeric', call. = FALSE)
	}

	if (hyp_sd < 0) {
		stop('standard deviation must be positive', call. = FALSE)
	}

	if (!is.numeric(conf.level)) {
    stop('conf.level must be numeric', call. = FALSE)
  } 

  if ((conf.level < 0) | (conf.level > 1)) {
    stop('conf.level must be between 0 and 1', call. = FALSE)
  }

	type     <- match.arg(alternative)
  var_name <- deparse(substitute(data))
	n        <- length(data)
	a        <- (1 - conf.level) / 2
	df       <- n - 1
	avg      <- round(mean(data), 3)
	ssd      <- round(sd(data), 3)
	se       <- round(ssd / sqrt(n), 3)
	hsd      <- hyp_sd
	t        <- (n - 1) * ((ssd / hsd) ^ 2)
	cint     <- qnorm(a, lower.tail = FALSE) * se
	confint  <- avg + c(-cint, +cint)
	lchi     <- pchisq(t, df)
	uchi     <- 1 - pchisq(t, df)
	tchi     <- 2 * (1 - pchisq(t, df))

	out <- list(t         = round(t, 2), 
                lchi    = round(lchi, 3), 
                uchi    = round(uchi, 3), 
                tchi    = round(tchi, 3), 
                confint = round(confint, 3), 
                n       = n, 
                sd      = ssd, 
                hyp_sd  = hyp_sd, 
                df      = df, 
                avg     = avg,
		        se      = se, 
                type    = type, 
                varname = var_name)

	class(out) <- 'osvar_test'

	return(out)
}


print.osvar_test <- function(data, ...) {

    print_osvar_test(data)

}


