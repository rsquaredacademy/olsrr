levene_test <- function(variable, ..., group_var = NA, 
	alternative = c("mean", "median", "trimmed-mean", "all"), trim.mean = 0.1) UseMethod('levene_test')

levene_test.default <- function(variable, ..., group_var = NA, 
	alternative = c("mean", "median", "trimmed-mean", "all"), trim.mean = 0.1) {

	type    <- match.arg(alternative)
	varname <- deparse(substitute(variable))

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
		    
		  variable  <- unlist(z)
		  group_var <- unlist(out)

		} else {

    	if (length(variable) != length(group_var)) {
    		stop('Length of variable and group_var do not match.', call. = FALSE)
    	}

    }

	)
	
	if (!is.factor(group_var)) {
		group_var <- as.factor(group_var)
	}

	comp       <- complete.cases(variable, group_var)
	n          <- length(comp)
  k          <- nlevels(group_var)
  lens       <- tapply(variable[comp], group_var[comp], length)
  avgs       <- tapply(variable[comp], group_var[comp], mean)
	sds        <- tapply(variable[comp], group_var[comp], sd)

	len        <- length(variable)
	avg        <- mean(variable)
	sd         <- sd(variable)
	
	metric_bf  <- tapply(variable[comp], group_var[comp], mean)
	y_bf       <- abs(variable - metric_bf[group_var])
	result_bf  <- anova(lm(y_bf ~ group_var))
	fstat_bf   <- result_bf$`F value`[1]
	p_bf       <- result_bf$`Pr(>F)`[1]

	metric_lev <- tapply(variable[comp], group_var[comp], median)
	y_lev      <- abs(variable - metric_lev[group_var])
	result_lev <- anova(lm(y_lev ~ group_var))
	fstat_lev  <- result_lev$`F value`[1]
	p_lev      <- result_lev$`Pr(>F)`[1]

	metric_bft <- tapply(variable[comp], group_var[comp], mean, trim = trim.mean)
	y_bft      <- abs(variable - metric_bft[group_var])
	result_bft <- anova(lm(y_bft ~ group_var))
	fstat_bft  <- result_bft$`F value`[1]
	p_bft      <- result_bft$`Pr(>F)`[1]

	n_df       <- k - 1
	d_df       <- n - k

	out <- list(bf    = round(fstat_bf, 4), 
              p_bf  = round(p_bf, 4), 
              lev   = round(fstat_lev, 4), 
		          p_lev = round(p_lev, 4), 
              bft   = round(fstat_bft, 4), 
              p_bft = round(p_bft, 4), 
              avgs  = round(avgs, 2), 
              sds   = round(sds, 2), 
              avg   = round(avg, 2), 
              sd    = round(sd, 2),
              n     = n, 
              levs  = levels(group_var), 
		          n_df  = n_df, 
              d_df  = d_df, 
              lens  = lens, 
		          len   = len, 
              type  = type)
	
	class(out) <- 'levene_test'
	
  return(out)

}


levene_test.lm <- function(model) {

	levene_test.formula(formula(model), data=model.frame(model), ...)

}


levene_test.formula <- function(formula, data) {
	
	dat       <- model.frame(formula, data)
	variable  <- dat[, 1]
	group_var <- dat[, 2]

	levene_test.default(variable = variable, group_var = group_var)
}


print.levene_test <- function(data) {

  print_levene_test(data)

}