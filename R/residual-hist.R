hist_resid <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	resid <- residuals(model)
	minx  <- min(resid) - 1
	maxx  <- max(resid) + 1
	h     <- hist(resid, xlim = c(minx, maxx))

	xfit  <- seq(min(resid), max(resid), length = 80) 
	yfit  <- dnorm(xfit, mean = mean(resid), sd = sd(resid)) 
	yfit  <- yfit * diff(h$mids[1:2]) * length(resid) 
	lines(xfit, yfit, col = "blue", lwd = 2)

	z <- list(residuals = resid,
						breaks    = h$breaks,
						counts    = h$counts, 
						density   = h$density, 
						mids      = h$mids, 
						xname     = h$xname, 
						equidist  = h$equidist,
						norm      = yfit)

}




