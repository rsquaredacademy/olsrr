addvar_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	data   <- model.frame(model)
	xnames <- colnames(data)
	nl     <- ncol(data)
	dat2   <- data[-1]

	for(i in 2:nl) {

	    j    <- i - 1
	    dat  <- data[-i]
	    k    <- names(dat)
	    fla  <- as.formula(paste(k[1], "~ ."))
	    y    <- residuals(lm(fla, data = dat))
	    k2   <- names(dat2)
	    n    <- length(k2)
	    fla2 <- as.formula(paste(k2[j], "~ ."))
	    x    <- residuals(lm(fla2, data = dat2))

	    plot(x, y, col = "blue", 
	         xlab = paste(xnames[i], " | Others"),
	         ylab = paste(xnames[1], " | Others"))
	    abdat <- data.frame(y, x)
	    abline(lm(y ~ x, data = abdat), col = "gray")
	}
	
}