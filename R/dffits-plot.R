dffits_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	dffitsm  <- unlist(dffits(model))
	k        <- length(model$coefficients)
	n        <- nrow(model.frame(model))
	dffits_t <- 2 * sqrt(k / n)
	outlier  <- dffitsm[abs(dffitsm) > dffits_t]
	obs      <- length(outlier)
	obs_n    <- c()

	for (i in seq_len(obs)) {
		obs_n[i] <- which(dffitsm == outlier[i])
	}

	plot(seq_len(length(dffitsm)), dffitsm, type = "h", lwd = 1, col = "blue",
	     xlab = "Observation", ylab = "DFFITS", 
	     main = paste("Influence Diagnostics for", names(model.frame(model))[1]))
	points(dffitsm, col = "blue")
	abline(h = 0, col = "blue")
	abline(h = -dffits_t)
	abline(h = dffits_t)

	if(obs > 0) {
		text(jitter(obs_n), outlier, obs_n, cex = 0.8)   
	}

	z <- list(dffits    = dffitsm,
						threshold = round(dffits_t, 4),
						outliers  = obs_n)

}



