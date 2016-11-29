cooksd_chart <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	ckd <- cooks_d(model)
	ts  <- 4 / length(ckd)

	plot(seq_len(length(ckd)), ckd, type = "h", lwd = 1, col = "blue",
	     xlab = "Observation", ylab = "Cook's D", 
	     main = paste("Cook's D for", names(model.frame(model))[1]))
	points(ckd, col = "blue")
	abline(h = ts, col = "gray")

	outlier <- ckd[ckd > ts]
	obs     <- as.numeric(names(outlier))

	text(jitter(obs), as.vector(outlier), obs, cex = 0.8)

	z <- list(cooks_d   = ckd,
						threshold = round(ts, 4),
						outliers  = obs)
	
}