cooksd_bplot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	cooksd <- cooks_d(model)
	n      <- length(cooksd)
	ckd    <- data.frame(obs = seq_len(n), cd = cooksd)
	ts     <- 4 / length(ckd$cd)

	ckd <- ckd %>%
	    mutate(color = ifelse((cd >= ts), "outlier", "normal"))
	
	ckd$color1 <- factor(ckd$color)
	ckd$color2 <- ordered(ckd$color1, levels = c("normal", "outlier"))
	color      <- c(NA, "blue")
	maxx       <- max(ckd$cd) + 0.1
	ln         <- length(ckd$cd)

	barplot(ckd$cd, col = color[ckd$color2], axes = T, xlim = c(0, maxx), 
	        width = 0.5, space = 1, horiz = T, names.arg = seq_len(ln),
	        cex.names = 0.5, main = "Cook's D",
	        xlab = "Cook's D", 
	        ylab = "Observation")
	abline(v = 0)
	abline(v = ts, lwd = 0.1, col = "gray")

	z <- list(cooks_d   = ckd$cd,
					  threshold = round(ts, 4),
					  normal    = ckd$obs[ckd$color == "normal"],
					  outlier   = ckd$obs[ckd$color == "outlier"])
}