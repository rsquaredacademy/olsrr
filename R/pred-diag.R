pred_diag <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }
	
	nam       <- names(model.frame(model))
	predictor <- model.response(model.frame(model))
    n         <- length(predictor)
    tab       <- table(predictor)
    counts    <- unlist(sapply(tab, seq_len))
    height    <- max(tab) * 1.5
    op        <- par(no.readonly = TRUE)
    on.exit(par(op))

    par(mfrow = c(2, 2))

    plot(sort(predictor), counts, ylim = c(0, height), col = "blue", 
    	xlab = nam[1], ylab = "Counts", main = paste("Dotchart of", nam[1]))

    plot(predictor, type = "b", xlab = "Observation", col = "blue", 
		ylab = nam[1], main = paste("Scatter Plot of", nam[1]))

    h  <- hist(predictor, plot = F)
    yl <- max(h$counts) + 0.3 * max(h$counts)
    yp <- h$counts + 1

    hist(predictor, col = "blue", xlab = nam[1],
    	ylim = c(0, yl), main = paste("Histogram of", nam[1]))
    text(x = h$mids, y = yp, labels = h$counts)

    k <- boxplot(predictor, ylab = nam[1], col = "blue", 
	        outcol = "red", main = paste("Boxplot of", nam[1]))
	vals  <- round(k$stats, 2)
	leg_t <- paste("Min:", vals[1], "  Q1:", vals[2], 
		"  Median:", vals[3], "  Q3:", vals[4], "  Max:", vals[5])
	mtext(leg_t, side = 1, cex = 0.5)

}


