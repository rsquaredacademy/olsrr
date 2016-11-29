studvslev_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	leverage  <- unname(hatvalues(model))
	rstudent  <- unname(rstudent(model))
	k         <- length(model$coefficients)
	n         <- nrow(model.frame(model))
	lev_thrsh <- ((2 * k) + 2) / n
	rst_thrsh <- 2
	miny      <- min(rstudent) - 3
	maxy      <- max(rstudent) + 3
	minx      <- min(leverage)
	maxx      <- ifelse((max(leverage) > lev_thrsh), max(leverage), (lev_thrsh + 0.05))
	levrstud  <- data.frame(obs = seq_len(n), leverage, rstudent)
	color     <- c("blue", "red", "green", "violet")
	
	levrstud$color[(leverage < lev_thrsh & abs(rstudent) < 2)] <- "normal" 
	levrstud$color[(leverage > lev_thrsh & abs(rstudent) < 2)] <- "leverage" 
	levrstud$color[(leverage < lev_thrsh & abs(rstudent) > 2)] <- "outlier" 
	levrstud$color[(leverage > lev_thrsh & abs(rstudent) > 2)] <- "outlier & leverage" 

	n_obs  <- levrstud$obs[(levrstud$leverage < lev_thrsh & abs(levrstud$rstudent) < 2)] 
	l_obs  <- levrstud$obs[(levrstud$leverage > lev_thrsh & abs(levrstud$rstudent) < 2)] 
	o_obs  <- levrstud$obs[(levrstud$leverage < lev_thrsh & abs(levrstud$rstudent) > 2)] 
	ol_obs <- levrstud$obs[(levrstud$leverage > lev_thrsh & abs(levrstud$rstudent) > 2)]

	
	levrstud$color3 <- factor(levrstud$color)
	levrstud$color4 <- ordered(levrstud$color3, 
	                           levels = c("normal", "leverage", "outlier",
	                                      "outlier & leverage"))

	text_loc <- filter(levrstud, 
	                   color == "outlier" | color == "leverage" | color == "outlier & leverage")
	text_loc[, c(1, 2)]
	textann <- which(levrstud$color == "outlier" | levrstud$color == "leverage" |
	                     levrstud$color == "outlier & leverage")

	plot(leverage, rstudent, 
			 col  = color[levrstud$color4],
	     xlab = "Leverage", ylab = "RStudent",
	     main = paste("Outlier and Leverage Diagnostics for", names(model.frame(model))[1]),
	     ylim = c(miny, maxy), 
	     xlim = c(minx, maxx))
	abline(h  = 2)
	abline(h  = -2)
	abline(v  = lev_thrsh)
	text(jitter(text_loc[, 1], factor = 5), text_loc[, 2], textann, cex = 0.5)
	legend("top", c("Outlier", "Leverage", "Outlier & Leverage"), 
		     horiz = T,
	       pch   = 1, 
	       col   = c("green", "red", "violet"), 
	       cex   = 0.5)

	z <- list(leverage        = leverage,
						studresid       = rstudent,
						lev_threshold   = lev_thrsh,
						rstud_threshold = rst_thrsh,
						normal          = n_obs,
						outliers        = o_obs,
						leverages       = l_obs,
						out_lev         = ol_obs)
	
}

