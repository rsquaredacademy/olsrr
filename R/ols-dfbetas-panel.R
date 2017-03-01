#' @importFrom stats dfbetas
#' @title DFBETAs Panel
#' @description Panel of plots to detect influential observations using DFBETAs.
#' @param model an object of class \code{lm}
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_dfbetas_panel(model)
#' @export
#'
ols_dfbetas_panel <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	      dfb <- dfbetas(model)
	        n <- nrow(dfb)
	       np <- ncol(dfb)
	threshold <- 2 / sqrt(n)
			  obs <- NULL
			  txt <- NULL
	Observation <- NULL
		myplots <- list()
		outliers <- list()
	for (i in seq_len(np)) {

		dbetas <- dfb[, i]

		d <- tibble(obs = seq_len(n), dbetas = dbetas)
		d$color <- ifelse(((d$dbetas >= threshold) | (d$dbetas <= -threshold)), c("outlier"), c("normal"))
    d$color1 <- factor(d$color)
    d$Observation <- ordered(d$color1, levels = c("normal", "outlier"))
    d <- d %>% mutate(txt = ifelse(Observation == 'outlier', obs, NA))
	  f <- d %>% filter(., Observation == 'outlier') %>% select(obs, dbetas)
		p <- eval(substitute(ggplot(d, aes(x = obs, y = dbetas, label = txt, ymin = 0, ymax = dbetas)) +
			geom_linerange(colour = 'blue') +
			geom_hline(yintercept = c(0, threshold, -threshold), colour = 'red') +
			geom_point(colour = 'blue', shape = 1) +
			xlab('Observation') + ylab('DFBETAS') +
			ggtitle(paste("Influence Diagnostics for", colnames(dfb)[i])) +
			geom_text(hjust = -0.2, nudge_x = 0.15, size = 2, family="serif", fontface="italic", colour="darkred") +
			annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2, 
                  family="serif", fontface="italic", colour="darkred", 
                  label = paste('Threshold:', round(threshold, 2))),
			list(i = i)))
		# print(p)
		myplots[[i]] <- p
		outliers[[i]] <- f
	}

	suppressWarnings(do.call(grid.arrange, c(myplots, list(ncol = 2))))
	names(outliers) <- model %>% coefficients() %>% names()
	invisible(outliers)
}