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
		myplots <- list()
	for (i in seq_len(np)) {

		dbetas <- dfb[, i]

		d <- tibble(obs = seq_len(n), dbetas = dbetas)
		p <- eval(substitute(ggplot(d, aes(x = obs, y = dbetas, ymin = 0, ymax = dbetas)) +
			geom_linerange(colour = 'blue') +
			geom_hline(yintercept = c(0, threshold, -threshold), colour = 'red') +
			geom_point(colour = 'blue', shape = 1) +
			xlab('Observation') + ylab('DFBETAS') +
			ggtitle(paste("Influence Diagnostics for", colnames(dfb)[i])),
			list(i = i)))
		print(p)
		myplots[[i]] <- p
	}

	do.call(grid.arrange, c(myplots, list(ncol = 2)))
}