#' @importFrom stats dffits
#' @title DFFITS Plot
#' @description Plot for detecting influential observations using DFFITS.
#' @param model an object of class \code{lm}
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_dffits_plot(model)
#' @export
#'
ols_dffits_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	dffitsm  <- model %>% dffits() %>% unlist()
				 k <- length(model$coefficients)
				 n <- model %>% model.frame() %>% nrow()
	dffits_t <- 2 * sqrt(k / n)
	     obs <- NULL
		dbetas <- NULL

	d <- tibble(obs = seq_len(n), dbetas = dffitsm)
	p <- ggplot(d, aes(x = obs, y = dbetas, ymin = 0, ymax = dffitsm)) +
		geom_linerange(colour = 'blue') +
		geom_hline(yintercept = c(0, dffits_t, -dffits_t), colour = 'red') +
		geom_point(colour = 'blue', shape = 1) +
		xlab('Observation') + ylab('DFFITS') +
		ggtitle(paste("Influence Diagnostics for", names(model.frame(model))[1]))

	print(p)
}