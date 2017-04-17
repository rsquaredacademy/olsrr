#' @importFrom stats cooks.distance
#' @importFrom dplyr filter select
#' @importFrom ggplot2 geom_bar coord_flip ylim geom_hline geom_label
#' @title Cooks' D Bar Plot
#' @description Bar Plot of Cook's distance to detect observations that strongly influence fitted values of the model.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_cooksd_barplot(model)
#' @export
#'
ols_cooksd_barplot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	obs <- NULL
	txt <- NULL
	cd <- NULL
	Observation <- NULL
	k <- cdplot(model)
	d <- k$ckd
	d <- d %>% mutate(txt = ifelse(Observation == 'outlier', obs, NA))
	f <- d %>% filter(., Observation == 'outlier') %>% select(obs, cd)

	p <- ggplot(d, aes(x = obs, y = cd, label = txt)) + 
		geom_bar(width = 0.5, stat = 'identity', aes(fill = Observation)) +
		scale_fill_manual(values = c('blue', 'red')) + coord_flip() +
		ylim(0, k$maxx) + ylab("Cook's D") + xlab('Observation') + 
		ggtitle("Cook's D Bar Plot") + geom_hline(yintercept = 0) +
		geom_hline(yintercept = k$ts, colour = 'red') +
		geom_text(hjust = -0.2, nudge_x = 0.05, size = 2) +
		annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2, 
                  family="serif", fontface="italic", colour="darkred", 
                  label = paste('Threshold:', k$ts))
	
	suppressWarnings(print(p))
	colnames(f) <- c("Observation", "Cook's Distance")
	invisible(f)

}