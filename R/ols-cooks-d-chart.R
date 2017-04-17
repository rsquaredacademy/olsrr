#' @importFrom ggplot2 geom_linerange
#' @title Cooks' D Chart
#' @description Chart of Cook's distance to detect observations that strongly influence fitted values of the model.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_cooksd_chart(model)
#' @export
#'
ols_cooksd_chart <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	obs <- NULL
	ckd <- NULL
	txt <- NULL
	Observation <- NULL
	k <- cdchart(model)
	d <- k$d
	d <- d %>% mutate(txt = ifelse(Observation == 'outlier', obs, NA))
	f <- d %>% filter(., Observation == 'outlier') %>% select(obs, ckd)
	p <- ggplot(d, aes(x = obs, y = ckd, , label = txt, ymin = min(ckd), ymax = ckd)) +
		geom_linerange(colour = 'blue') + geom_point(shape = 1, colour = 'blue') +
		geom_hline(yintercept = k$ts, colour = 'red') + xlab('Observation') + 
		ylab("Cook's D") + ggtitle("Cook's D Chart") +
		geom_text(vjust = -1, size = 3, family="serif", fontface="italic", colour="darkred") +
		annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2, 
                  family="serif", fontface="italic", colour="darkred", 
                  label = paste('Threshold:', k$ts))
		
	suppressWarnings(print(p))
	colnames(f) <- c("Observation", "Cook's Distance")
	invisible(f)

}