#' @importFrom dplyr filter
#' @importFrom ggplot2 geom_vline
#' @title Studentized Residuals vs Leverage Plot 
#' @description Graph for detecting outliers and/or observations with high leverage.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(read ~ write + math + science, data = hsb)
#' ols_rsdlev_plot(model)
#'
#' @export
#'
ols_rsdlev_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	Observation <- NULL
	   leverage <- NULL
	        txt <- NULL
	        obs <- NULL
	       resp <- model %>% model.frame() %>% names() %>% `[`(1)
						g <- rstudlev(model)
						d <- g$levrstud
						d <- d %>% mutate(txt = ifelse(Observation == 'normal', NA, obs))
						f <- d %>% filter(., Observation == 'outlier') %>% select(obs, leverage, rstudent)

	p <- ggplot(d, aes(leverage, rstudent, label = txt)) +
		geom_point(shape = 1, aes(colour = Observation)) +
		scale_color_manual(values = c("blue", "red", "green", "violet")) +
		xlim(g$minx, g$maxx) + ylim(g$miny, g$maxy) +
		xlab('Leverage') + ylab('RStudent') +
		ggtitle(paste("Outlier and Leverage Diagnostics for", resp)) +
		geom_hline(yintercept = c(2, -2), colour = 'maroon') +
		geom_vline(xintercept = g$lev_thrsh, colour = 'maroon') +
		geom_text(vjust = -1, size = 3, family="serif", fontface="italic", colour="darkred") +
		annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2, 
      family="serif", fontface="italic", colour="darkred", 
      label = paste('Threshold:', g$lev_thrsh))

	suppressWarnings(print(p))
	colnames(f) <- c("Observation", "Leverage", "Studentized Residuals")
	invisible(f)

}