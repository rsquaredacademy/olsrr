#' @importFrom stats fitted rstudent
#' @importFrom dplyr mutate
#' @title Deleted Studentized Residual vs Fitted Values Plot
#' @description Plot for detecting outliers.
#' @param model an object of class \code{lm}
#' @examples 
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_dsrvsp_plot(model)
#' @export
#'
ols_dsrvsp_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	dsr <- NULL
	pred <- NULL
	txt <- NULL
	obs <- NULL
	Observation <- NULL
	
	k <- dpred(model)
	d <- k$ds
  d <- d %>% mutate(txt = ifelse(Observation == 'outlier', obs, NA))
	f <- d %>% filter(., Observation == 'outlier') %>% select(obs, pred, dsr)
	p <- ggplot(d, aes(x = pred, y = dsr, label = txt)) +
		geom_point(aes(colour = Observation)) +
		scale_color_manual(values = c('blue', 'red')) +
		ylim(k$cminx, k$cmaxx) + xlab('Predicted Value') + 
		ylab('Deleted Studentized Residual') +
		ggtitle("Deleted Studentized Residual vs Predicted Values") +
		geom_hline(yintercept = c(-2, 2), colour = 'red') +
		geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family="serif", fontface="italic", colour="darkred") 
		annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2, 
      family="serif", fontface="italic", colour="darkred", 
      label = paste0('Threshold: abs(', 2, ')'))

	suppressWarnings(print(p))
	colnames(f) <- c("Observation", "Fitted Values", "Deleted Studentized Residual")
	invisible(f)
}