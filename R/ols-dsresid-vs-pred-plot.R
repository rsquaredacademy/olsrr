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
	p <- ggplot(d, aes(x = pred, y = dsr, label = txt))
	p <- p + geom_point(aes(colour = Observation))
	p <- p + scale_color_manual(values = c('blue', 'red'))
	p <- p + ylim(k$cminx, k$cmaxx)
	p <- p + xlab('Predicted Value') + ylab('Deleted Studentized Residual')
	p <- p + ggtitle("Deleted Studentized Residual vs Predicted Values")
	p <- p + geom_hline(yintercept = c(-2, 2), colour = 'red')
	p <- p + geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family="serif", fontface="italic", colour="darkred") 
	p <- p + annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2, 
                  family="serif", fontface="italic", colour="darkred", 
                  label = paste0('Threshold: abs(', 2, ')'))
	suppressWarnings(print(p))
	invisible(f)
}