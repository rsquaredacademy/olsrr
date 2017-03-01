#' @importFrom ggplot2 scale_fill_manual annotate
#' @title Studentized Residual Plot
#' @description Graph for identifying outliers
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_srsd_plot(model)
#' @export
#'
ols_srsd_plot <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	obs <- NULL
	dsr <- NULL
	Observation <- NULL
	g <- srdata(model)
	d <- g$dsr
	d <- d %>% mutate(txt = ifelse(Observation == 'outlier', obs, NA))
	f <- d %>% filter(., Observation == 'outlier') %>% select(obs, dsr)
	p <- ggplot(d, aes(x = obs, y = dsr, label = txt))
	p <- p + geom_bar(width = 0.5, stat = 'identity', aes(fill = Observation))
	p <- p + scale_fill_manual(values = c('blue', 'red'))
	p <- p + ylim(g$cminx, g$cmaxx)
	p <- p + coord_flip()
	p <- p + xlab('Observation') + ylab('Deleted Studentized Residuals')
	p <- p + ggtitle('Studentized Residuals')
	p <- p + geom_hline(yintercept = c(g$cminx, g$cmaxx), color = 'red')
	p <- p + geom_hline(yintercept = c(0, g$nseq, g$pseq))
	p <- p + geom_text(hjust = -0.2, nudge_x = 0.05, size = 2)
	p <- p + annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2, 
                  family="serif", fontface="italic", colour="darkred", 
                  label = paste0('Threshold: abs(', 3, ')'))
	suppressWarnings(print(p))
	colnames(f) <- c("Observation", "Studentized Residuals")
	invisible(f)

}