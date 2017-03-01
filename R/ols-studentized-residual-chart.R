#' @importFrom stats rstandard
#' @title Standardized Residual Chart
#' @description Chart for identifying outliers 
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_srsd_chart(model)
#' @export
#'
ols_srsd_chart <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	    obs <- NULL
	Observation <- NULL
	  sdres <- rstandard(model)
	outlier <- sdres[abs(sdres) > 2]
	      d <- data.frame(obs = seq_len(length(sdres)), sdres = sdres)
	d$color <- ifelse(((d$sdres >= 2) | (d$sdres <= -2)), c("outlier"), c("normal"))
  d$color1 <- factor(d$color)
  d$Observation <- ordered(d$color1, levels = c("normal", "outlier"))
  d <- d %>% mutate(txt = ifelse(Observation == 'outlier', obs, NA))
	f <- d %>% filter(., Observation == 'outlier') %>% select(obs, sdres)
	p <- ggplot(d, aes(x = obs, y = sdres, label = txt, ymin = 0, ymax = sdres))
	p <- p + geom_linerange(colour = 'blue')
	p <- p + geom_point(shape = 1, colour = 'blue')
	p <- p + geom_hline(yintercept = 0, colour = 'gray')
	p <- p + geom_hline(yintercept = c(2, -2), colour = 'red')
	p <- p + xlab('Observation') + ylab('Studentized Residuals')
	p <- p + ggtitle('Standardized Residuals Chart')
	p <- p + geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family="serif", fontface="italic", colour="darkred") 
	p <- p + annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2, 
                  family="serif", fontface="italic", colour="darkred", 
                  label = paste0('Threshold: abs(', 2, ')'))
	suppressWarnings(print(p))
	invisible(f)
}