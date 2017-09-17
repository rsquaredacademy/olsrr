#' @importFrom stats rstandard
#' @title Standardized Residual Chart
#' @description Chart for identifying outliers 
#' @param model an object of class \code{lm}
#' @details Standardized residual (internally studentized) is the residual divided by estimated 
#' standard deviation.
#' @return \code{ols_srsd_chart} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a tibble with observation number and \code{standardized resiudals} that 
#' exceed \code{threshold}} for classifying an observation as an outlier
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#' 
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
	  			  txt <- NULL
		Observation <- NULL
	  			sdres <- rstandard(model)
				outlier <- sdres[abs(sdres) > 2]
	            d <- data.frame(obs = seq_len(length(sdres)), sdres = sdres)
	      d$color <- ifelse(((d$sdres >= 2) | (d$sdres <= -2)), c("outlier"), c("normal"))
       d$color1 <- factor(d$color)
  d$Observation <- ordered(d$color1, levels = c("normal", "outlier"))
						  d <- d %>% mutate(txt = ifelse(Observation == 'outlier', obs, NA))
							f <- d %>% filter(., Observation == 'outlier') %>% select(obs, sdres)

	p <- ggplot(d, aes(x = obs, y = sdres, label = txt, ymin = 0, ymax = sdres)) +
		geom_linerange(colour = 'blue') +
		geom_point(shape = 1, colour = 'blue') +
		geom_hline(yintercept = 0, colour = 'gray') +
		geom_hline(yintercept = c(2, -2), colour = 'red') +
		xlab('Observation') + ylab('Studentized Residuals') +
		ggtitle('Standardized Residuals Chart') +
		geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family="serif", fontface="italic", colour="darkred", na.rm = TRUE) +
		annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2, 
      family="serif", fontface="italic", colour="darkred", 
      label = paste0('Threshold: abs(', 2, ')'))

	suppressWarnings(print(p))
	colnames(f) <- c("Observation", "Studentized Residual")
	result <- list(outliers = f, threshold = 2, plot = p)
	invisible(result)
}