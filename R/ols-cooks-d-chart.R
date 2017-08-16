#' @importFrom ggplot2 geom_linerange
#' @title Cooks' D Chart
#' @description Chart of cook's distance to detect observations that strongly influence fitted values of the model.
#' @param model an object of class \code{lm}
#' @details Cook's distance was introduced by American statistician R Dennis Cook in 1977. It is used 
#' to identify influential data points. It depends on both the residual and leverage i.e it takes it account
#' both the \emph{x} value and \emph{y} value of the observation.
#' 
#' Steps to compute Cook's distance:
#' 
#' \itemize{
#'   \item Delete observations one at a time.
#'   \item Refit the regression model on remaining \eqn{n - 1} observations
#'   \item exmine how much all of the fitted values change when the ith observation is deleted.
#' }
#' 
#' A data point having a large cook's d indicates that the data point strongly influences the fitted values.
#'
#' @return \code{ols_cooksd_chart} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a tibble with observation number and \code{cooks distance} that exceed \code{threshold}}
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#' 
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
                  label = paste('Threshold:', round(k$ts, 3)))
		
	suppressWarnings(print(p))
	colnames(f) <- c("Observation", "Cook's Distance")
	result <- list(outliers = f, threshold = k$ts)
	invisible(result)

}