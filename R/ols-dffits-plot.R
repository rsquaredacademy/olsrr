#' @importFrom stats dffits
#' @title DFFITS Plot
#' @description Plot for detecting influential observations using DFFITs.
#' @param model an object of class \code{lm}
#' @details DFFIT - difference in fits, is used to identify influential data points. It quantifies 
#' the number of standard deviations that the fitted value changes when the ith data point is omitted.
#' 
#' Steps to compute DFFITs:
#' 
#' \itemize{
#'   \item Delete observations one at a time.
#'   \item Refit the regression model on remaining \eqn{n - 1} observations
#'   \item examine how much all of the fitted values change when the ith observation is deleted.
#' }
#' 
#' An observation is deemed influential if the absolute value of its DFFITS value is greater than:
#' \deqn{2\sqrt(p + 1) / (n - p -1)}
#'
#' where n is the number of observations and p is the number of predictors including intercept.
#'
#' @return \code{ols_dffits_plot} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a tibble with observation number and \code{DFFITs} that exceed \code{threshold}}
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @references Belsley, David A.; Kuh, Edwin; Welsh, Roy E. (1980). Regression 
#' Diagnostics: Identifying Influential Data and Sources of Collinearity. 
#' Wiley Series in Probability and Mathematical Statistics. 
#' New York: John Wiley & Sons. ISBN 0-471-05856-4.
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
				    txt <- NULL
		     dbetas <- NULL
		Observation <- NULL

	            d <- tibble(obs = seq_len(n), dbetas = dffitsm)
	      d$color <- ifelse(((d$dbetas >= dffits_t) | (d$dbetas <= -dffits_t)), c("outlier"), c("normal"))
       d$color1 <- factor(d$color)
  d$Observation <- ordered(d$color1, levels = c("normal", "outlier"))
  						d <- d %>% mutate(txt = ifelse(Observation == 'outlier', obs, NA))
							f <- d %>% filter(., Observation == 'outlier') %>% select(obs, dbetas)

	p <- ggplot(d, aes(x = obs, y = dbetas, label = txt, ymin = 0, ymax = dffitsm)) +
		geom_linerange(colour = 'blue') +
		geom_hline(yintercept = c(0, dffits_t, -dffits_t), colour = 'red') +
		geom_point(colour = 'blue', shape = 1) +
		xlab('Observation') + ylab('DFFITS') +
		ggtitle(paste("Influence Diagnostics for", names(model.frame(model))[1])) +
		geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family="serif", fontface="italic", colour="darkred") +
		annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = 2, 
                  family="serif", fontface="italic", colour="darkred", 
                  label = paste('Threshold:', round(dffits_t, 2)))

	suppressWarnings(print(p))
	colnames(f) <- c("Observation", "DFFITs")
	result <- list(outliers = f, threshold = round(dffits_t, 2))
	invisible(result)
}