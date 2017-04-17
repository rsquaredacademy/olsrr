#' @importFrom stats dffits
#' @title DFFITS Plot
#' @description Plot for detecting influential observations using DFFITS.
#' @param model an object of class \code{lm}
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
	invisible(f)
}