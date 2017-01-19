#' @importFrom stats dfbetas
#' @title DFBETAS Panel
#' @description DFBETAS Panel
#' @param model an object of class \code{lm}
#' @return \code{dfbetas_panel} returns a list containing the
#' following components:
#'
#' \item{dfbetas}{dfbetas}
#' \item{threshold}{threshold for outliers}
#' \item{outliers}{residual points that are outliers}
#' @export
#'
dfbetas_panel <- function(model) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	dfb       <- dfbetas(model)
	n         <- nrow(dfb)
	np        <- ncol(dfb)
	threshold <- 2 / sqrt(n)

	for (i in seq_len(np)) {

		dbetas <- dfb[, i]

		d <- tibble(obs = seq_len(n), dbetas = dbetas) %>%
			ggplot(., aes(obs, dbetas, ymin = 0, max = dbetas)) +
			geom_linerange(colour = 'blue') +
			geom_hline(yintercept = c(0, threshold, -threshold), colour = 'red') +
			geom_point(colour = 'blue', shape = 1) +
			xlab('Observation') + ylab('DFBETAS') +
			ggtitle(paste("Influence Diagnostics for", colnames(dfb)[i]))

		print(d)
	}
}


# dfbetas_panel <- function(model) {
#
# 	if (!all(class(model) == 'lm')) {
#     stop('Please specify a OLS linear regression model.', call. = FALSE)
#   }
#
# 	dfb       <- dfbetas(model)
# 	n         <- nrow(dfb)
# 	np        <- ncol(dfb)
# 	threshold <- 2 / sqrt(n)
# 	obs_l     <- list()
#
# 	for (i in seq_len(np)) {
#
# 		dbetas  <- dfb[, i]
# 		outlier <- dbetas[abs(dbetas) > threshold]
# 		obs     <- length(outlier)
# 		obs_n   <- c()
#
# 		for (j in seq_len(obs)) {
# 			obs_n[j] <- which(dbetas == outlier[j])
# 		}
#
# 		obs_l[[i]] <- obs_n
# 		ymin       <- min(dbetas)
# 		ymax       <- max(dbetas)
# 		yminn      <- ifelse(ymin < -threshold, ymin, -threshold)
# 		ymaxx      <- ifelse(ymax > threshold, ymax, threshold)
#
# 		plot(seq_len(length(dbetas)), dbetas, type = "h", lwd = 1, col = "blue",
# 		     xlab = "Observation", ylab = "DFBETAS",
# 		     ylim = c(yminn, ymaxx),
# 		     main = paste("Influence Diagnostics for", colnames(dfb)[i]))
# 		points(dbetas, col = "blue")
# 		abline(h = 0, col = "blue")
# 		abline(h = -threshold)
# 		abline(h = threshold)
#
# 		if(obs > 0) {
# 		    text(jitter(obs_n), outlier, obs_n, cex = 0.8)
# 		}
#
# 	}
#
# 	names(obs_l) <- names(dfb)
#
# 	z <- list(dfbetas   = dfb,
# 						threshold = threshold,
# 						outliers  = obs_l)
#
# }

# dfbetas panel
# dfbp <- function(dfb) {
#   d <- dfb %>% `[`(, 1) %>%
#     `[`(abs(dbetas) > threshold) %>%
#     length()
#   return(d)
# }


# obs <- dfb %>% `[`(, 1) %>%
#     `[`(abs(dbetas) > threshold) %>%
#     length()
#
# obs <- dfb %>% `[`(, 1) %>%
#     abs() %>%
#     `>`(threshold) %>%
#     match(TRUE) %>%
#     na.omit() %>%
#     length()
