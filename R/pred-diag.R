#' @importFrom stats model.frame model.response
#' @importFrom ggplot2 geom_dotplot geom_histogram
#' @title Predictor Diagnostics
#' @description Predictor Diagnostics
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @export
#'
pred_diag <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

          nam <- names(model.frame(model))
    predictor <- model.response(model.frame(model))
         xval <- predictor %>% length() %>% seq_len()
            x <- NULL
            y <- NULL


    d1 <- tibble(x = predictor)
    p1 <- ggplot(d1, aes(x = x)) +
        geom_dotplot(binwidth = 1, fill = 'blue') +
        xlab(nam[1]) + ggtitle(paste('Dot Plot of', nam[1]))



    d2 <- tibble(x = xval, y = predictor)
    p2 <- ggplot(d2, aes(x = x, y = y)) +
        geom_point(color = 'blue') +
        geom_line(color = 'blue') +
        xlab('Observation') + ylab(nam[1]) +
        ggtitle(paste('Trend Plot of', nam[1]))


    d3 <- tibble(x = predictor)
    p3 <- ggplot(d3, aes(x = x)) +
        geom_histogram(bins = 5, color = 'black', fill = 'blue') +
        xlab(nam[1]) + ggtitle(paste('Histogram of', nam[1]))


    d4 <- tibble(x = predictor)
    p4 <- ggplot(d4, aes(x = factor(0), y = x)) +
        geom_boxplot(fill = 'blue') +
        xlab('') + ylab(nam[1]) +
        ggtitle(paste('Boxplot of', nam[1])) +
        theme(axis.text.x = element_blank())

    grid.arrange(d1, d2, d3, d4, ncol = 2, top = 'Response Diagnostics')

}


# pred_diag <- function(model) {
#
#     if (!all(class(model) == 'lm')) {
#         stop('Please specify a OLS linear regression model.', call. = FALSE)
#     }
#
# 	  nam       <- names(model.frame(model))
# 	  predictor <- model.response(model.frame(model))
#     n         <- length(predictor)
#     tab       <- table(predictor)
#     counts    <- unlist(sapply(tab, seq_len))
#     height    <- max(tab) * 1.5
#     op        <- par(no.readonly = TRUE)
#
#     plot(sort(predictor), counts, ylim = c(0, height), col = "blue",
#     	xlab = nam[1], ylab = "Counts", main = paste("Dotchart of", nam[1]))
#
#     plot(predictor, type = "b", xlab = "Observation", col = "blue",
# 		ylab = nam[1], main = paste("Scatter Plot of", nam[1]))
#
#     h  <- hist(predictor, plot = F)
#     yl <- max(h$counts) + 0.3 * max(h$counts)
#     yp <- h$counts + 1
#
#     hist(predictor, col = "blue", xlab = nam[1],
#     	ylim = c(0, yl), main = paste("Histogram of", nam[1]))
#     text(x = h$mids, y = yp, labels = h$counts)
#
#     k <- boxplot(predictor, ylab = nam[1], col = "blue",
# 	        outcol = "red", main = paste("Boxplot of", nam[1]))
# 	  vals  <- round(k$stats, 2)
# 	  leg_t <- paste("Min:", vals[1], "  Q1:", vals[2],
# 		"  Median:", vals[3], "  Q3:", vals[4], "  Max:", vals[5])
# 	  mtext(leg_t, side = 1, cex = 0.5)
#
# }
