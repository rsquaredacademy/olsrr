#' @importFrom stats model.frame model.response
#' @importFrom ggplot2 geom_dotplot geom_histogram
#' @title Visualize Response Variable 
#' @description Panel of plots to explore and visualize the response variable.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_resp_viz(model)
#' @export
#'
ols_resp_viz <- function(model) {

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

    grid.arrange(p1, p2, p3, p4, ncol = 2, top = 'Response Diagnostics')

}