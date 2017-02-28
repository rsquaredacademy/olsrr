#' @title Potential Residual Plot
#' @description Plot to aid in classifying unusual observations as high-leverage points,
#' outliers, or a combination of both.
#' @param model an object of class \code{lm}
#' @references Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_potrsd_plot(model)
#' @export
#'
ols_potrsd_plot <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    res <- NULL
    pot <- NULL
    d <- tibble(res = hadio(model, 3), pot = hadio(model, 2))
    p <- ggplot(d, aes(x = res, y = pot))
    p <- p + geom_point(colour = 'blue', shape = 1)
    p <- p + xlab('Residual') + ylab('Potential')
    p <- p + ggtitle('Potential-Residual Plot')
    print(p)
}


