#' @title Potential Residual Plot
#' @description Potential Residual Plot
#' @param model an object of class \code{lm}
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return SP
#' @export
#'
poten_resid_plot <- function(model) {

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


# poten_resid_plot <- function(model) {
#
#     if (!all(class(model) == 'lm')) {
#         stop('Please specify a OLS linear regression model.', call. = FALSE)
#     }
#
#     hi  <- hadi(model)
#     pot <- unname(hi$potential)
#     res <- unname(hi$residual)
#     plot(res, pot, col = "blue", xlab = "Residual", ylab = "Potential",
#          main = "Potential-Residual Plot")
#
# }
