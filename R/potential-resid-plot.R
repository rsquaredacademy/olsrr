#' @title Potential Residual Plot
#' @description Plot to aid in classifying unusual observations as high-leverage points,
#' outliers, or a combination of both.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' poten_resid_plot(model)
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
