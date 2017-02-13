#' @title Hadi Plot
#' @description Hadi's measure of influence based on the fact that influential observations can be present in either
#' the response variable or in the predictors or both. The plot is used to detect influential observations
#' based on Hadi's measure.
#' @param model an object of class \code{lm}
#' @references Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' hadi_plot(model)
#' @export
#'
hadi_plot <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    obs <- NULL
    hdi <- model %>% hadi() %>% `$`(hadi) %>% unname()
      d <- tibble(obs = seq_len(length(hdi)), hdi = hdi)
      p <- ggplot(d, aes(obs, hdi, ymin = min(hdi), ymax = hdi)) +
        geom_linerange(colour = 'blue') +
        geom_point(shape = 1, colour = 'blue') +
        xlab('Observation') + ylab("Hadi's Measure") +
        ggtitle("Hadi's Influence Measure")

    print(p)
}

# hadi_plot <- function(model) {
#
#     if (!all(class(model) == 'lm')) {
#         stop('Please specify a OLS linear regression model.', call. = FALSE)
#     }
#
#     hi  <- hadi(model)
#     hdi <- unname(hi$hadi)
#     plot(seq_len(length(hdi)), hdi, type = "h", lwd = 1, col = "blue",
#          xlab = "Observation", ylab = "Hadi's Measure",
#          main = "Hadi's Influence Measure")
#     points(hdi, col = "blue")
#
# }
