#' Residual vs regressor plot
#'
#' @description
#' Graph to determine whether we should add a new predictor to the model already
#' containing other predictors. The residuals from the model is regressed on the
#' new predictor and if the plot shows non random pattern, you should consider
#' adding the new predictor to the model.
#'
#' @param model An object of class \code{lm}.
#' @param variable New predictor to be added to the \code{model}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_rvsr_plot(model, drat)
#'
#' @seealso [ols_avplots()], [ols_rpc_plot()]
#'
#' @export
#'
ols_rvsr_plot <- function(model, variable) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  x <- NULL
  y <- NULL

  d        <- rvsrdata(model)
  varyable <- enquo(variable)

  inter <-
    eval(model$call$data) %>%
    select(!! varyable)

  x <- pull(inter, 1)
  y <- residuals(model)
  v <- names(x)
  k <- tibble(x = x, y = y)

  p <- ggplot(k, aes(x = x, y = y)) +
    geom_point(shape = 1, colour = "blue") +
    xlab(paste(v)) + ylab("Residual") +
    ggtitle(paste("Residual vs", v)) +
    geom_hline(yintercept = 0, colour = "red")

  print(p)

}

rvsrdata <- function(model) {

  np <-
    model %>%
    coefficients() %>%
    length() %>%
    subtract(1)

  dat <-
    model %>%
    model.frame() %>%
    select(-1)

  pnames <-
    model %>%
    coefficients() %>%
    names() %>%
    extract(-1)

  list(np = np, dat = dat, pnames = pnames)

}
