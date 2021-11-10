#' Residual plus component plot
#'
#' @description
#' The residual plus component plot indicates whether any non-linearity is
#' present in the relationship between response and predictor variables and can
#' suggest possible transformations for linearizing the data.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_plot_comp_plus_resid(model)
#'
#' @seealso [ols_plot_added_variable()], [ols_plot_resid_regressor()]
#'
#' @export
#'
ols_plot_comp_plus_resid <- function(model, print_plot = TRUE) {

  check_model(model)

  pl      <- cpout(model)
  myplots <- list()

  for (i in seq_len(pl$lmc)) {
    k <- cpdata(pl$data, pl$mc, pl$e, i)
    p <- 
      eval(
        substitute(
          ggplot(k, aes(x = x, y = y)) +
          geom_point(colour = "blue", size = 2) + 
          xlab(pl$nam[i]) +
          ylab(paste0("Residual + Component (", pl$indvar, ")")) +
          stat_smooth(method = "lm", se = FALSE), list(i = i)
        )
      )

    myplots[[i]] <- p
  }

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}


cpdata <- function(data, mc, e, i) {

  x <- data[[i]]
  y <- ((mc[i] * data[i]) + e)[[1]]

  data.frame(x = x, y = y)

}

cpout <- function(model) {

  e      <- residuals(model)
  mc     <- coefficients(model)[-1]
  data   <- as.data.frame(model.matrix(model))[, -1]
  lmc    <- length(mc)
  nam    <- names(data)
  indvar <- names(model.frame(model))[1]

  list(e      = e,
       mc     = mc,
       data   = data,
       lmc    = lmc,
       nam    = nam,
       indvar = indvar)

}