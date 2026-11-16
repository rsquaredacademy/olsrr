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
#' @details The plot is not available for weighted models.
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
  check_weights_diagnostic(model, "ols_plot_comp_plus_resid")

  pl      <- cpout(model)
  myplots <- list()

  for (i in seq_len(pl$lmc)) {
    k <- cpdata(pl$data, pl$mc, pl$e, i)
    p <-
      eval(
        substitute(
          ggplot(k, aes(x = x, y = y)) +
          geom_point(colour = "blue", size = 2) +
          stat_smooth(method = "lm", se = FALSE) +
          xlab(pl$nam[i]) +
          ylab(paste0("Residual + Component (", pl$indvar, ")")),
          list(i = i)
        )
      )

    myplots[[i]] <- p
  }

  if (print_plot) {
    g <- marrangeGrob(myplots, nrow = 2, ncol = 2, top = "Residual Plus Component Plot")
    grid::grid.draw(g)
    invisible(g)
  } else {
    return(myplots)
  }

}


cpdata <- function(data, mc, e, i) {

  x <- data[[i]]
  y <- (mc[i] * data[[i]]) + e
  data.frame(x = x, y = y)

}

cpout <- function(model) {

  e      <- model_residuals_used(model)
  X      <- model.matrix(model)
  keep   <- setdiff(colnames(X), "(Intercept)")
  mc     <- stats::coef(model)[keep]
  data   <- as.data.frame(X[, keep, drop = FALSE])
  lmc    <- length(mc)
  nam    <- keep
  indvar <- names(model.frame(model))[1]

  list(e = e, mc = mc, data = data, lmc = lmc, nam = nam, indvar = indvar)

}
