#' Added variable plots
#'
#' @description
#' Added variable plot provides information about the marginal importance of a
#' predictor variable, given the other predictor variables already in
#' the model. It shows the marginal importance of the variable in reducing the
#' residual variability.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details The added variable plot was introduced by Mosteller and Tukey
#' (1977). It enables us to visualize the regression coefficient of a new
#' variable being considered to be included in a model. The plot can be
#' constructed for each predictor variable.
#'
#' Let us assume we want to test the effect of adding/removing variable \emph{X} from a
#' model. Let the response variable of the model be \emph{Y}
#'
#' Steps to construct an added variable plot:
#'
#' \itemize{
#'   \item Regress \emph{Y} on all variables other than \emph{X} and store the residuals (\emph{Y} residuals).
#'   \item Regress \emph{X} on all the other variables included in the model (\emph{X} residuals).
#'   \item Construct a scatter plot of \emph{Y} residuals and \emph{X} residuals.
#' }
#'
#' What do the \emph{Y} and \emph{X} residuals represent? The \emph{Y} residuals represent the part
#' of \strong{Y} not explained by all the variables other than X. The \emph{X} residuals
#' represent the part of \strong{X} not explained by other variables. The slope of the line
#' fitted to the points in the added variable plot is equal to the regression
#' coefficient when \strong{Y} is regressed on all variables including \strong{X}.
#'
#' A strong linear relationship in the added variable plot indicates the increased
#' importance of the contribution of \strong{X} to the model already containing the
#' other predictors.
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_plot_added_variable(model)
#'
#' @importFrom stats lm
#' @importFrom ggplot2 ggplot aes geom_point xlab ylab stat_smooth
#' @importFrom gridExtra marrangeGrob
#'
#' @seealso [ols_plot_resid_regressor()], [ols_plot_comp_plus_resid()]
#'
#' @export
#'
ols_plot_added_variable <- function(model, print_plot = TRUE) {

  check_model(model)

  data    <- ols_prep_avplot_data(model)
  xnames  <- colnames(data)
  nl      <- length(xnames)
  myplots <- list()

  for (i in 2:nl) {

    x <- ols_prep_regress_x(data, i)
    y <- ols_prep_regress_y(data, i)
    d <- data.frame(x, y)

    p <- eval(substitute(ggplot(d, aes(x = x, y = y)) +
      geom_point(colour = "blue", size = 2) +
      xlab(paste(xnames[i], " | Others")) +
      ylab(paste(xnames[1], " | Others")) +
      stat_smooth(method = "lm", se = FALSE), list(i = i)))

    j <- i - 1
    myplots[[j]] <- p

  }

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}

#' Remove columns
#'
#' Removes columns and returns a matrix.
#'
#' @param data A `data.frame`.
#' @param i A numeric vector of length 1.
#'
#' @noRd
#'
remove_columns <- function(data, i) {
	as.matrix(data[, c(-1, -i)])
}

#' Select columns
#'
#' Select column and return as matrix.
#'
#' @param data A `data.frame`.
#' @param i A numeric vector of length 1.
#'
#' @noRd
#'
select_columns <- function(data, i = 1) {
	as.matrix(data[, i])
}
