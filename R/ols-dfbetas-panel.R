#' DFBETAs panel
#'
#' Panel of plots to detect influential observations using DFBETAs.
#'
#' @param model An object of class \code{lm}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#'
#' @details
#' DFBETA measures the difference in each parameter estimate with and without
#' the influential point. There is a DFBETA for each data point i.e if there are
#' n observations and k variables, there will be \eqn{n * k} DFBETAs. In
#' general, large values of DFBETAS indicate observations that are influential
#' in estimating a given parameter. Belsley, Kuh, and Welsch recommend 2 as a
#' general cutoff value to indicate influential observations and
#' \eqn{2/\sqrt(n)} as a size-adjusted cutoff.
#'
#' @return list; \code{ols_plot_dfbetas} returns a list of \code{data.frame} (for intercept and each predictor)
#' with the observation number and DFBETA of observations that exceed the threshold for classifying
#' an observation as an outlier/influential observation.
#'
#' @references
#' Belsley, David A.; Kuh, Edwin; Welsh, Roy E. (1980). Regression
#' Diagnostics: Identifying Influential Data and Sources of Collinearity.
#'
#' Wiley Series in Probability and Mathematical Statistics.
#' New York: John Wiley & Sons. pp. ISBN 0-471-05856-4.
#'
#' @section Deprecated Function:
#' \code{ols_dfbetas_panel()} has been deprecated. Instead use \code{ols_plot_dfbetas()}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_plot_dfbetas(model)
#'
#' @seealso [ols_plot_dffits()]
#'
#' @importFrom stats dfbetas
#'
#' @export
#'
ols_plot_dfbetas <- function(model, print_plot = TRUE) {

  check_model(model)

  obs <- NULL
  txt <- NULL

  dfb       <- dfbetas(model)
  n         <- nrow(dfb)
  np        <- ncol(dfb)
  threshold <- 2 / sqrt(n)

  myplots   <- list()
  outliers  <- list()

  for (i in seq_len(np)) {

    dbetas  <- dfb[, i]
    df_data <- data.frame(obs = seq_len(n), dbetas = dbetas)
    d       <- ols_prep_dfbeta_data(df_data, threshold)
    f       <- ols_prep_dfbeta_outliers(d)

    p <- eval(substitute(
      ggplot(d, aes(x = obs, y = dbetas, label = txt, ymin = 0, ymax = dbetas)) +
        geom_linerange(colour = "blue") +
        geom_hline(yintercept = c(0, threshold, -threshold), colour = "red") +
        geom_point(colour = "blue", shape = 1) +
        xlab("Observation") + ylab("DFBETAS") +
        ggtitle(paste("Influence Diagnostics for", colnames(dfb)[i])) +
        geom_text(hjust = -0.2, nudge_x = 0.15, size = 2, family = "serif",
                  fontface = "italic", colour = "darkred", na.rm = TRUE) +
        annotate(
          "text", x = Inf, y = Inf, hjust = 1.5, vjust = 2,
          family = "serif", fontface = "italic", colour = "darkred",
          label = paste("Threshold:", round(threshold, 2))
        ),
      list(i = i)
    ))

    myplots[[i]]  <- p
    outliers[[i]] <- f

  }

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(list(plots = myplots, outliers = outliers))
  }

}

#' @export
#' @rdname ols_plot_dfbetas
#' @usage NULL
#'
ols_dfbetas_panel <- function(model) {
  .Deprecated("ols_plot_dfbetas()")
}

