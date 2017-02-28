#' @title Ordinary Least Squares Regression
#' @description Ordinary Least Squares Regression
#' @param object an object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted.
#' @param ... other inputs
#' @return \code{ols_regress} returns an object of class \code{"ols_regress"}. An object
#' of class \code{"ols_regress"} is a list containing the following
#' components:
#'
#' \item{r}{f statistic}
#' \item{rsq}{pure error}
#' \item{adjr}{regression sum of squares}
#' \item{sigma}{error sum of squares}
#' \item{cv}{total sum of squares}
#' \item{mse}{mean squared error}
#' \item{mae}{p-value of \code{fstat}}
#' \item{aic}{degrees of freedom}
#' \item{sbc}{name(s) of \code{variable}}
#' \item{sbic}{name of \code{group_var}}
#' \item{prsq}{f statistic}
#' \item{error_df}{p-value of \code{fstat}}
#' \item{model_df}{degrees of freedom}
#' \item{total_df}{name(s) of \code{variable}}
#' \item{ess}{name of \code{group_var}}
#' \item{rss}{f statistic}
#' \item{tss}{p-value of \code{fstat}}
#' \item{rms}{degrees of freedom}
#' \item{ems}{name(s) of \code{variable}}
#' \item{f}{name of \code{group_var}}
#' \item{p}{name of \code{group_var}}
#' \item{n}{p-value of \code{fstat}}
#' \item{betas}{degrees of freedom}
#' \item{sbetas}{name(s) of \code{variable}}
#' \item{std_errors}{name of \code{group_var}}
#' \item{tvalues}{f statistic}
#' \item{pvalues}{p-value of \code{fstat}}
#' \item{df}{degrees of freedom}
#' \item{conf_lm}{name(s) of \code{variable}}
#' \item{title}{name of \code{group_var}}
#' \item{dependent}{f statistic}
#' \item{predictors}{p-value of \code{fstat}}
#' \item{mvars}{degrees of freedom}
#' \item{model}{name(s) of \code{variable}}
#' @examples
#' ols_regress(mpg ~ disp + hp + wt, data = mtcars)
#' @export
#'
ols_regress <- function(object, ...) UseMethod('ols_regress')

#' @export
#'
ols_regress.default <- function(object, data, conf.level = 0.95, title = 'model', ...) {


  if (missing(data)) {
    stop('data missing', call. = FALSE)
  }

  if(!is.data.frame(data)) {
    stop('data must be a data frame', call. = FALSE)
  }

  if (!is.numeric(conf.level)) {
    stop('conf.level must be numeric', call. = FALSE)
  }

  if ((conf.level < 0) | (conf.level > 1)) {
    stop('conf.level must be between 0 and 1', call. = FALSE)
  }

  if (!is.character(title)) {
    stop(paste(title, 'is not a string, Please specify a string as title.'), call. = FALSE)
  }

  result        <- reg_comp(object, data, conf.level, title)
  class(result) <- 'ols_regress'
  return(result)

}

#' @rdname ols_regress
#' @export
#'
ols_regress.lm <- function(object, ...) {
    formula <- formula(object)
    data    <- model.frame(object)
    ols_regress.default(object = formula, data = data)
}

#' @export
#'
print.ols_regress <- function(x, ...) {
    print_reg(x)
}
