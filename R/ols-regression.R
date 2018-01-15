#' @importFrom stringr str_detect
#' @importFrom magrittr %>% extract
#' @importFrom rlang is_formula
#' @title Ordinary Least Squares Regression
#' @description Ordinary Least Squares Regression
#' @param object an object of class "formula" (or one that can be coerced to
#' that class): a symbolic description of the model to be fitted or class \code{lm}
#' @param ... other inputs
#' @return \code{ols_regress} returns an object of class \code{"ols_regress"}. An object
#' of class \code{"ols_regress"} is a list containing the following components:
#'
#' \item{r}{square root of rsquare, correlation between observed and predicted values of dependent variable}
#' \item{rsq}{coefficient of determination or r-square}
#' \item{adjr}{adjusted rsquare}
#' \item{sigma}{root mean squared error}
#' \item{cv}{coefficient of variation}
#' \item{mse}{mean squared error}
#' \item{mae}{mean absolute error}
#' \item{aic}{akaike information criteria}
#' \item{sbc}{bayesian information criteria}
#' \item{sbic}{sawa bayesian information criteria}
#' \item{prsq}{predicted rsquare}
#' \item{error_df}{residual degrees of freedom}
#' \item{model_df}{regression degrees of freedom}
#' \item{total_df}{total degrees of freedom}
#' \item{ess}{error sum of squares}
#' \item{rss}{regression sum of squares}
#' \item{tss}{total sum of squares}
#' \item{rms}{regression mean square}
#' \item{ems}{error mean square}
#' \item{f}{f statistis}
#' \item{p}{p-value for \code{f}}
#' \item{n}{number of predictors including intercept}
#' \item{betas}{betas; estimated coefficients}
#' \item{sbetas}{standardized betas}
#' \item{std_errors}{standard errors}
#' \item{tvalues}{t values}
#' \item{pvalues}{p-value of \code{tvalues}}
#' \item{df}{degrees of freedom of \code{betas}}
#' \item{conf_lm}{confidence intervals for coefficients}
#' \item{title}{title for the model}
#' \item{dependent}{character vector; name of the dependent variable}
#' \item{predictors}{character vector; name of the predictor variables}
#' \item{mvars}{character vector; name of the predictor variables including intercept}
#' \item{model}{input model for \code{ols_regress}}
#' @section Interaction Terms:
#' If the model includes interaction terms, the standardized betas
#' are computed after scaling and centering the predictors.
#' @references https://www.ssc.wisc.edu/~hemken/Stataworkshops/stdBeta/Getting%20Standardized%20Coefficients%20Right.pdf
#' @examples
#' ols_regress(mpg ~ disp + hp + wt, data = mtcars)
#'
#' # if model includes interaction terms set iterm to TRUE
#' ols_regress(mpg ~ disp * wt, data = mtcars, iterm = TRUE)
#' @export
#'
ols_regress <- function(object, ...) UseMethod('ols_regress')

#' @export
#'
ols_regress.default <- function(object, data, conf.level = 0.95,
                                iterm = FALSE, title = 'model', ...) {


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

  # detect if model formula includes interaction terms
  if (is_formula(object)) {
    detect_iterm <- object %>%
      str_detect(pattern = '\\*') %>%
      extract(3)
  } else {
    detect_iterm <- object %>%
      str_detect(pattern = '\\*')
  }

  # set interaction to TRUE if formula contains interaction terms
  if (detect_iterm) {
    iterm <- TRUE
  }

  result        <- reg_comp(object, data, conf.level, iterm, title)
  class(result) <- 'ols_regress'
  return(result)

}

#' @rdname ols_regress
#' @export
#'
ols_regress.lm <- function(object, ...) {
    formula <- formula(object)
    # data    <- model.frame(object)
    data <- eval(object$call$data)
    ols_regress.default(object = formula, data = data)
}

#' @export
#'
print.ols_regress <- function(x, ...) {
    print_reg(x)
}
