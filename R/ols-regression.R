#' Ordinary least squares regression
#'
#' @description Ordinary least squares regression.
#'
#' @param object An object of class "formula" (or one that can be coerced to
#'   that class): a symbolic description of the model to be fitted or class
#'   \code{lm}.
#'
#' @param conf.level Confidence level for the confidence intervals of the
#'   coefficients.
#' @param iterm Logical; if \code{TRUE}, the predictors are scaled and centered
#'   before computing the standardized betas. See \emph{Interaction Terms}
#'   below.
#' @param title Character; title of the model, printed as the header of the
#'   parameter estimates table.
#'
#' @param ... Other inputs.
#'
#' @return \code{ols_regress} returns an object of class \code{"ols_regress"}.
#' An object of class \code{"ols_regress"} is a list containing the following
#' components:
#'
#' \item{r}{square root of rsquare, correlation between observed and predicted values of dependent variable}
#' \item{rsq}{coefficient of determination or r-square}
#' \item{adjr}{adjusted rsquare}
#' \item{rmse}{root mean squared error}
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
#'
#' @section Interaction Terms:
#' If the model includes interaction terms, the standardized betas
#' are computed after scaling and centering the predictors (with the model's
#' prior weights when the model is weighted) and refitting the model.
#'
#' @section Weighted Models:
#' Models fitted with \code{lm(..., weights = ...)} are supported. The reported
#' estimates, standard errors, test statistics, confidence intervals and
#' R-squared match \code{summary()}, \code{anova()} and \code{confint()}.
#' Weights cannot be supplied through the formula interface; fit the model with
#' \code{lm()} and pass the fitted model instead. Objects of class \code{glm}
#' are not supported; use \code{lm()} or the formula interface.
#'
#' @references https://www.ssc.wisc.edu/~hemken/Stataworkshops/stdBeta/Getting%20Standardized%20Coefficients%20Right.pdf
#'
#' Kutner, M. H., Nachtsheim, C. J., Neter, J. and Li, W. (2005). \emph{Applied Linear Statistical Models}, 5th ed. New York: McGraw-Hill/Irwin.
#'
#' Aiken, L. S. and West, S. G. (1991). \emph{Multiple Regression: Testing and Interpreting Interactions}. Newbury Park, CA: Sage.
#'
#' @examples
#' ols_regress(mpg ~ disp + hp + wt, data = mtcars)
#'
#' # if model includes interaction terms set iterm to TRUE
#' ols_regress(mpg ~ disp * wt, data = mtcars, iterm = TRUE)
#'
#' @export
#'
ols_regress <- function(object, ...) UseMethod("ols_regress")

#' @export
#'
ols_regress.default <- function(object, data, conf.level = 0.95,
                                iterm = FALSE, title = "model", ...) {
  if ("weights" %in% names(match.call(expand.dots = FALSE)$...)) {
    stop("weights are not supported in ols_regress when using a formula; fit the model with lm() and pass the model instead.", call. = FALSE)
  }

  if (missing(data)) {
    stop("data missing", call. = FALSE)
  }

  if (!is.numeric(conf.level)) {
    stop("conf.level must be numeric", call. = FALSE)
  }

  if ((conf.level < 0) | (conf.level > 1)) {
    stop("conf.level must be between 0 and 1", call. = FALSE)
  }

  check_logic(iterm)

  if (!is.character(title)) {
    stop(paste(title, "is not a string, Please specify a string as title."), call. = FALSE)
  }

  # detect if model formula includes interaction terms
  if (inherits(object, "formula")) {
    detect_iterm <- grepl(object, pattern = "\\*")[3]
  } else {
    detect_iterm <-  grepl(object, pattern = "\\*")
  }

  # set interaction to TRUE if formula contains interaction terms
  if (detect_iterm) {
    iterm <- TRUE
  }

  result <- reg_comp(object, data, conf.level, iterm, title)
  class(result) <- "ols_regress"
  return(result)
}

#' @rdname ols_regress
#' @export
#'
ols_regress.lm <- function(object, conf.level = 0.95, iterm = FALSE,
                           title = "model", ...) {

  check_model(object)

  if (!is.null(object$family)) {
    stop("ols_regress() supports only lm models; fit the model with lm() or use the formula interface instead.", call. = FALSE)
  }

  formula <- formula(object)
  data    <- ols_model_data(object)

  if (!is.numeric(conf.level)) {
    stop("conf.level must be numeric", call. = FALSE)
  }

  if ((conf.level < 0) | (conf.level > 1)) {
    stop("conf.level must be between 0 and 1", call. = FALSE)
  }

  check_logic(iterm)

  if (!is.character(title)) {
    stop(paste(title, "is not a string, Please specify a string as title."), call. = FALSE)
  }

  if (inherits(formula, "formula")) {
    detect_iterm <- grepl(formula, pattern = "\\*")[3]
  } else {
    detect_iterm <- grepl(formula, pattern = "\\*")
  }

  if (detect_iterm) {
    iterm <- TRUE
  }

  result <- reg_comp(formula, data, conf.level, iterm, title, model = object)
  class(result) <- "ols_regress"
  return(result)

}

#' @export
#'
print.ols_regress <- function(x, ...) {
  print_reg(x)
}
