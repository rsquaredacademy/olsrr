#' ANOVA
#'
#' ANOVA for regression model.
#'
#' @param model An object of class \code{lm}.
#' @param ... Other arguments.
#'
#' @return \code{ols_anova} returns an object of class \code{"ols_anova"}.
#' An object of class \code{"ols_anova"} is a list containing the following
#' components:
#'
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
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#'
#' # anova
#' ols_anova(model)
#'
#' @export
#'
ols_anova <- function(model, ...) UseMethod("ols_anova")

#' @export
#'
ols_anova.default <- function(model, ...) {
  anovam   <- anova(model)
  n        <- length(anovam$Df)
  error_df <- anovam$Df[n]
  model_df <- sum(anovam$Df) - error_df
  total_df <- sum(anovam$Df)
  ess      <- anovam$`Sum Sq`[n]
  tss      <- sum(anovam$`Sum Sq`)
  rss      <- tss - ess
  rms      <- rss / model_df
  ems      <- ess / error_df
  f        <- rms / ems
  p        <- pf(f, model_df, error_df, lower.tail = F)

  result <-
    list(
      error_df = error_df,
      model_df = model_df,
      total_df = total_df,
      ess      = ess,
      rss      = rss,
      tss      = tss,
      rms      = rms,
      ems      = ems,
      f        = f,
      p        = p
    )

  class(result) <- "ols_anova"

  return(result)
}

#' @export
#'
print.ols_anova <- function(x, ...) {
  print_anova(x)
}
