#' Stepwise AIC backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on akaike information criterion, in a stepwise
#' manner until there is no variable left to remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_backward_*}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param digits Number of decimal places to display.
#' @param ... Other arguments.
#'
#' @return List containing the following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#' \item{others}{list; info used for plotting and printing}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward_aic(model)
#'
#' # stepwise backward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_backward_aic(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' # include or exclude variable
#' # force variables to be included in the selection process
#' ols_step_backward_aic(model, include = c("alc_mod", "gender"))
#'
#' # use index of variable instead of name
#' ols_step_backward_aic(model, include = c(7, 6))
#'
#' # force variable to be excluded from selection process
#' ols_step_backward_aic(model, exclude = c("alc_heavy", "bcs"))
#'
#' # use index of variable instead of name
#' ols_step_backward_aic(model, exclude = c(8, 1))
#'
#' @importFrom ggplot2 geom_text
#' @importFrom utils tail
#'
#' @family backward selection procedures
#'
#' @export
#'
ols_step_backward_aic <- function(model, ...) UseMethod("ols_step_backward_aic")

#' @export
#' @rdname ols_step_backward_aic
#'
ols_step_backward_aic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_backward(model, "aic", include, exclude, progress, details)
  class(out) <- "ols_step_backward_aic"
  return(out)
}

#' @export
#'
print.ols_step_backward_aic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "backward")
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_backward_aic
#' @export
#'
plot.ols_step_backward_aic <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {
  p <- ols_stepaic_plot(x, details, digits)
  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}

#' Stepwise SBC backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on schwarz bayesian criterion, in a stepwise
#' manner until there is no variable left to remove any more.
#'
#' @inheritParams ols_step_backward_aic
#'
#' @inherit ols_step_backward_aic return references
#'
#' @examples
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward_sbc(model)
#'
#' # stepwise backward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_backward_sbc(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' # include or exclude variable
#' # force variables to be included in the selection process
#' ols_step_backward_sbc(model, include = c("alc_mod", "gender"))
#'
#' # use index of variable instead of name
#' ols_step_backward_sbc(model, include = c(7, 6))
#'
#' # force variable to be excluded from selection process
#' ols_step_backward_sbc(model, exclude = c("alc_heavy", "bcs"))
#'
#' # use index of variable instead of name
#' ols_step_backward_sbc(model, exclude = c(8, 1))
#'
#' @family backward selection procedures
#'
#' @export
#'
ols_step_backward_sbc <- function(model, ...) UseMethod("ols_step_backward_sbc")

#' @export
#' @rdname ols_step_backward_sbc
#'
ols_step_backward_sbc.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_backward(model, "sbc", include, exclude, progress, details)
  class(out) <- "ols_step_backward_sbc"
  return(out)
}

#' @export
#'
print.ols_step_backward_sbc <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "backward")
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_backward_sbc
#' @export
#'
plot.ols_step_backward_sbc <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {
  p <- ols_stepaic_plot(x, details, digits)
  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}

#' Stepwise SBIC backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on sawa bayesian criterion, in a stepwise
#' manner until there is no variable left to remove any more.
#'
#' @inheritParams ols_step_backward_aic
#'
#' @inherit ols_step_backward_aic return references
#'
#' @examples
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward_sbic(model)
#'
#' # stepwise backward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_backward_sbic(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' # include or exclude variable
#' # force variables to be included in the selection process
#' ols_step_backward_sbic(model, include = c("alc_mod", "gender"))
#'
#' # use index of variable instead of name
#' ols_step_backward_sbic(model, include = c(7, 6))
#'
#' # force variable to be excluded from selection process
#' ols_step_backward_sbic(model, exclude = c("alc_heavy", "bcs"))
#'
#' # use index of variable instead of name
#' ols_step_backward_sbic(model, exclude = c(8, 1))
#'
#' @family backward selection procedures
#'
#' @export
#'
ols_step_backward_sbic <- function(model, ...) UseMethod("ols_step_backward_sbic")

#' @export
#' @rdname ols_step_backward_sbic
#'
ols_step_backward_sbic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_backward(model, "sbic", include, exclude, progress, details)
  class(out) <- "ols_step_backward_sbic"
  return(out)
}

#' @export
#'
print.ols_step_backward_sbic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "backward")
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_backward_sbic
#' @export
#'
plot.ols_step_backward_sbic <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {
  p <- ols_stepaic_plot(x, details, digits)
  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}

#' Stepwise R-Squared backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on r-squared, in a stepwise manner until there is
#' no variable left to remove any more.
#'
#' @inheritParams ols_step_backward_aic
#'
#' @inherit ols_step_backward_aic return references
#'
#' @examples
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward_r2(model)
#'
#' # final model
#' k$model
#'
#' # include or exclude variable
#' # force variables to be included in the selection process
#' ols_step_backward_r2(model, include = c("alc_mod", "gender"))
#'
#' # use index of variable instead of name
#' ols_step_backward_r2(model, include = c(7, 6))
#'
#' # force variable to be excluded from selection process
#' ols_step_backward_r2(model, exclude = c("alc_heavy", "bcs"))
#'
#' # use index of variable instead of name
#' ols_step_backward_r2(model, exclude = c(8, 1))
#'
#' @family backward selection procedures
#'
#' @export
#'
ols_step_backward_r2 <- function(model, ...) UseMethod("ols_step_backward_r2")

#' @export
#' @rdname ols_step_backward_r2
#'
ols_step_backward_r2.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_backward(model, "rsq", include, exclude, progress, details)
  class(out) <- "ols_step_backward_r2"
  return(out)
}

#' @export
#'
print.ols_step_backward_r2 <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "backward")
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_backward_r2
#' @export
#'
plot.ols_step_backward_r2 <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {
  p <- ols_stepaic_plot(x, details, digits)
  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}

#' Stepwise Adjusted R-Squared backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on adjusted r-squared, in a stepwise
#' manner until there is no variable left to remove any more.
#'
#' @inheritParams ols_step_backward_aic
#'
#' @inherit ols_step_backward_aic return references
#'
#' @examples
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward_adj_r2(model)
#'
#' # final model
#' k$model
#'
#' # include or exclude variable
#' # force variables to be included in the selection process
#' ols_step_backward_adj_r2(model, include = c("alc_mod", "gender"))
#'
#' # use index of variable instead of name
#' ols_step_backward_adj_r2(model, include = c(7, 6))
#'
#' # force variable to be excluded from selection process
#' ols_step_backward_adj_r2(model, exclude = c("alc_heavy", "bcs"))
#'
#' # use index of variable instead of name
#' ols_step_backward_adj_r2(model, exclude = c(8, 1))
#'
#' @family backward selection procedures
#'
#' @export
#'
ols_step_backward_adj_r2 <- function(model, ...) UseMethod("ols_step_backward_adj_r2")

#' @export
#' @rdname ols_step_backward_adj_r2
#'
ols_step_backward_adj_r2.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_backward(model, "adjrsq", include, exclude, progress, details)
  class(out) <- "ols_step_backward_adj_r2"
  return(out)
}

#' @export
#'
print.ols_step_backward_adj_r2 <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "backward")
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_backward_adj_r2
#' @export
#'
plot.ols_step_backward_adj_r2 <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {
  p <- ols_stepaic_plot(x, details, digits)
  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}
