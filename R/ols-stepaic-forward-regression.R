#' Stepwise AIC forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on akaike information criterion, in a stepwise
#' manner until there is no variable left to enter any more.
#'
#' @param model An object of class \code{lm}.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_forward_*}.
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
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_aic(model)
#'
#' # stepwise forward regression plot
#' k <- ols_step_forward_aic(model)
#' plot(k)
#'
#' # extract final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' ols_step_forward_aic(model, include = c("age"))
#'
#' # use index of variable instead of name
#' ols_step_forward_aic(model, include = c(5))
#'
#' # force variable to be excluded from selection process
#' ols_step_forward_aic(model, exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_aic(model, exclude = c(4))
#'
#' # include & exclude variables in the selection process
#' ols_step_forward_aic(model, include = c("age"), exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_aic(model, include = c(5), exclude = c(4))
#'
#' @family forward selection procedures
#'
#' @export
#'
ols_step_forward_aic <- function(model, ...) UseMethod("ols_step_forward_aic")

#' @export
#' @rdname ols_step_forward_aic
#'
ols_step_forward_aic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_forward(model, "aic", include, exclude, progress, details)
  class(out) <- "ols_step_forward_aic"
  return(out)
}

#' @export
#'
print.ols_step_forward_aic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "forward")
  } else {
    print("No variables have been added to the model.")
  }
}

#' @rdname ols_step_forward_aic
#' @export
#'
plot.ols_step_forward_aic <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {

  p <- ols_stepaic_plot(x, details, digits)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}

#' Stepwise SBC forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on schwarz bayesian criterion, in a stepwise
#' manner until there is no variable left to enter any more.
#'
#' @inheritParams ols_step_forward_aic
#'
#' @inherit ols_step_forward_aic return references
#'
#' @examples
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_sbc(model)
#'
#' # stepwise forward regression plot
#' k <- ols_step_forward_sbc(model)
#' plot(k)
#'
#' # extract final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' ols_step_forward_sbc(model, include = c("age"))
#'
#' # use index of variable instead of name
#' ols_step_forward_sbc(model, include = c(5))
#'
#' # force variable to be excluded from selection process
#' ols_step_forward_sbc(model, exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_sbc(model, exclude = c(4))
#'
#' # include & exclude variables in the selection process
#' ols_step_forward_sbc(model, include = c("age"), exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_sbc(model, include = c(5), exclude = c(4))
#'
#' @family forward selection procedures
#'
#' @export
#'
ols_step_forward_sbc <- function(model, ...) UseMethod("ols_step_forward_sbc")

#' @rdname ols_step_forward_sbc
#' @export
#'
ols_step_forward_sbc.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_forward(model, "sbc", include, exclude, progress, details)
  class(out) <- "ols_step_forward_sbc"
  return(out)
}

#' @export
#'
print.ols_step_forward_sbc <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "forward")
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_forward_sbc
#' @export
#'
plot.ols_step_forward_sbc <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {
  p <- ols_stepaic_plot(x, details, digits)
  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}

#' Stepwise SBIC forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on sawa bayesian criterion, in a stepwise
#' manner until there is no variable left to enter any more.
#'
#' @inheritParams ols_step_forward_aic
#' @inherit ols_step_forward_aic return references
#'
#' @examples
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_sbic(model)
#'
#' # stepwise forward regression plot
#' k <- ols_step_forward_sbic(model)
#' plot(k)
#'
#' # extract final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' ols_step_forward_sbic(model, include = c("age"))
#'
#' # use index of variable instead of name
#' ols_step_forward_sbic(model, include = c(5))
#'
#' # force variable to be excluded from selection process
#' ols_step_forward_sbic(model, exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_sbic(model, exclude = c(4))
#'
#' # include & exclude variables in the selection process
#' ols_step_forward_sbic(model, include = c("age"), exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_sbic(model, include = c(5), exclude = c(4))
#'
#' @family forward selection procedures
#'
#' @export
#'
ols_step_forward_sbic <- function(model, ...) UseMethod("ols_step_forward_sbic")

#' @export
#' @rdname ols_step_forward_sbic
#'
ols_step_forward_sbic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_forward(model, "sbic", include, exclude, progress, details)
  class(out) <- "ols_step_forward_sbic"
  return(out)
}

#' @export
#'
print.ols_step_forward_sbic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "forward")
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_forward_sbic
#' @export
#'
plot.ols_step_forward_sbic <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {
  p <- ols_stepaic_plot(x, details, digits)
  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}

#' Stepwise R-Squared forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on r-squared, in a stepwise manner until there
#' is no variable left to enter any more.
#'
#' @inheritParams ols_step_forward_aic
#' @inherit ols_step_forward_aic return references
#'
#' @examples
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_r2(model)
#'
#' # stepwise forward regression plot
#' k <- ols_step_forward_r2(model)
#' plot(k)
#'
#' # extract final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' ols_step_forward_r2(model, include = c("age"))
#'
#' # use index of variable instead of name
#' ols_step_forward_r2(model, include = c(5))
#'
#' # force variable to be excluded from selection process
#' ols_step_forward_r2(model, exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_r2(model, exclude = c(4))
#'
#' # include & exclude variables in the selection process
#' ols_step_forward_r2(model, include = c("age"), exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_r2(model, include = c(5), exclude = c(4))
#'
#' @family forward selection procedures
#'
#' @export
#'
ols_step_forward_r2 <- function(model, ...) UseMethod("ols_step_forward_r2")

#' @rdname ols_step_forward_r2
#' @export
#'
ols_step_forward_r2.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_forward(model, "rsq", include, exclude, progress, details)
  class(out) <- "ols_step_forward_r2"
  return(out)
}

#' @export
#'
print.ols_step_forward_r2 <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "forward")
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_forward_r2
#' @export
#'
plot.ols_step_forward_r2 <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {
  p <- ols_stepaic_plot(x, details, digits)
  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}

#' Stepwise Adjusted R-Squared forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on adjusted r-squared, in a stepwise
#' manner until there is no variable left to enter any more.
#'
#' @inheritParams ols_step_forward_aic
#' @inherit ols_step_forward_aic return references
#'
#' @examples
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_adj_r2(model)
#'
#' # stepwise forward regression plot
#' k <- ols_step_forward_adj_r2(model)
#' plot(k)
#'
#' # extract final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' ols_step_forward_adj_r2(model, include = c("age"))
#'
#' # use index of variable instead of name
#' ols_step_forward_adj_r2(model, include = c(5))
#'
#' # force variable to be excluded from selection process
#' ols_step_forward_adj_r2(model, exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_adj_r2(model, exclude = c(4))
#'
#' # include & exclude variables in the selection process
#' ols_step_forward_adj_r2(model, include = c("age"), exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_adj_r2(model, include = c(5), exclude = c(4))
#'
#' @family forward selection procedures
#'
#' @export
#'
ols_step_forward_adj_r2 <- function(model, ...) UseMethod("ols_step_forward_adj_r2")

#' @rdname ols_step_forward_adj_r2
#' @export
#'
ols_step_forward_adj_r2.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {
  out <- ols_step_forward(model, "adjrsq", include, exclude, progress, details)
  class(out) <- "ols_step_forward_adj_r2"
  return(out)
}

#' @export
#'
print.ols_step_forward_adj_r2 <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "forward")
  } else {
    print("No variables have been removed from the model.")
  }
}

#' @rdname ols_step_forward_adj_r2
#' @export
#'
plot.ols_step_forward_adj_r2 <- function(x, print_plot = TRUE, details = TRUE, digits = 3, ...) {
  p <- ols_stepaic_plot(x, details, digits)
  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}
