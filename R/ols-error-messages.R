check_model <- function(model) {

  if (!all(class(model) == "lm")) {

    cat("Hmmm.. Looks like you have specified the wrong model. Follow the below steps to debug this error.

* Check if you have used the lm() function to build the model.
* If you have never used it before, you can learn more by typing ?lm or help(lm) in the Console\n.

Below is an example using mtcars data:\n lm(formula = mpg ~ disp + hp + wt, data = mtcars)\n",

"Happy modeling :)\n")

    stop("", call. = FALSE)

  }
}

check_logic <- function(logic) {

  lval <- is.logical(logic)
  logic_class <- class(logic)
  logic_name <- deparse(substitute(logic))

  if (lval != TRUE) {

    cat("\n *", logic_name, "can take only 2 values, either TRUE or FALSE", "\n * You have used", logic, "\n * Replace", logic, "with either TRUE or FALSE", "\n\n")

    stop("", call. = FALSE)

  }

}

check_options <- function(option) {

  default_options <- c("none", "bonferroni", "sidak", "holm")
  valid <- any(default_options == option)
  option_class <- class(option)
  option_name <- deparse(substitute(option))

  if (valid != TRUE) {

    cat("\n", option_name, "can take the following values only: \n", "* 'none'\n", "* 'bonferroni'\n", "* 'sidak'\n", "* 'holm'\n\n", "You have used", paste0(option, ","), "please use any one of the above listed value.\n\n")

    stop("", call. = FALSE)

  }
}


check_values <- function(value, lower, upper) {

  valid <- (value >= lower && value <= upper)
  value_class <- class(value)
  value_name <- deparse(substitute(value))

  if (valid != TRUE) {

    cat("\n")
    cat(value_name, "can take on values between", lower, "and", upper, "only.", "You have used", paste0(value, ","), "please specify a value between", lower, "and", upper, "only.", "\n\n")

    stop("", call. = FALSE)

  }

}

check_npredictors <- function(model, min) {

  n <-
    model %>%
    coefficients() %>%
    length()

  if (n < min) {

    cat("\n")
    cat("Hello there.. the model contains only one predictor. For stepwise selection, please specify a model with at least 2 predictors. \n\n")

    stop("", call. = FALSE)

  }

}


check_lfit <- function(model) {

  n <-
    model %>%
    coefficients() %>%
    length()

  preds <- n - 1

  if (n > 2) {

    cat("\n")
    cat("Hello there.. the lack of fit F test is available only for simple linear regression model i.e. model with a single predictor. The specified model contains", preds, "predictors. Please specify a model with a single predictor.", "\n\n")

    stop("", call. = FALSE)

  }

}

check_modelvars <- function(model, vars) {

  fmla <- deparse(formula(model))
  k <- vars %in% names(model$coefficients)
  wvars <- vars[!k]
  nvars <- length(wvars)

  if (nvars > 0) {

    cat("\n")
    cat("The specified model:", "\n\n", fmla, "\n\n", "does not contain the predictor(s)", paste(wvars, collapse = ", "), ".\n\n", "Please specify the correct predictor(s).\n\n")

    stop("", call. = FALSE)

  }

}
