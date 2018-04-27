#' @importFrom checkmate check_tibble check_data_frame check_logical check_choice check_number
check_model <- function(model) {

  if (!all(class(model) == "lm")) {

    cat("Hmmm.. Looks like you have specified the wrong model. Follow the below steps to debug this error.

* Check if you have used the", crayon::bold$blue("lm()"), "function to build the model.
* If you have never used it before, you can learn more by typing", crayon::bold$red("?lm"), "or", crayon::bold$red("help(lm)"), "in the Console\n.

Below is an example using mtcars data:\n",

crayon::bold$blue("lm(formula = mpg ~ disp + hp + wt, data = mtcars)"), "\n",

"Happy modeling :)\n")

    stop("", call. = FALSE)

  }
}

check_data <- function(data) {

  tib <- check_tibble(data)
  df  <- check_data_frame(data)
  data_class <- class(data)
  data_name <- deparse(substitute(data))

  if (tib != TRUE & df != TRUE) {

    cat(crayon::bold$red(data_name), "must either be a", crayon::bold$blue("data.frame"), "or a", crayon::bold$blue("tibble"), "but you have used a", crayon::bold$blue(data_class), "vector. Use the", crayon::bold$blue("class()"), "function to check the type of", crayon::bold$red(data_name), " as shown below:\n\n", crayon::bold$blue(paste0("class(", data_name, ")")), "\n\n If", crayon::bold$red(data_name), "is a column in a data set, use the name of the data set as the input.", "\n\n Type", crayon::bold$red("?data.frame"), "or", crayon::bold$red("?tibble"), "to learn how to create and use them.\n")

    stop("", call. = FALSE)
  }

}

check_logic <- function(logic) {

  lval <- check_logical(logic)
  logic_class <- class(logic)
  logic_name <- deparse(substitute(logic))

  if (lval != TRUE) {

    cat("\n *", crayon::bold$red(logic_name), "can take only 2 values, either", crayon::bold$blue("TRUE"), "or", crayon::bold$blue("FALSE"), "\n * You have used", crayon::bold$red(logic), "\n * Replace",  crayon::bold$blue(logic), "with either", crayon::bold$blue("TRUE"), "or", crayon::bold$blue("FALSE"), "\n\n")

    stop("", call. = FALSE)

  }

}

check_options <- function(option) {

  valid <- check_choice(option, c("none", "bonferroni", "sidak", "holm"))
  option_class <- class(option)
  option_name <- deparse(substitute(option))

  if (valid != TRUE) {

    cat("\n", crayon::bold$red(option_name), "can take the following values only: \n", "* 'none'\n", "* 'bonferroni'\n", "* 'sidak'\n", "* 'holm'\n\n", "You have used", paste0(crayon::bold$blue(option), ","), "please use any one of the above listed value.\n\n")

    stop("", call. = FALSE)

  }
}


check_values <- function(value, lower, upper) {

  valid <- check_number(value, lower = lower, upper = upper)
  value_class <- class(value)
  value_name <- deparse(substitute(value))

  if (valid != TRUE) {

    cat("\n")
    cat(crayon::bold$red(value_name), "can take on values between", crayon::bold$red(lower) , "and", crayon::bold$red(upper), "only.", "You have used", paste0(crayon::bold$blue(value), ","), "please specify a value between", crayon::bold$red(lower) , "and", crayon::bold$red(upper), "only.", "\n\n")

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
    cat("Hello there.. the lack of fit F test is available only for simple linear regression model i.e. model with a single predictor. The specified model contains", crayon::bold$red(preds), "predictors. Please specify a model with a single predictor.", "\n\n")

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
    cat("The specified model:", "\n\n", crayon::bold$blue(fmla), "\n\n", "does not contain the predictor(s)", crayon::bold$red(paste(wvars, collapse = ", ")), ".\n\n", "Please specify the correct predictor(s).\n\n")

    stop("", call. = FALSE)

  }

}
