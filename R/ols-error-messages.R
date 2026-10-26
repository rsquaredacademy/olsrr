check_model <- function(model) {
  if (!inherits(model, "lm")) {
    model_name <- deparse(substitute(model))
    stop(paste0("`", model_name, "` must be an object of class `lm`."), call. = FALSE)
  }
}

check_weights <- function(model, fn = as.character(sys.call(-1)[[1]])) {
  if (!is.null(model$weights) && any(model$weights != 1)) {
    fn_name <- if (is.character(fn)) fn else deparse(fn)
    fn_name <- sub(".*::", "", fn_name)
    fn_name <- sub("\\(.*", "", fn_name)
    stop(paste0("weighted models are not supported by ", fn_name, "; fit without weights or use step()"), call. = FALSE)
  }
}

check_order <- function(n, max_order) {
  if (max_order > n) {
    stop("Maximum subset order should be less than or equal to the number of predictors in the specified model.", call. = FALSE)
  }
}

check_logic <- function(logic) {

  lval        <- is.logical(logic)
  logic_name  <- deparse(substitute(logic))

  if (lval != TRUE) {
    stop(paste0("`", logic_name, '` must be a flag (TRUE or FALSE).'), call. = FALSE)
  }

}

check_options <- function(option) {

  default_options <- c("none", "bonferroni", "sidak", "holm")
  valid           <- any(default_options == option)
  option_class    <- class(option)
  option_name     <- deparse(substitute(option))

  if (valid != TRUE) {
    err_message <- paste0("`", option_name, "` can take the following values only:\n* `none`\n* `bonferroni`\n* `sidak`\n* `holm`")
    stop(err_message, call. = FALSE)
  }
}


check_values <- function(value, lower, upper) {

  valid       <- (value >= lower && value <= upper)
  value_class <- class(value)
  value_name  <- deparse(substitute(value))

  if (valid != TRUE) {
    err_message <- paste0("`", value_name, "` must be between ", lower, " and ", upper, ".")
    stop(err_message, call. = FALSE)
  }

}

check_npredictors <- function(model, min) {

  n <- length(coefficients(model))

  if (n < min) {
    err_message <- paste0("For stepwise selection, model should include at least `", min, "` predictors.")
    stop(err_message, call. = FALSE)
  }

}


check_lfit <- function(model) {

  n     <- length(coefficients(model))
  preds <- n - 1

  if (n > 2) {
    err_message <- paste0("Lack of fit F test is available only for models with a single predictor.")
    stop(err_message, call. = FALSE)
  }

}

check_modelvars <- function(model, vars) {

  fmla  <- deparse(formula(model))
  valid <- unique(c(attr(model$terms, "term.labels"),
                    setdiff(colnames(model.matrix(model)), "(Intercept)")))
  k     <- vars %in% valid
  wvars <- vars[!k]
  nvars <- length(wvars)

  if (nvars > 0) {
    err_message <- paste0("Model `(", fmla, ")` should include the predictor(s) `", paste0(wvars, collapse = ", "), "`.")
    stop(err_message, call. = FALSE)
  }

}

# Map predictor terms to the columns of the model matrix. Factor terms expand
# to one column per dummy; numeric terms map to themselves.
vars_columns <- function(model, vars) {

  terms  <- attr(model$terms, "term.labels")
  assign <- attr(model.matrix(model), "assign")
  mm     <- colnames(model.matrix(model))
  out    <- character(0)

  for (v in vars) {
    i <- match(v, terms)
    if (!is.na(i)) {
      out <- c(out, mm[assign == i])
    } else if (v %in% mm) {
      out <- c(out, v)
    }
  }

  out

}
