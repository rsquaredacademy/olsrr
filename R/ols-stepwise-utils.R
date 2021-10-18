check_inputs <- function(model, include, exclude, progress, details) {

  check_model(model)
  check_npredictors(model, 2)
  
  indterms <- coeff_names(model)
  check_terms(include, indterms)
  check_terms(exclude, indterms, include = FALSE)

  check_logic(progress)
  check_logic(details)
  
}

check_terms <- function(clude, indterms, include = TRUE) {

  if (!include) {
    process <- "excluded"
  } else {
    process <- "included"
  }

  if (is.character(clude)) {
    npm <- clude %in% indterms
    if (!all(npm)) {
      stop(
        paste0(
          paste(clude[!npm], collapse = ", "),
          " not part of the model and hence cannot be forcibly ", process, ". Please verify the variable names."),
        call. = FALSE)
    }
  }

  lenterms <- length(indterms)

  if (is.numeric(clude)) {
    if (any(clude > lenterms)) {
      stop(paste0("Index of variable to be ", process, " should be between 1 and ", lenterms, "."), call. = FALSE)
    } 
  }
  
}

ols_base_model <- function(include, response, data) {
  if (is.null(include)) {
    lm(paste(response, "~", 1), data = data)
  } else {
    lm(paste(response, "~", paste(include, collapse = " + ")), data = data)
  }
}

