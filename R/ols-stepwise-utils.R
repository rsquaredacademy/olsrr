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

ols_candidate_terms <- function(cterms = NULL, direction = c("forward", "backward", "both")) {
  method <- match.arg(direction)

  if (method == "forward") {
    title <- "Forward Selection Method"
    width <- 24
  } else if (method == "backward") {
    title <- "Backward Elimination Method"
    width <- 27
  } else {
    title <- "Stepwise Selection Method"
    width <- 25
  }

  cat(format(title, justify = "left", width = width), "\n")
  cat(rep("-", width), sep = "", "\n\n")
  cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
  for (i in seq_len(length(cterms))) {
    cat(paste(i, ".", cterms[i]), "\n")
  }
  cat("\n")

}

ols_base_model_stats <- function(response, include, direction = c("forward", "backward", "both"), aic) {
  
  method <- match.arg(direction)
  cat("\n")
  cat("Step  => 0", "\n")

  if (method == "backward") {
    cat("Model =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
  } else {
    if (is.null(include)) {
      cat("Model =>", paste(response, "~", 1, "\n"))
    } else {
      cat("Model =>", paste(response, "~", paste(include, collapse = " + "), "\n"))  
    }
  }
  
  cat("AIC   =>", aic, "\n\n")
  cat("Initiating stepwise selection...", "\n\n")

}

ols_progress_init <- function(direction = c("forward", "backward", "both")) {

  method <- match.arg(direction)

  if (method == "forward") {
    display <- "Entered:"
  } else if (method == "backward") {
    display <- "Removed:"
  } else {
    display <- "Entered/Removed:"
  }

  cat("\n")
  cat(paste0("Variables ", display), "\n\n")

}

ols_progress_display <- function(preds, direction = c("others", "both"), type = c("added", "removed")) {

  method <- match.arg(direction)
  base   <- paste("=>", tail(preds, n = 1))

  if (method == "others") {
    cat(base, "\n")
  } else {
    cat(paste(base, type), "\n")
  }

}

ols_stepwise_details <- function(step, rpred, preds, response, aic, type = c("added", "removed")) {

  cat("Step    =>", step, "\n")

  if (type == "added") {
    cat("Added   =>", tail(rpred, n = 1), "\n")
  } else {
    cat("Removed =>", tail(rpred, n = 1), "\n")
  }
  
  cat("Model   =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
  cat("AIC     =>", aic, "\n\n")

}

ols_stepwise_metrics <- function(df, metric = c("aic", "r2", "adj_r2"), predictors, aics, rss, ess, rsq, arsq) {

  type <- match.arg(metric)

  w1 <- max(nchar("Predictor"), nchar(predictors))
  w2 <- 2
  w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
  w4 <- max(nchar("Sum Sq"), nchar(format(round(rss, 3), nsmall = 3)))
  w5 <- max(nchar("ESS"), nchar(format(round(ess, 3), nsmall = 3)))
  w6 <- max(nchar("R-Sq"), nchar(format(round(rsq, 3), nsmall = 3)))
  w7 <- max(nchar("Adj. R-Sq"), nchar(format(round(arsq, 3), nsmall = 3)))
  w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
  ln <- length(aics)

  if (type == "aic") {
    cat(format("Information Criteria Table", justify = "centre", width = w), "\n")
  } else if (type == "r2") {
    cat(format("R-Squared Table", justify = "centre", width = w), "\n")
  } else {
    cat(format("Adjusted R-Squared Table", justify = "centre", width = w), "\n")
  }

  cat(rep("-", w), sep = "", "\n")
  
  cat(
    fl("Predictor", w1), fs(), 
    fc("DF", w2), fs(), 
    fc("AIC", w3), fs(),
    fc("Sum Sq", w4), fs(), 
    fc("ESS", w5), fs(), 
    fc("R-Sq", w6), fs(),
    fc("Adj. R-Sq", w7), "\n")

  cat(rep("-", w), sep = "", "\n")

  for (i in seq_len(ln)) {
    cat(
      fl(df[i, 1], w1), fs(), 
      fg(1, w2), fs(), 
      fg(format(round(df[i, 2], 3), nsmall = 3), w3), fs(),
      fg(format(round(df[i, 4], 3), nsmall = 3), w4), fs(), 
      fg(format(round(df[i, 3], 3), nsmall = 3), w5), fs(),
      fg(format(round(df[i, 5], 3), nsmall = 3), w6), fs(), 
      fg(format(round(df[i, 6], 3), nsmall = 3), w7), "\n")
  }

  cat(rep("-", w), sep = "", "\n\n")

}