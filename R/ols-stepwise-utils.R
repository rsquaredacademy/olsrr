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
    cat(paste0(i, ". ", cterms[i]), "\n")
    if (interactive()) {
      Sys.sleep(0.3)
    }
  }
  cat("\n")

}

ols_base_model_stats <- function(response, include, criteria, aic) {

  mat  <- switch(criteria,
    aic    = "AIC    ",
    sbc    = "SBC    ",
    sbic   = "SBIC   ",
    rsq    = "R2     ",
    adjrsq = "Adj. R2")

  cat("\n")
  cat("Step     => 0", "\n")
  if (interactive()) {
    Sys.sleep(0.3)
  }

  if (is.null(include)) {
    cat("Model    =>", paste(response, "~", 1, "\n"))
  } else {
    cat("Model    =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
  }

  if (interactive()) {
    Sys.sleep(0.3)
  }

  cat(paste0(mat, "  =>"), aic, "\n\n")

  if (interactive()) {
    Sys.sleep(0.3)
  }
  cat("Initiating stepwise selection...", "\n\n")

}

ols_progress_init <- function(direction = c("forward", "backward", "both")) {

  method <- match.arg(direction)

  if (method == "forward") {
    display <- "Entered:"
  } else if (method == "backward") {
    display <- "Removed:"
  } else {
    display <- "Added/Removed:"
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

  if (interactive()) {
    Sys.sleep(0.3)
  }

}

ols_stepwise_details <- function(step, rpred, preds, response, aic, type = c("added", "removed"), metric = "AIC") {

  cat("Step     =>", step, "\n")
  
  if (interactive()) {
    Sys.sleep(0.3)
  }

  if (type == "added") {
    cat("Added    =>", tail(rpred, n = 1), "\n")
  } else {
    cat("Removed  =>", tail(rpred, n = 1), "\n")
  }

  if (interactive()) {
    Sys.sleep(0.3)
  }

  cat("Model    =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))

  if (interactive()) {
    Sys.sleep(0.3)
  }

  mat  <- switch(metric,
    aic    = "AIC    ",
    sbc    = "SBC    ",
    sbic   = "SBIC   ",
    rsq    = "R2     ",
    adjrsq = "Adj. R2")

  cat(paste0(mat, "  =>"), round(aic, 5), "\n\n")

}

ols_stepwise_metrics <- function(df, metric = c("aic", "sbc", "sbic", "rsq", "adjrsq"), predictors, aics, bics, sbics, rsq, arsq, method = c("add", "remove")) {

  type <- match.arg(metric)

  w1 <- max(nchar("Predictor"), nchar(predictors))
  w2 <- 2
  w3 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
  w4 <- max(nchar("SBC"), nchar(format(round(bics, 3), nsmall = 3)))
  w5 <- max(nchar("SBIC"), nchar(format(round(sbics, 3), nsmall = 3)))
  w6 <- max(nchar("R2"), nchar(format(round(rsq, 5), nsmall = 5)))
  w7 <- max(nchar("Adj. R2"), nchar(format(round(arsq, 5), nsmall = 5)))
  w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)
  ln <- length(aics)

  if (method == "add") {
    cat(format("Table: Adding New Variables", justify = "centre", width = w), "\n")
  } else {
    cat(format("Table: Removing Existing Variables", justify = "centre", width = w), "\n")
  }

  cat(rep("-", w), sep = "", "\n")

  cat(
    fl("Predictor", w1), fs(),
    fc("DF", w2), fs(),
    fc("AIC", w3), fs(),
    fc("SBC", w4), fs(),
    fc("SBIC", w5), fs(),
    fc("R2", w6), fs(),
    fc("Adj. R2", w7), "\n")

  cat(rep("-", w), sep = "", "\n")

  for (i in seq_len(ln)) {
    cat(
      fl(df[i, 1], w1), fs(),
      fg(1, w2), fs(),
      fg(format(round(df[i, 2], 3), nsmall = 3), w3), fs(),
      fg(format(round(df[i, 3], 3), nsmall = 3), w4), fs(),
      fg(format(round(df[i, 4], 3), nsmall = 3), w5), fs(),
      fg(format(round(df[i, 5], 5), nsmall = 5), w6), fs(),
      fg(format(round(df[i, 6], 5), nsmall = 5), w7), "\n")
  }

  cat(rep("-", w), sep = "", "\n\n")

}

ols_stepwise_table <- function(df, predictors, p_val, rsq, arsq, aics) {

  w1 <- max(nchar("Predictor"), nchar(predictors))
  w2 <- max(nchar("Pr(>|t|)"), nchar(format(round(p_val, 5), nsmall = 5)))
  w3 <- max(nchar("R-Squared"), nchar(format(round(rsq, 3), nsmall = 3)))
  w4 <- max(nchar("Adj. R-Squared"), nchar(format(round(arsq, 3), nsmall = 3)))
  w5 <- max(nchar("AIC"), nchar(format(round(aics, 3), nsmall = 3)))
  w  <- sum(w1, w2, w3, w4, w5, 16)
  ln <- length(aics)

  cat(format("Selection Metrics Table", justify = "centre", width = w), "\n")

  cat(rep("-", w), sep = "", "\n")

  cat(
    fl("Predictor", w1), fs(),
    fc("Pr(>|t|)", w2), fs(),
    fc("R-Squared", w3), fs(),
    fc("Adj. R-Squared", w4), fs(),
    fc("AIC", w5), "\n")

  cat(rep("-", w), sep = "", "\n")

  for (i in seq_len(ln)) {
    cat(
      fl(df[i, 1], w1), fs(),
      fg(format(round(df[i, 2], 5), nsmall = 5), w2), fs(),
      fg(format(round(df[i, 3], 3), nsmall = 3), w3), fs(),
      fg(format(round(df[i, 4], 3), nsmall = 3), w4), fs(),
      fg(format(round(df[i, 5], 3), nsmall = 3), w5), "\n")
  }

  cat(rep("-", w), sep = "", "\n\n")

}

ols_stepwise_table_p <- function(df, predictors, p_val) {

  w1 <- max(nchar("Predictor"), nchar(predictors))
  w2 <- max(nchar("Pr(>|t|)"), nchar(format(round(p_val, 5), nsmall = 5)))
  w  <- sum(w1, w2, 4)
  ln <- length(predictors)

  cat(format("Significance Table", justify = "centre", width = w), "\n")

  cat(rep("-", w), sep = "", "\n")

  cat(
    fl("Predictor", w1), fs(),
    fc("Pr(>|t|)", w2), "\n")

  cat(rep("-", w), sep = "", "\n")

  for (i in seq_len(ln)) {
    cat(
      fl(df[i, 1], w1), fs(),
      fg(format(round(df[i, 2], 5), nsmall = 5), w2), "\n")
  }

  cat(rep("-", w), sep = "", "\n\n")

}

ols_stepwise_break <- function(direction = c("forward", "backward", "both")) {

  method <- match.arg(direction)

  if (method == "forward") {
    op <- "added."
  } else if (method == "backward") {
    op <- "removed."
  } else {
    op <- "added or removed."
  }

  cat("\n")
  cat(paste("No more variables to be", op))
  cat("\n")

}

ols_stepwise_vars <- function(preds, direction = c("forward", "backward", "both")) {

  method <- match.arg(direction)

  if (method == "forward" || method == "both") {
    op <- "Selected:"
  } else {
    op <- "Removed:"
  }

  cat("\n")
  if (length(preds) > 0) {
    cat(paste("Variables", op), "\n\n")
    for (i in seq_len(length(preds))) {
      cat(paste("=>", preds[i]), "\n")
      if (interactive()) {
        Sys.sleep(0.3)
      }
    }
  }

}

ols_stepaic_plot <- function(x, details = TRUE, digits = 3) {

  pred <- ols_step_plot_text(x, x$others$direction, details, x$others$criteria, digits)
  data <- ols_stepwise_plot_data(x, pred, x$others$criteria)
  info <- ols_metric_info(x, x$others$direction, x$others$criteria)
  ols_stepaic_plot_build(data$d, data$d2, data$xmin, data$xmax, data$ymin, data$ymax, info, x$others$direction, x$others$criteria)

}

ols_stepwise_plot_data <- function(x, pred, metric = "r2") {

  step <- x$metrics$step
  aic  <- x$metrics[[metric]]
  xmin <- 0
  xmax <- max(step) + 1
  ymin <- min(aic) - (min(aic) * 0.05)
  ymax <- max(aic) + (max(aic) * 0.05)
  d2   <- data.frame(x = step, y = aic, tx = pred)
  d    <- data.frame(a = step, b = aic)

  return(list(d = d, d2 = d2, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
}


ols_step_plot_text <- function(x, direction = c("forward", "backward", "both"), details = TRUE, metric = "r2", digits = 3) {

  method <- match.arg(direction)

  if (method == "forward" || method == "backward") {
    if (details) {
      pred <- paste0("[", x$metrics$variable, ", ", format(round(x$metrics[[metric]], digits), nsmall = digits), "]")
    } else {
      pred <- x$metrics$variable
    }
  } else {
    if (details) {
      pred <- ifelse(x$metrics$method == "addition",
                             paste0("[+", x$metrics$variable, ", ", format(round(x$metrics[[metric]], digits), nsmall = digits), "]"),
                             paste0("[-", x$metrics$variable, ", ", format(round(x$metrics[[metric]], digits), nsmall = digits), "]"))
    } else {
      pred <- ifelse(x$metrics$method == "addition",
                               paste0("+", x$metrics$variable),
                               paste0("-", x$metrics$variable))
    }
  }

  return(pred)
}


ols_metric_info <- function(x, direction = c("forward", "backward", "both"), metric = "r2") {

  method <- match.arg(direction)

  if (metric == "r2") {
    met <- "rsq"
  } else if (metric == "adj_r2") {
    met <- "adjr"
  } else {
    met <- metric
  }

  if (method == "forward" || method == "both") {
    the_model <- x$others$base_model
    the_info  <- "Base Model  : "
  } else {
    the_model <- x$others$full_model
    the_info  <- " Full Model  : "
  }

  np <- coeff_names(the_model)
  if (is.null(np)) {
    mi <- null_model_metrics(the_model, x$others$full_model)
  } else {
    mi <- ols_regress(the_model)
  }

  base_model_met  <- round(mi[[met]], 3)
  final_model_met <- round(ols_regress(x$model)[[met]], 3)
  metric_info <- paste0(the_info, format(base_model_met, nsmall = 3), "\n",
                        "Final Model : ", format(final_model_met, nsmall = 3))

  return(metric_info)

}

ols_stepaic_plot_build <- function(d, d2, xmin, xmax, ymin, ymax, metric_info, direction = c("forward", "backward", "both"), criteria = "aic") {

  method <- match.arg(direction)

  mat  <- switch(criteria,
    aic    = "AIC",
    sbc    = "SBC",
    sbic   = "SBIC",
    r2     = "R2",
    adj_r2 = "Adj. R2")

  if (method == "forward") {
    title <- paste0("Stepwise ", mat , " Forward Selection")
    nudge <- 0.1
  } else if (method == "backward") {
    title <- paste0("Stepwise ", mat , " Backward Elimination")
    nudge <- 0.1
  } else {
    title <- paste0("Stepwise ", mat, " Both Direction Selection")
    nudge <- 0.5
  }

  y_lab  <- mat
  v_just <- "bottom"
  h_just <- 1.2
  ann_x  <- Inf

  ols_step_ggplot(d, d2, xmin, xmax, ymin, ymax, y_lab, title, v_just, h_just, nudge, ann_x, metric_info)

}


ols_plot_stepwise <- function(x, metric = "r2", y_lab = "R-Square", details = TRUE,
  direction = c("forward", "backward", "both")) {

  type <- match.arg(direction)
  pred <- ols_step_plot_text(x, type, details, metric)
  data <- ols_stepwise_plot_data(x, pred, metric)
  info <- ols_metric_info(x, type, metric)

  v_just <- ifelse(metric %in% c("aic", "rmse"), "bottom", "top")
  h_just <- ifelse(metric %in% c("aic", "rmse"), 1.2, 0)
  ann_x  <- ifelse(metric %in% c("aic", "rmse"), Inf, 0)

  if (metric == "r2") {
    title <- "R-Square"
  } else if (metric == "adj_r2") {
    title <- "Adjusted R-Square"
  } else if (metric == "aic") {
    title <- "Akaike Information Criteria"
  } else {
    title <- "Root Mean Squared Error"
  }

  ols_step_ggplot(data$d, data$d2, data$xmin, data$xmax, data$ymin, data$ymax,
    y_lab, title, v_just, h_just, nudge = 0.05, ann_x, info)

}

#' @importFrom ggplot2 aes_string
ols_step_ggplot <- function(d, d2, xmin, xmax, ymin, ymax, y_lab, title, v_just, h_just, nudge = 0.05, ann_x, metric_info) {

  ggplot(d, aes_string(x = "a", y = "b")) +
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) +
    geom_text(data = d2, aes_string(x = "x", y = "y", label = "tx"), size = 3,
              hjust = "left", vjust = v_just, nudge_x = nudge) +
    annotate("text", x = ann_x, y = Inf, hjust = h_just, vjust = 2,
             family = "serif", fontface = "bold", size = 3,
             label = metric_info) +
    xlab("Step") +
    ylab(y_lab) +
    ggtitle(title) +
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax))
}

ols_print_final_model <- function(data) {
  cat("\n\n")
  cat("Final Model Output", "\n")
  cat(rep("-", 18), sep = "", "\n\n")
  print(ols_regress(data$model))
  cat("\n")
}


print_step_output <- function(data, direction = c("forward", "backward", "both")) {

  method <- match.arg(direction)

  print_step_zero(data, method)

  mi         <- print_step_mi(data, method)
  metrics    <- print_step_metrics(data, mi)
  predictors <- print_step_predictors(data, method)

  ols_print_output(metrics, predictors)
  ols_print_final_model(data)

}

print_step_zero <- function(data, method) {

  if (length(data$metrics$step) < 1) {
    if (method == "forward") {
      stop("No variables have been added to the model.")
    } else if (method == "backward") {
      stop("No variables have been removed from the model.")
    } else {
      stop("No variables have been added to or removed from the model.")
    }
  }

}

print_step_mi <- function(data, method) {

  if (method == "forward" || method == "both") {
    np <- coeff_names(data$others$base_model)
    mi <- null_model_metrics(data$others$base_model, data$others$full_model)
  } else {
    mi <- ols_regress(data$others$full_model)
  }

  return(mi)
}

print_step_metrics <- function(data, mi) {

  aic   <- c(mi$aic, data$metrics$aic)
  sbc   <- c(mi$sbc, data$metrics$sbc)
  sbic  <- c(mi$sbic, data$metrics$sbic)
  r2    <- c(mi$rsq, data$metrics$r2)
  adjr  <- c(mi$adjr, data$metrics$adj_r2)
  step  <- c(0, data$metrics$step)
  ln    <- length(aic)

  return(list(aic = aic, sbc = sbc, sbic = sbic, r2 = r2, adjr = adjr, step = step, ln = ln))

}

print_step_predictors <- function(data, method) {

  if (method == "both") {
    data$metrics$sign <- ifelse(data$metrics$method == "addition", "+", "-")
    variable <- paste0(data$metrics$variable, " (", data$metrics$sign, ")")
  }

  if (method == "forward") {
    predictors <- c("Base Model", data$metrics$variable)
  } else if (method == "backward") {
    predictors <- c("Full Model", data$metrics$variable)
  } else {
    predictors <- c("Base Model", variable)
  }

  return(predictors)
}

ols_print_output <- function(metrics, predictors) {

  w1 <- nchar("Step")
  w2 <- max(nchar(predictors))
  w3 <- max_nchar("AIC", metrics$aic)
  w4 <- max_nchar("SBC", metrics$sbc)
  w5 <- max_nchar("SBIC", metrics$sbic)
  w6 <- max_nchar("R2", metrics$r2, 5, 5)
  w7 <- max_nchar("Adj. R2", metrics$adjr, 5, 5)
  w  <- sum(w1, w2, w3, w4, w5, w6, w7, 24)

  cat("\n\n", format("Stepwise Summary", width = w, justify = "centre"), "\n")
  cat(rep("-", w), sep = "", "\n")

  cat(format("Step", width = w1, justify = "centre"), fs(),
    fl("Variable", w2), fs(),
    fc("AIC", w3), fs(),
    fc("SBC", w4), fs(),
    fc("SBIC", w5), fs(),
    fc("R2", w6), fs(),
    fc("Adj. R2", w7), "\n")

  cat(rep("-", w), sep = "", "\n")

  for (i in seq_len(metrics$ln)) {
    cat(format(as.character(metrics$step[i]), width = w1, justify = "centre"), fs(),
      fl(predictors[i], w2), fs(),
      fg(format(round(metrics$aic[i], 3), nsmall = 3), w3), fs(),
      fg(format(round(metrics$sbc[i], 3), nsmall = 3), w4), fs(),
      fg(format(round(metrics$sbic[i], 3), nsmall = 3), w5), fs(),
      fg(format(round(metrics$r2[i], 5), nsmall = 5), w6), fs(),
      fg(format(round(metrics$adjr[i], 5), nsmall = 5), w7), "\n")
  }

  cat(rep("-", w), sep = "")
}


ols_rsquared_init <- function(include, metric, response, rsq_base) {

  cat("\n")
    if (is.null(include)) {
      if (metric == "r2") {
        cat("Step   => 0", "\n")
        cat("Model  =>", paste(response, "~", 1, "\n"))
        cat("R2     =>", round(rsq_base, 3), "\n\n")
      } else {
        cat("Step     => 0", "\n")
        cat("Model    =>", paste(response, "~", 1, "\n"))
        cat("Adj. R2  =>", round(rsq_base, 3), "\n\n")
      }
    } else {
      if (metric == "r2") {
        cat("Step   => 0", "\n")
        cat("Model  =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
        cat("R2     =>", round(rsq_base, 3), "\n\n")
      } else {
        cat("Step     => 0", "\n")
        cat("Model    =>", paste(response, "~", paste(include, collapse = " + "), "\n"))
        cat("Adj. R2  =>", round(rsq_base, 3), "\n\n")
      }
    }
    cat("Initiating stepwise selection...", "\n\n")
}

ols_rsquared_selected <- function(metric, step, preds, response, rsq1) {

  if (metric == "r2") {
        cat("Step      =>", step, "\n")
        cat("Selected  =>", tail(preds, n = 1), "\n")
        cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
        cat("R2        =>", round(rsq1, 3), "\n\n")
      } else {
        cat("Step      =>", step, "\n")
        cat("Selected  =>", tail(preds, n = 1), "\n")
        cat("Model     =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
        cat("Adj. R2   =>", round(rsq1, 3), "\n\n")
      }
}


ols_rsquared_removed <- function(metric, step, rpred, preds, response, aic_f) {
  if (metric == "r2") {
          cat("Step     =>", step, "\n")
          cat("Removed  =>", tail(rpred, n = 1), "\n")
          cat("Model    =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
          cat("R2       =>", aic_f, "\n\n")
        } else {
          cat("Step     =>", step, "\n")
          cat("Removed  =>", tail(rpred, n = 1), "\n")
          cat("Model    =>", paste(response, "~", paste(preds, collapse = " + "), "\n"))
          cat("Adj. R2  =>", aic_f, "\n\n")
        }
}

ols_base_criteria <- function(model, criteria) {
  switch (criteria,
    aic    = ols_aic(model),
    sbc    = ols_sbc(model),
    sbic   = ols_sbic(model, model),
    rsq    = summary(model)$r.squared,
    adjrsq = summary(model)$adj.r.squared
  )
}

ols_sort_metrics <- function(data, criteria) {

  mat  <- switch(criteria,
    aic    = "aics",
    sbc    = "bics",
    sbic   = "sbics",
    rsq    = "rsq",
    adjrsq = "arsq")

  if (criteria == "aic" || criteria == "sbc" || criteria == "sbic") {
    data[order(data[[mat]]), ]
  } else {
    data[order(-data[[mat]]), ]
  }
}

ols_threshold <- function(data, criteria) {
  if (criteria == "aic" || criteria == "sbc" || criteria == "sbic") {
    which(data == min(data))
  } else {
    which(data == max(data))
  }
}

ols_f_criteria <- function(criteria, mat, minc, bmetric) {
  if (criteria == "aic" || criteria == "sbc" || criteria == "sbic") {
    mat[minc] < bmetric
  } else {
    mat[minc] > bmetric
  }
}

ols_next_criteria <- function(criteria, mat, minaic, laic, lpreds) {
  if (criteria == "aic" || criteria == "sbc" || criteria == "sbic") {
    mat[minaic] < laic[lpreds]
  } else {
    mat[minaic] > laic[lpreds]
  }
}




