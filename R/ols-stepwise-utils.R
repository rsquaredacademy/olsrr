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
    }
  }

}

ols_stepaic_plot <- function(x, direction = c("forward", "backward", "both"), details = TRUE) {

  type <- match.arg(direction)
  pred <- ols_step_plot_text(x, type, details)
  data <- ols_stepwise_plot_data(x, pred)
  info <- ols_metric_info(x, type)
  ols_stepaic_plot_build(data$d, data$d2, data$xmin, data$xmax, data$ymin, data$ymax, info, type)

}

ols_stepwise_plot_data <- function(x, pred) {

  tx    <- NULL
  a     <- NULL
  b     <- NULL

  step <- x$metrics$step
  aic  <- x$metrics$aic
  xmin <- min(step) - 0.4
  xmax <- max(step) + 1
  ymin <- min(aic) - (min(aic) * 0.05)
  ymax <- max(aic) + (max(aic) * 0.05)
  d2   <- data.frame(x = step, y = aic, tx = pred)
  d    <- data.frame(a = step, b = aic)

  return(list(d = d, d2 = d2, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
}


ols_step_plot_text <- function(x, direction = c("forward", "backward", "both"), details = TRUE) {

  method <- match.arg(direction)

  if (method == "forward" || method == "backward") {
    if (details) {
      pred <- paste0("[", x$metrics$variable, ", ", round(x$metrics$aic, 2), "]")
    } else {
      pred <- x$metrics$variable
    }
  } else {
    if (details) {
      pred <- ifelse(x$metrics$method == "addition", 
                             paste0("[+", x$metrics$variable, ", ", round(x$metrics$aic, 2), "]"), 
                             paste0("[-", x$metrics$variable, ", ", round(x$metrics$aic, 2), "]"))
    } else {
      pred <- ifelse(x$metrics$method == "addition", 
                               paste0("+", x$metrics$variable),
                               paste0("-", x$metrics$variable))
    }
  }

  return(pred)
}


ols_metric_info <- function(x, direction = c("forward", "backward", "both")) {

  method <- match.arg(direction)

  final_model_aic <- round(ols_aic(x$model), 3)
  final_aic <- paste0("Final Model AIC : ", format(final_model_aic, nsmall = 3))

  if (method == "forward" || method == "both") {
    base_model_aic  <- round(ols_aic(x$others$base_model), 3)
    metric_info <- paste0("Base Model AIC  : ", format(base_model_aic, nsmall = 3), "\n", final_aic)
  } else {
    full_model_aic  <- round(ols_aic(x$others$full_model), 3)
    metric_info <- paste0("Full Model AIC  : ", format(full_model_aic, nsmall = 3), "\n", final_aic)
  }

  return(metric_info)

}

#' @importFrom ggplot2 aes_string
ols_stepaic_plot_build <- function(d, d2, xmin, xmax, ymin, ymax, metric_info, direction = c("forward", "backward", "both")) {

  method <- match.arg(direction)

  if (method == "forward") {
    title <- "Stepwise AIC Forward Selection"
    nudge <- 0.1
  } else if (method == "backward") {
    title <- "Stepwise AIC Backward Elimination"
    nudge <- 0.1
  } else {
    title <- "Stepwise AIC Both Direction Selection"
    nudge <- 0.5
  }

  ggplot(d, aes_string(x = "a", y = "b")) + 
    geom_line(color = "blue") +
    geom_point(color = "blue", 
               shape = 1, 
               size = 2) + 
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + 
    xlab("Step") + 
    ylab("AIC") +
    ggtitle(title) +
    geom_text(data = d2, aes_string(x = "x", y = "y", label = "tx"), size = 3, 
              hjust = "left", vjust = "bottom", nudge_x = nudge) +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
             family = "serif", fontface = "italic", size = 3,
             label = metric_info)

}


plot_stepwise <- function(x, metric = "r2", y_lab = "R-Square", details = TRUE, 
  direction = c("forward", "backward", "both")) {

  method <- match.arg(direction)
  step <- x$metrics$step
  r2   <- x$metrics[[metric]]

  if (method == "forward" || method == "backward") {
    if (details) {
      pred <- paste0("[", x$metrics$variable, ", ", round(x$metrics[[metric]], 2), "]")
    } else {
      pred <- x$metrics$variable
    }
  } else {
    if (details) {
      pred <- ifelse(x$metrics$method == "addition", 
                               paste0("[+", x$metrics$variable, ", ", round(x$metrics[[metric]], 2), "]"), 
                               paste0("[-", x$metrics$variable, ", ", round(x$metrics[[metric]], 2), "]"))
    } else {
      pred <- ifelse(x$metrics$method == "addition", 
                               paste0("+", x$metrics$variable),
                               paste0("-", x$metrics$variable))
    }
  }

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
    mi <- null_model_metrics(the_model)
  } else {
    mi <- ols_regress(the_model)
  }

  base_model_met  <- round(mi[[met]], 3)
  final_model_met <- round(ols_regress(x$model)[[met]], 3)
  metric_info <- paste0(the_info, format(base_model_met, nsmall = 3), "\n",
                        "Final Model : ", format(final_model_met, nsmall = 3))

  y    <- step
  xloc <- y
  yloc <- r2
  xmin <- min(y) - 1
  xmax <- max(y) + 1
  ymin <- min(r2) - (min(r2) * 0.03)
  ymax <- max(r2) + (max(r2) * 0.03)

  a <- NULL
  b <- NULL
  tx <- NULL
  
  d  <- data.frame(a = y, b = r2)
  d2 <- data.frame(x = xloc, y = yloc, tx = pred)
  
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
  
  ggplot(d, aes(x = a, y = b)) +
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) +
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + 
    xlab("Step") + 
    ylab(y_lab) + 
    ggtitle(title) +
    geom_text(data = d2, aes(x = x, y = y, label = tx), size = 3,
              hjust = "left", vjust = v_just, nudge_x = 0.05) +
    annotate("text", x = ann_x, y = Inf, hjust = h_just, vjust = 2,
             family = "serif", fontface = "bold", size = 3,
             label = metric_info)

}

ols_print_final_model <- function(data) {
  cat("\n\n")
  cat("Final Model Output", "\n")
  cat(rep("-", 18), sep = "", "\n\n") 
  print(ols_regress(data$model))
  cat("\n")
}


print_step_aic <- function(data, direction = c("forward", "backward", "both")) {

  method <- match.arg(direction)

  if (length(data$metrics$step) < 1) {
    if (method == "forward") {
      stop("No variables have been added to the model.")  
    } else if (method == "backward") {
      stop("No variables have been removed from the model.")
    } else {
      stop("No variables have been added to or removed from the model.")
    }
  }

  if (method == "forward" || method == "both") {
    np <- coeff_names(data$others$base_model)

    if (is.null(np)) {
      mi <- null_model_metrics(data$others$base_model)
    } else {
      mi <- ols_regress(data$others$base_model)
    }
  } else {
    mi <- ols_regress(data$others$full_model)
  }

  aic  <- c(mi$aic, data$metrics$aic)
  ess  <- c(mi$ess, data$metrics$ess)
  rss  <- c(mi$rss,  data$metrics$rss)
  r2   <- c(mi$rsq, data$metrics$r2)
  adjr <- c(mi$adjr, data$metrics$adj_r2) 
  step <- c(0, data$metrics$step)
  ln   <- length(aic)

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

  if (method == "both") {
    methods <- c("", data$metrics$method)
  }
  
  w1 <- nchar("Step")
  w2 <- max(nchar(predictors))
  w3 <- max_nchar("AIC", aic)
  w4 <- max_nchar("Sum Sq", rss)
  w5 <- max_nchar("ESS", ess)
  w6 <- max_nchar("R-Sq", r2, 5, 5)
  w7 <- max_nchar("Adj. R-Sq", adjr, 5, 5)
  
  w <- sum(w1, w2, w3, w4, w5, w6, w7, 24)  
  
  cat("\n\n", format("Stepwise Summary", width = w, justify = "centre"), "\n")
  cat(rep("-", w), sep = "", "\n")

  cat(
    format("Step", width = w1, justify = "centre"), fs(),
    fl("Variable", w2), fs(), 
    fc("AIC", w3), fs(),
    fc("Sum Sq", w4), fs(), 
    fc("ESS", w5), fs(), 
    fc("R-Sq", w6), fs(),
    fc("Adj. R-Sq", w7), "\n"
  )

  cat(rep("-", w), sep = "", "\n")
  
  for (i in seq_len(ln)) {
    cat(
      format(as.character(step[i]), width = w1, justify = "centre"), fs(),
      fl(predictors[i], w2), fs(), 
      fg(format(round(aic[i], 3), nsmall = 3), w3), fs(),
      fg(format(round(rss[i], 3), nsmall = 3), w4), fs(),
      fg(format(round(ess[i], 3), nsmall = 3), w5), fs(),
      fg(format(round(r2[i], 5), nsmall = 5), w6), fs(),
      fg(format(round(adjr[i], 5), nsmall = 5), w7), "\n"
    )  
  }
  
  cat(rep("-", w), sep = "")
  
  ols_print_final_model(data)

}









