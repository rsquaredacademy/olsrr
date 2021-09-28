plot_rsquared_forward <- function(x, print_plot = TRUE, details = TRUE) {

	tx  <- NULL
  a   <- NULL
  b   <- NULL

  if (x$others$metric == "r2") {
  	metric <- "r2"
  	m_sum  <- "r.squared"
  	y_lab  <- "R-Squared"
  	title  <- "Stepwise R-Squared Forward Selection"
  } else {
  	metric <- "adj_r2"
  	m_sum  <- "adj.r.squared"
  	y_lab  <- "Adj. R-Squared"
  	title  <- "Stepwise Adjusted R-Squared Forward Selection"
  }

  step <- x$metrics$step
  rsq  <- x$metrics[[metric]]

  if (details) {
    x$metrics$text <- paste0("[", x$metrics$variable, ", ", round(x$metrics[[metric]], 2), "]")
    pred <- x$metrics$text
  } else {
    pred <- x$metrics$variable
  }
  
  y    <- step
  xloc <- y
  yloc <- rsq
  xmin <- min(y) - 0.4
  xmax <- max(y) + 1
  ymin <- min(rsq) - (min(rsq) * 0.05)
  ymax <- max(rsq) + (max(rsq) * 0.05)

  d2 <- data.frame(x = xloc, y = yloc, tx = pred)
  d  <- data.frame(a = y, b = rsq)

  # metric info
  base_model_rsq  <- round(summary(x$others$base_model)[[m_sum]], 3)
  final_model_rsq <- round(summary(x$model)[[m_sum]], 3)

  if (x$others$metric == "r2") {
  	metric_info <- paste0("Base Model R-Squared  : ", format(base_model_rsq, nsmall = 3), "\n",
                          "Final Model R-Squared : ", format(final_model_rsq, nsmall = 3))	
  } else {
  	metric_info <- paste0("Base Model Adj. R-Squared  : ", format(base_model_rsq, nsmall = 3), "\n",
                          "Final Model Adj. R-Squared : ", format(final_model_rsq, nsmall = 3))
  }
  

  p <-
    ggplot(d, aes(x = a, y = b)) + 
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + 
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + 
    xlab("Step") + 
    ylab(y_lab) +
    ggtitle(title) +
    geom_text(data = d2, aes(x = x, y = y, label = tx), size = 3,
              hjust = "left", vjust = "bottom", nudge_x = 0.1) +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
             family = "serif", fontface = "italic", size = 3,
             label = metric_info)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}

plot_rsquared_backward <- function(x, print_plot = TRUE, details = TRUE) {

	tx    <- NULL
  a     <- NULL
  b     <- NULL

  if (x$others$metric == "r2") {
  	metric <- "r2"
  	m_sum  <- "r.squared"
  	y_lab  <- "R-Squared"
  	title  <- "Stepwise R-Squared Backward Selection"
  } else {
  	metric <- "adj_r2"
  	m_sum  <- "adj.r.squared"
  	y_lab  <- "Adj. R-Squared"
  	title  <- "Stepwise Adjusted R-Squared Backward Selection"
  }

  preds <- x$metrics$variable
  rsq   <- x$metrics[[metric]]
  step  <- x$metrics$step

  if (details) {
    x$metrics$text <- paste0("[", x$metrics$variable, ", ", round(x$metrics[[metric]], 2), "]")
    pred <- x$metrics$text
  } else {
    pred <- x$metrics$variable
  }

  y     <- step
  xloc  <- y 
  yloc  <- rsq
  xmin  <- min(y) - 0.4
  xmax  <- max(y) + 1
  ymin  <- min(rsq) - (min(rsq) * 0.05)
  ymax  <- max(rsq) + (max(rsq) * 0.05)

  d2    <- data.frame(x = xloc, y = yloc, tx = pred)
  d     <- data.frame(a = y, b = rsq)

  # metric info
  full_model_rsq  <- round(summary(x$others$full_model)[[m_sum]], 3)
  final_model_rsq <- round(summary(x$model)[[m_sum]], 3)

  if (x$others$metric == "r2") {
  	metric_info <- paste0("Base Model R-Squared  : ", format(full_model_rsq, nsmall = 3), "\n",
                          "Final Model R-Squared : ", format(final_model_rsq, nsmall = 3))	
  } else {
  	metric_info <- paste0("Base Model Adj. R-Squared  : ", format(full_model_rsq, nsmall = 3), "\n",
                          "Final Model Adj. R-Squared : ", format(final_model_rsq, nsmall = 3))
  }

  p <-
    ggplot(d, aes(x = a, y = b)) + 
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + 
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + 
    xlab("Step") + 
    ylab(y_lab) +
    ggtitle(title) +
    geom_text(data = d2, aes(x = x, y = y, label = tx), size = 3,
              vjust = "bottom", hjust = "left", nudge_x = 0.1) +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
             family = "serif", fontface = "italic", size = 3,
             label = metric_info)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}

plot_rsquared_both <- function(x, print_plot = TRUE, details = TRUE) {

	tx  <- NULL
  a   <- NULL
  b   <- NULL

  if (x$others$metric == "r2") {
  	metric <- "r2"
  	m_sum  <- "r.squared"
  	y_lab  <- "R-Squared"
  	title  <- "Stepwise R-Squared Selection"
  } else {
  	metric <- "adj_r2"
  	m_sum  <- "adj.r.squared"
  	y_lab  <- "Adj. R-Squared"
  	title  <- "Stepwise Adjusted R-Squared Selection"
  }

  step <- x$metrics$step
  rsq  <- x$metrics[[metric]]

  # text annotation
  if (details) {
    x$metrics$text <- ifelse(x$metrics$method == "addition", 
                           paste0("[+", x$metrics$variable, ", ", round(x$metrics[[metric]], 2), "]"), 
                           paste0("[-", x$metrics$variable, ", ", round(x$metrics[[metric]], 2), "]"))
    pred <- x$metrics$text
  } else {
    x$metrics$text <- ifelse(x$metrics$method == "addition", 
                             paste0("+", x$metrics$variable),
                             paste0("-", x$metrics$variable))
    pred <- x$metrics$text
  }
  
  y     <- step
  xloc  <- y 
  yloc  <- rsq 
  xmin  <- min(y) - 0.4
  xmax  <- max(y) + 1
  ymin  <- min(rsq) - (min(rsq) * 0.05)
  ymax  <- max(rsq) + (max(rsq) * 0.05)

  d2 <- data.frame(x = xloc, y = yloc, tx = pred)
  d  <- data.frame(a = y, b = rsq)

  # metric info
  base_model_rsq  <- round(summary(x$others$base_model)[[m_sum]], 3)
  final_model_rsq <- round(summary(x$model)[[m_sum]], 3)

  if (x$others$metric == "r2") {
  	metric_info <- paste0("Base Model R-Squared  : ", format(base_model_rsq, nsmall = 3), "\n",
                          "Final Model R-Squared : ", format(final_model_rsq, nsmall = 3))	
  } else {
  	metric_info <- paste0("Base Model Adj. R-Squared  : ", format(base_model_rsq, nsmall = 3), "\n",
                          "Final Model Adj. R-Squared : ", format(final_model_rsq, nsmall = 3))
  }

  p <-
    ggplot(d, aes(x = a, y = b)) + 
    geom_line(color = "blue") +
    geom_point(color = "blue", 
               shape = 1, 
               size = 2) + 
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + 
    xlab("Step") + 
    ylab(y_lab) +
    ggtitle(title) +
    geom_text(data = d2, aes(x = x, y = y, label = tx), size = 3, 
              hjust = "left", vjust = "bottom", nudge_x = 0.05) +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
             family = "serif", fontface = "italic", size = 3,
             label = metric_info)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }
}