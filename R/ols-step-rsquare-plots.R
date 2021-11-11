ols_plot_rsquared <- function(x, print_plot = TRUE, details = TRUE) {

  type <- x$others$metric
  direction <- x$others$direction

  if (type == "r2") {
    metric <- "r2"
    m_sum  <- "r.squared"
    y_lab  <- "R-Squared"
  } else {
    metric <- "adj_r2"
    m_sum  <- "adj.r.squared"
    y_lab  <- "Adj. R-Squared"
  }

  if (direction == "forward") {
    nudge <- 0.1
    if (type == "r2") {
      title  <- "Stepwise R-Squared Forward Selection"
    } else {
      title  <- "Stepwise Adjusted R-Squared Forward Selection"
    }
  } else if (direction == "backward") {
    nudge <- 0.1
    if (type == "r2") {
      title  <- "Stepwise R-Squared Backward Selection"
    } else {
      title  <- "Stepwise Adjusted R-Squared Backward Selection"
    }
  } else {
    nudge <- 0.05
    if (type == "r2") {
      title  <- "Stepwise R-Squared Selection"
    } else {
      title  <- "Stepwise Adjusted R-Squared Selection"
    }
  }

  pred <- ols_step_plot_text(x, direction, details, metric)
  data <- ols_stepwise_plot_data(x, pred, metric)
  info <- ols_metric_info(x, direction, metric)

  p <- ols_step_ggplot(data$d, data$d2, data$xmin, data$xmax, data$ymin,
                       data$ymax, y_lab, title, "bottom", 1.2, nudge, Inf, info)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
