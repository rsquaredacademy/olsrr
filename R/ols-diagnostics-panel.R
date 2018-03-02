#' Diagnostics panel
#'
#' Panel of plots for regression diagnostics.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_plot_diagnostics(model)
#' }
#'
#' @importFrom ggplot2 xlim stat_qq geom_histogram geom_line theme element_blank
#' @importFrom stats quantile
#'
#' @export
#'
ols_plot_diagnostics <- function(model) {

  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  predicted <- NULL
  fct_color <- NULL
  leverage  <- NULL
  levrstud  <- NULL
  color     <- NULL
  pred      <- NULL
  dsr       <- NULL
  obs       <- NULL
  ckd       <- NULL
  txt       <- NULL
  ds        <- NULL
  cd        <- NULL


  # residual vs fitted values plot
  d1 <- rvspdata(model)
  p1 <- ggplot(d1, aes(x = predicted, y = resid)) +
    geom_point(shape = 1, colour = "blue") +
    xlab("Predicted Value") + ylab("Residual") +
    ggtitle("Residual vs Predicted Values") +
    geom_hline(yintercept = 0, colour = "red")

  # deleted studentized residual vs predicted values
  k <- dpred(model)

  d22 <-
    k %>%
    use_series(ds) %>%
    mutate(
      txt = ifelse(color == "outlier", obs, NA)
    )

  d2 <- ggplot(d22, aes(x = pred, y = dsr, label = txt)) +
    geom_point(aes(colour = fct_color)) +
    scale_color_manual(values = c("blue", "red")) +
    ylim(k$cminx, k$cmaxx) + xlab("Predicted Value") +
    ylab("Deleted Studentized Residual") + labs(color = "Observation") +
    ggtitle("Deleted Studentized Residual vs Predicted Values") +
    geom_hline(yintercept = c(-2, 2), colour = "red")

  # studentized residuals vs leverage plot
  j <- rstudlev(model)

  d33 <-
    j %>%
    use_series(levrstud) %>%
    mutate(
      txt = ifelse(color == "normal", NA, obs)
    )

  resp <-
    model %>%
    model.frame() %>%
    names() %>%
    extract(1)

  title <- paste("Outlier and Leverage Diagnostics for", resp)

  d3 <- ggplot(d33, aes(leverage, rstudent, label = txt)) +
    geom_point(shape = 1, aes(colour = fct_color)) + labs("Observation") +
    scale_color_manual(values = c("blue", "red", "green", "violet")) +
    xlim(j$minx, j$maxx) + ylim(j$miny, j$maxy) +
    xlab("Leverage") + ylab("RStudent") + ggtitle(title) +
    geom_hline(yintercept = c(2, -2), colour = "maroon") +
    geom_vline(xintercept = j$lev_thrsh, colour = "maroon")

  # residual qqplot
  resid <- residuals(model)
  y     <- quantile(resid[!is.na(resid)], c(0.25, 0.75))
  x     <- qnorm(c(0.25, 0.75))
  slope <- diff(y) / diff(x)
  int   <- y[1L] - slope * x[1L]
  d4    <- tibble(x = resid)

  p4 <- ggplot(d4, aes(sample = x)) + stat_qq(color = "blue") +
    geom_abline(slope = slope, intercept = int, color = "red") +
    xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
    ggtitle("Normal Q-Q Plot")


  # observed vs fitted values plot
  oname <-
    model %>%
    model.frame() %>%
    names() %>%
    extract(1)

  d5 <- obspred(model)

  p5 <- ggplot(d5, aes(x = x, y = y)) +
    geom_point(color = "blue", shape = 1) +
    xlab("Predicted Value") + ylab(paste(oname)) +
    ggtitle(paste("Observed by Predicted for", oname)) +
    geom_abline(intercept = 0, slope = 1, color = "blue") +
    geom_segment(
      aes(x = min(x), y = min(y), xend = max(x), yend = max(y)),
      colour = "red"
    )

  # cook's d chart
  k <- cdplot(model)
  d <- plot_data(k)

  d6 <- ggplot(d, aes(x = obs, y = cd, label = txt, ymin = min(cd), ymax = cd)) +
    geom_linerange(colour = "blue") + geom_point(shape = 1, colour = "blue") +
    geom_hline(yintercept = k$ts, colour = "red") + xlab("Observation") +
    ylab("Cook's D") + ggtitle("Cook's D Chart")

  # residual fit spread plot
  d    <- fmdata(model)
  ymin <- min(d$y) + (0.25 * min(d$y))
  ymax <- max(d$y) + (0.25 * max(d$y))

  d7 <- ggplot(d, aes(x = x, y = y)) +
    geom_point(shape = 1, color = "blue") +
    xlim(c(-0.2, 1.2)) + ylim(c(ymin, ymax)) +
    xlab("Proportion Less") + ylab("Fit - Mean") +
    ggtitle("Residual Fit Spread Plot")

  da    <- rsdata(model)
  ymina <- min(da$y) + (0.25 * min(da$y))
  ymaxa <- max(da$y) + (0.25 * max(da$y))

  d8 <- ggplot(da, aes(x = x, y = y)) +
    geom_point(color = "blue", shape = 1) +
    ylim(c(ymina, ymaxa)) + xlim(c(-0.2, 1.2)) +
    ylab("Residual") + xlab("Proportion Less") +
    ggtitle("Residual Fit Spread Plot")

  # residual histogram
  b  <- histdata(model)
  h  <- hist(b$resid, plot = FALSE)
  f  <- histn(b$resid, h)
  db <- tibble(x = f$xfit, y = f$yfit)
  d9 <- tibble(x = b$resid)

  p9 <- ggplot(d9, aes(x = x)) +
    geom_histogram(bins = 6, color = "black", fill = "#ADD8E6") +
    geom_line(data = db, aes(x = x, y = y), color = "#0000A0", size = 1.2) +
    xlab("Residuals") + ggtitle("Residual Histogram")

  # residual box plot
  d10 <- tibble(resid = residuals(model))

  p10 <- ggplot(d10, aes(x = factor(0), y = resid)) +
    geom_boxplot(
      outlier.color = "green", outlier.size = 3,
      fill = "grey80", colour = "#3366FF"
    ) +
    xlab(" ") + ylab("Residuals") + ggtitle("Residual Box Plot") +
    theme(axis.text.x = element_blank())

  grid.arrange(p1, d2, d3, p4, p5, d6, d7, d8, p9, p10, ncol = 2)

  result <- list(
    plot_1 = p1, plot_2 = d2, plot_3 = d3, plot_4 = p4,
    plot_5 = p5, plot_6 = d6, plot_7 = d7, plot_8 = d8, plot_9 = p9,
    plot_10 = p10
  )

  invisible(result)
}


#' @export
#' @rdname ols_plot_diagnostics
#' @usage NULL
#'
ols_diagnostic_panel <- function(model) {
  .Deprecated("ols_plot_diagnostics()")
}
