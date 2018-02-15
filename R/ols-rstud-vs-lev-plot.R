#' @importFrom dplyr filter
#' @importFrom ggplot2 geom_vline
#' @title Studentized Residuals vs Leverage Plot
#' @description Graph for detecting outliers and/or observations with high leverage.
#' @param model an object of class \code{lm}
#' @examples
#' model <- lm(read ~ write + math + science, data = hsb)
#' ols_rsdlev_plot(model)
#'
#' @export
#'
ols_rsdlev_plot <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  Observation <- NULL
  leverage <- NULL
  txt <- NULL
  obs <- NULL
  resp <- model %>% model.frame() %>% names() %>% `[`(1)
  g <- rstudlev(model)
  d <- g$levrstud
  d <- d %>% mutate(txt = ifelse(Observation == "normal", NA, obs))
  f <- d %>% filter(., Observation == "outlier") %>% select(obs, leverage, rstudent)

  p <- ggplot(d, aes(leverage, rstudent, label = txt)) +
    geom_point(shape = 1, aes(colour = Observation)) +
    scale_color_manual(values = c("blue", "red", "green", "violet")) +
    xlim(g$minx, g$maxx) + ylim(g$miny, g$maxy) +
    xlab("Leverage") + ylab("RStudent") +
    ggtitle(paste("Outlier and Leverage Diagnostics for", resp)) +
    geom_hline(yintercept = c(2, -2), colour = "maroon") +
    geom_vline(xintercept = g$lev_thrsh, colour = "maroon") +
    geom_text(vjust = -1, size = 3, family = "serif", fontface = "italic", colour = "darkred") +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste("Threshold:", round(g$lev_thrsh, 3))
    )

  suppressWarnings(print(p))
  colnames(f) <- c("Observation", "Leverage", "Studentized Residuals")
  result <- list(leverage = f, threshold = g$lev_thrsh, plot = p)
  invisible(result)
}

rstudlev <- function(model) {
  leverage <- unname(hatvalues(model))
  rstudent <- unname(rstudent(model))
  k <- length(model$coefficients)
  n <- nrow(model.frame(model))
  lev_thrsh <- ((2 * k) + 2) / n
  rst_thrsh <- 2
  miny <- min(rstudent) - 3
  maxy <- max(rstudent) + 3
  minx <- min(leverage)
  maxx <- ifelse((max(leverage) > lev_thrsh), max(leverage), (lev_thrsh + 0.05))
  levrstud <- data.frame(obs = seq_len(n), leverage, rstudent)


  levrstud$color[(leverage < lev_thrsh & abs(rstudent) < 2)] <- "normal"
  levrstud$color[(leverage > lev_thrsh & abs(rstudent) < 2)] <- "leverage"
  levrstud$color[(leverage < lev_thrsh & abs(rstudent) > 2)] <- "outlier"
  levrstud$color[(leverage > lev_thrsh & abs(rstudent) > 2)] <- "outlier & leverage"
  levrstud$color3 <- factor(levrstud$color)
  levrstud$Observation <- ordered(
    levrstud$color3,
    levels = c(
      "normal", "leverage", "outlier",
      "outlier & leverage"
    )
  )

  result <- list(
    levrstud = levrstud, lev_thrsh = lev_thrsh, minx = minx,
    miny = miny, maxx = maxx, maxy = maxy
  )
  return(result)
}
