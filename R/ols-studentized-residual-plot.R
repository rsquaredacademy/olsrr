#' @importFrom ggplot2 scale_fill_manual annotate
#' @title Studentized Residual Plot
#' @description Graph for identifying outliers
#' @param model an object of class \code{lm}
#' @details Studentized deleted residuals (or externally studentized residuals) is the deleted residual
#' divided by its estimated standard deviation. Studentized residuals are going to be more effective for
#' detecting outlying Y observations than standardized residuals. If an observation has an externally
#' studentized residual that is larger than 3 (in absolute value) we can call it an outlier.
#' @return \code{ols_srsd_plot} returns  a list containing the
#' following components:
#'
#' \item{outliers}{a tibble with observation number and \code{studentized residuals} that
#' exceed \code{threshold}} for classifying an observation as an outlier
#' \item{threshold}{\code{threshold} for classifying an observation as an outlier}
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_srsd_plot(model)
#' @export
#'
ols_srsd_plot <- function(model) {
  if (!all(class(model) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  obs <- NULL
  dsr <- NULL
  txt <- NULL
  Observation <- NULL
  g <- srdata(model)
  d <- g$dsr
  d <- d %>% mutate(txt = ifelse(Observation == "outlier", obs, NA))
  f <- d %>% filter(., Observation == "outlier") %>% select(obs, dsr)

  p <- ggplot(d, aes(x = obs, y = dsr, label = txt)) +
    geom_bar(width = 0.5, stat = "identity", aes(fill = Observation)) +
    scale_fill_manual(values = c("blue", "red")) + xlab("Observation") +
    ylab("Deleted Studentized Residuals") + ggtitle("Studentized Residuals Plot") +
    ylim(g$cminx, g$cmaxx) + geom_hline(yintercept = c(0, g$nseq, g$pseq)) +
    geom_hline(yintercept = c(-3, 3), color = "red") +
    geom_text(hjust = -0.2, nudge_x = 0.05, size = 2, na.rm = TRUE) +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = paste0("Threshold: abs(", 3, ")")
    )


  suppressWarnings(print(p))
  colnames(f) <- c("Observation", "Studentized Residuals")
  result <- list(outliers = f, threshold = 3, plot = p)
  invisible(result)
}

srdata <- function(model) {
  dstud <- unname(rstudent(model))
  n <- length(dstud)
  dsr <- tibble(obs = seq_len(n), dsr = dstud)
  dsr <- dsr %>%
    mutate(color = ifelse((abs(dsr) >= 3), "outlier", "normal"))

  dsr$color1 <- factor(dsr$color)
  dsr$Observation <- ordered(dsr$color1, levels = c("normal", "outlier"))
  cminxx <- dsr$dsr %>% min() %>% `-`(1) %>% floor()
  cmaxxx <- dsr$dsr %>% max() %>% `-`(1) %>% floor()
  # cminx <- dsr$dsr %>% min() %>% `-`(1) %>% floor()
  # cmaxx <- dsr$dsr %>% max() %>% `-`(1) %>% floor()
  cminx <- ifelse(cminxx > -3, -3, cminxx)
  cmaxx <- ifelse(cmaxxx < 3, 3, cmaxxx)
  nseq <- seq_len(abs(0 + cminx + 1)) * -1
  pseq <- seq_len(0 + cmaxx - 1)

  result <- list(dsr = dsr, cminx = cminx, cmaxx = cmaxx, nseq = nseq, pseq = pseq)
  return(result)
}
