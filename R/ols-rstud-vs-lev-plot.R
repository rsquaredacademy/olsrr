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

  resp <-
    model %>%
    model.frame() %>%
    names() %>%
    extract(1)

  title <- paste("Outlier and Leverage Diagnostics for", resp)

  g <- rstudlev(model)

  ann_paste <-
    g %>%
    use_series(lev_thrsh) %>%
    round(3)

  ann_label <- paste("Threshold:", ann_paste)

  d <-
    g %>%
    use_series(levrstud) %>%
    mutate(
      txt = ifelse(color == "normal", NA, obs)
    )

  f <-
    d %>%
    filter(color == "outlier") %>%
    select(obs, leverage, rstudent) %>%
    set_colnames(c("Observation", "Leverage", "Studentized Residuals"))

  p <- ggplot(d, aes(leverage, rstudent, label = txt)) +
    geom_point(shape = 1, aes(colour = fct_count)) + labs("Observation") +
    scale_color_manual(values = c("blue", "red", "green", "violet")) +
    xlim(g$minx, g$maxx) + ylim(g$miny, g$maxy) +
    xlab("Leverage") + ylab("RStudent") + ggtitle(title) +
    geom_hline(yintercept = c(2, -2), colour = "maroon") +
    geom_vline(xintercept = g$lev_thrsh, colour = "maroon") +
    geom_text(vjust = -1, size = 3, family = "serif", fontface = "italic", colour = "darkred") +
    annotate(
      "text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
      family = "serif", fontface = "italic", colour = "darkred",
      label = ann_label)

  suppressWarnings(print(p))
  result <- list(leverage = f, threshold = g$lev_thrsh, plot = p)
  invisible(result)

}

rstudlev <- function(model) {

  leverage <-
    model %>%
    hatvalues() %>%
    unname()

  rstudent <- model %>%
    rstudent() %>%
    unname()

  k <-
    model %>%
    coefficients() %>%
    length()

  n <-
    model %>%
    model.frame() %>%
    nrow()

  lev_thrsh <-
    2 %>%
    multiply_by(k) %>%
    add(2) %>%
    divide_by(n)

  rst_thrsh <- 2

  miny <-
    rstudent %>%
    min() %>%
    subtract(3)

  maxy <-
    rstudent %>%
    max() %>%
    add(3)

  minx <- min(leverage)
  maxx <- ifelse((max(leverage) > lev_thrsh), max(leverage),
                 (lev_thrsh + 0.05))

  levrstud <-
    tibble(obs = seq_len(n), leverage, rstudent) %>%
    mutate(

      color = case_when(
        (leverage < lev_thrsh & abs(rstudent) < 2) ~ "normal",
        (leverage > lev_thrsh & abs(rstudent) < 2) ~ "leverage",
        (leverage < lev_thrsh & abs(rstudent) > 2) ~ "outlier",
        TRUE ~ "outlier & leverage"

      ),

      fct_color = color %>%
        factor() %>%
        ordered(
          levels = c(
            "normal", "leverage", "outlier",
            "outlier & leverage"
          )
        )

    )

  list(
    levrstud = levrstud, lev_thrsh = lev_thrsh, minx = minx,
    miny = miny, maxx = maxx, maxy = maxy
  )

}
