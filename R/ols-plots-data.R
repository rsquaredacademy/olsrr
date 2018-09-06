#' Added variable plot data
#'
#' Data for generating the added variable plots.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_prep_avplot_data(model)
#'
#' @importFrom stats model.frame residuals as.formula
#' @importFrom dplyr bind_cols
#'
#' @export
#'
ols_prep_avplot_data <- function(model) {

  m1 <-
    model %>%
    model.frame() %>%
    as_data_frame()

  m2 <-
    model %>%
    model.matrix() %>%
    as_data_frame() %>%
    select(-1)

  m1 %>%
    select(1) %>%
    bind_cols(m2) %>%
    as_data_frame()

}

#' Regress predictor on other predictors
#'
#' Regress a predictor in the model on all the other predictors.
#'
#' @param data A \code{data.frame}.
#' @param i A numeric vector (indicates the predictor in the model).
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' data <- ols_prep_avplot_data(model)
#' ols_prep_regress_x(data, 1)
#'
#' @importFrom stats lsfit
#'
#' @export
#'
ols_prep_regress_x <- function(data, i) {

  x <- remove_columns(data, i)
  y <- select_columns(data, i)
  lsfit(x, y) %>%
    use_series(residuals)

}

#' Regress y on other predictors
#'
#' Regress y on all the predictors except the ith predictor.
#'
#' @param data A \code{data.frame}.
#' @param i A numeric vector (indicates the predictor in the model).
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' data <- ols_prep_avplot_data(model)
#' ols_prep_regress_y(data, 1)
#'
#' @export
#'
ols_prep_regress_y <- function(data, i) {

  x <- remove_columns(data, i)
  y <- select_columns(data)
  lsfit(x, y) %>%
    use_series(residuals)

}

#' Cooks' D plot data
#'
#' Prepare data for cook's d bar plot.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_prep_cdplot_data(model)
#'
#' @importFrom dplyr if_else
#'
#' @export
#'
ols_prep_cdplot_data <- function(model) {

  cd        <- NULL
  color     <- NULL
  cooksd    <- cooks.distance(model)
  n         <- length(cooksd)
  obs       <- seq_len(n)
  ckd       <- tibble(obs = obs, cd = cooksd)
  ts        <- 4 / n
  cooks_max <- max(cooksd)

  ckd %<>%
    mutate(
      color = if_else(cd >= ts, "outlier", "normal"),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier"))
    )

  maxx <-
    cooks_max %>%
    multiply_by(0.01) %>%
    add(cooks_max)

  list(ckd = ckd, maxx = maxx, ts = ts)

}

#' Cooks' D outlier observations
#'
#' Identify outliers in cook's d plot.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' k <- ols_prep_cdplot_data(model)
#' ols_prep_outlier_obs(k)
#'
#' @export
#'
ols_prep_outlier_obs <- function(k) {

  ckd   <- NULL
  color <- NULL
  obs   <- NULL

  k %>%
    use_series(ckd) %>%
    mutate(
      txt = ifelse(color == "outlier", obs, NA)
    )

}

#' Cooks' d outlier data
#'
#' Outlier data for cook's d bar plot.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' k <- ols_prep_cdplot_data(model)
#' ols_prep_cdplot_outliers(k)
#'
#' @export
#'
ols_prep_cdplot_outliers <- function(k) {

  color <- NULL
  ckd   <- NULL
  obs   <- NULL
  cd    <- NULL

  k %>%
    use_series(ckd) %>%
    filter(
      color == "outlier"
    ) %>%
    select(obs, cd) %>%
    set_colnames(c("observation", "cooks_distance"))

}

#' DFBETAs plot data
#'
#' Prepares the data for dfbetas plot.
#'
#' @param d A \code{tibble} or \code{data.frame} with dfbetas.
#' @param threshold The threshold for outliers.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' dfb <- dfbetas(model)
#' n <- nrow(dfb)
#' threshold <- 2 / sqrt(n)
#' dbetas  <- dfb[, 1]
#' df_data <- tibble::tibble(obs = seq_len(n), dbetas = dbetas)
#' ols_prep_dfbeta_data(df_data, threshold)
#'
#' @export
#'
ols_prep_dfbeta_data <- function(d, threshold) {

  color <- NULL
  obs   <- NULL

  d %>%
    mutate(
      color = ifelse(((d$dbetas >= threshold) | (d$dbetas <= -threshold)),
                     c("outlier"), c("normal")),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier")),
      txt = ifelse(color == "outlier", obs, NA)
    )

}

#' DFBETAs plot outliers
#'
#' Data for identifying outliers in dfbetas plot.
#'
#' @param d A \code{tibble} or \code{data.frame}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' dfb <- dfbetas(model)
#' n <- nrow(dfb)
#' threshold <- 2 / sqrt(n)
#' dbetas  <- dfb[, 1]
#' df_data <- tibble::tibble(obs = seq_len(n), dbetas = dbetas)
#' d <- ols_prep_dfbeta_data(df_data, threshold)
#' ols_prep_dfbeta_outliers(d)
#'
#' @export
#'
ols_prep_dfbeta_outliers <- function(d) {

  color  <- NULL
  obs    <- NULL
  dbetas <- NULL

  d %>%
    filter(
      color == "outlier"
    ) %>%
    select(obs, dbetas)

}
