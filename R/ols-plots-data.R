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
    as.data.frame()

  m2 <-
    model %>%
    model.matrix() %>%
    as.data.frame() %>%
    select(-1)

  m1 %>%
    select(1) %>%
    bind_cols(m2) %>%
    as.data.frame()

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
#' @param model An object of class \code{lm}.
#'
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
  ckd       <- data.frame(obs = obs, cd = cooksd)
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
#' @param k Cooks' d bar plot data.
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
#' @param k Cooks' d bar plot data.
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
#' df_data <- data.frame(obs = seq_len(n), dbetas = dbetas)
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
#' df_data <- data.frame(obs = seq_len(n), dbetas = dbetas)
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


#' Deleted studentized residual plot data
#'
#' Generates data for deleted studentized residual vs fitted plot.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_prep_dsrvf_data(model)
#'
#' @importFrom magrittr %<>%
#'
#' @export
#'
ols_prep_dsrvf_data <- function(model) {

  dsr   <- NULL
  color <- NULL
  pred  <- fitted(model)

  dsresid <-
    model %>%
    rstudent() %>%
    unname()

  n  <- length(dsresid)
  ds <- data.frame(obs = seq_len(n), dsr = dsresid)

  ds %<>%
    mutate(
      color = ifelse((abs(dsr) >= 2), "outlier", "normal"),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier"))
    )

  ds2 <- data.frame(obs       = seq_len(n),
                pred      = pred,
                dsr       = ds$dsr,
                color     = ds$color,
                fct_color = ds$fct_color)

  minx <-
    ds2 %>%
    use_series(dsr) %>%
    min() %>%
    subtract(1)

  maxx <-
    ds2 %>%
    use_series(dsr) %>%
    max() %>%
    add(1)

  cminx <- ifelse(minx < -2, minx, -2.5)
  cmaxx <- ifelse(maxx > 2, maxx, 2.5)

  list(ds = ds2, cminx = cminx, cmaxx = cmaxx)

}


#' Residual fit spread plot data
#'
#' Data for generating residual fit spread plot.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_prep_rfsplot_fmdata(model)
#' ols_prep_rfsplot_rsdata(model)
#'
#' @export
#'
ols_prep_rfsplot_fmdata<- function(model) {

  predicted <- fitted(model)
  pred_m    <- mean(predicted)
  y         <- predicted - pred_m
  percenti  <- ecdf(y)
  x         <- percenti(y)

  data.frame(x, y)

}

#' @rdname ols_prep_rfsplot_fmdata
#' @export
#'
ols_prep_rfsplot_rsdata <- function(model) {

  y         <- residuals(model)
  residtile <- ecdf(y)
  x         <- residtile(y)

  data.frame(x, y)

}

#' Residual vs regressor plot data
#'
#' Data for generating residual vs regressor plot.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_prep_rvsrplot_data(model)
#'
#' @export
#'
ols_prep_rvsrplot_data <- function(model) {

  np <-
    model %>%
    coefficients() %>%
    length() %>%
    subtract(1)

  dat <-
    model %>%
    model.frame() %>%
    select(-1)

  pnames <-
    model %>%
    coefficients() %>%
    names() %>%
    extract(-1)

  list(np = np, dat = dat, pnames = pnames)

}

#' Studentized residual vs leverage plot data
#'
#' Generates data for studentized resiudual vs leverage plot.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(read ~ write + math + science, data = hsb)
#' ols_prep_rstudlev_data(model)
#'
#' @importFrom dplyr case_when
#'
#' @export
#'
ols_prep_rstudlev_data <- function(model) {

  color <- NULL

  leverage <-
    model %>%
    hatvalues() %>%
    unname()

  rstudent <-
    model %>%
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
    data.frame(obs = seq_len(n), leverage, rstudent) %>%
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

  list(levrstud  = levrstud,
       lev_thrsh = lev_thrsh,
       minx      = minx,
       miny      = miny,
       maxx      = maxx,
       maxy      = maxy
  )

}

#' Studentized residual plot data
#'
#' Generates data for studentized residual plot.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(read ~ write + math + science, data = hsb)
#' ols_prep_srplot_data(model)
#'
#' @export
#'
ols_prep_srplot_data<- function(model) {

  color <- NULL

  dstud <-
    model %>%
    rstudent() %>%
    unname()

  obs <-
    dstud %>%
    length() %>%
    seq_len(.)

  dsr <-
    data.frame(obs = obs, dsr = dstud) %>%
    mutate(
      color = ifelse((abs(dsr) >= 3), "outlier", "normal"),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier"))
    )

  cminxx <-
    dsr %>%
    use_series(dsr) %>%
    min() %>%
    subtract(1) %>%
    floor(.)

  cmaxxx <-
    dsr %>%
    use_series(dsr) %>%
    max() %>%
    subtract(1) %>%
    floor(.)

  cminx <- ifelse(cminxx > -3, -3, cminxx)
  cmaxx <- ifelse(cmaxxx < 3, 3, cmaxxx)

  nseq <-
    0 %>%
    add(cminx) %>%
    add(1) %>%
    abs() %>%
    seq_len(.) %>%
    multiply_by(-1)

  pseq <-
    0 %>%
    add(cmaxx) %>%
    subtract(1) %>%
    seq_len(.)

  list(cminx = cminx,
       cmaxx = cmaxx,
       nseq  = nseq,
       pseq  = pseq,
       dsr   = dsr)

}

#' Standardized residual chart data
#'
#' Generates data for standardized residual chart.
#'
#' @param model An object of class \code{lm}.
#'
#' @examples
#' model <- lm(read ~ write + math + science, data = hsb)
#' ols_prep_srchart_data(model)
#'
#' @importFrom magrittr is_greater_than
#'
#' @export
#'
ols_prep_srchart_data <- function(model) {

  color <- NULL

  sdres <- rstandard(model)

  sdres_out <-
    sdres %>%
    abs() %>%
    is_greater_than(2)

  outlier <-
    sdres %>%
    extract(sdres_out)

  obs <-
    sdres %>%
    length() %>%
    seq_len(.)

  data.frame(obs = obs, sdres = sdres) %>%
    mutate(
      color = ifelse(((sdres >= 2) | (sdres <= -2)), c("outlier"), c("normal")),
      fct_color = color %>%
        factor() %>%
        ordered(levels = c("normal", "outlier")),
      txt = ifelse(color == "outlier", obs, NA)
    )

}
