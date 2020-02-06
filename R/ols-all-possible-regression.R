#' All possible regression
#'
#' @description
#' Fits all regressions involving one regressor, two regressors, three
#' regressors, and so on. It tests all possible subsets of the set of potential
#' independent variables.
#'
#' @param model An object of class \code{lm}.
#' @param x An object of class \code{ols_best_subset}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#'
#' @return \code{ols_step_all_possible} returns an object of class \code{"ols_step_all_possible"}.
#' An object of class \code{"ols_step_all_possible"} is a data frame containing the
#' following components:
#'
#' \item{n}{model number}
#' \item{predictors}{predictors in the model}
#' \item{rsquare}{rsquare of the model}
#' \item{adjr}{adjusted rsquare of the model}
#' \item{predrsq}{predicted rsquare of the model}
#' \item{cp}{mallow's Cp}
#' \item{aic}{akaike information criteria}
#' \item{sbic}{sawa bayesian information criteria}
#' \item{sbc}{schwarz bayes information criteria}
#' \item{gmsep}{estimated MSE of prediction, assuming multivariate normality}
#' \item{jp}{final prediction error}
#' \item{pc}{amemiya prediction criteria}
#' \item{sp}{hocking's Sp}
#'
#' @references
#' Mendenhall William and  Sinsich Terry, 2012, A Second Course in Statistics Regression Analysis (7th edition).
#' Prentice Hall
#'
#' @section Deprecated Function:
#' \code{ols_all_subset()} has been deprecated. Instead use \code{ols_step_all_possible()}.
#'
#' @family variable selection procedures
#'
#' @examples
#' model <- lm(mpg ~ disp + hp, data = mtcars)
#' k <- ols_step_all_possible(model)
#' k
#'
#' # plot
#' plot(k)
#'
#' @importFrom utils combn
#' @importFrom dplyr group_by summarise_all
#' @importFrom magrittr add use_series
#'
#' @export
#'
ols_step_all_possible <- function(model, ...) UseMethod("ols_step_all_possible")

#' @export
#'
ols_step_all_possible.default <- function(model, ...) {

  check_model(model)
  check_npredictors(model, 3)

  metrics <- allpos_helper(model)

  ui <- data.frame(
    n          = metrics$lpreds,
    predictors = unlist(metrics$preds),
    rsquare    = unlist(metrics$rsq),
    adjr       = unlist(metrics$adjrsq),
    predrsq    = unlist(metrics$predrsq),
    cp         = unlist(metrics$cp),
    aic        = unlist(metrics$aic),
    sbic       = unlist(metrics$sbic),
    sbc        = unlist(metrics$sbc),
    msep       = unlist(metrics$msep),
    fpe        = unlist(metrics$fpe),
    apc        = unlist(metrics$apc),
    hsp        = unlist(metrics$hsp),
    stringsAsFactors = F
  )

  sorted <- c()

  for (i in seq_len(metrics$lc)) {
    temp   <- ui[metrics$q[i]:metrics$t[i], ]
    temp   <- temp[order(temp$rsquare, decreasing = TRUE), ]
    sorted <- rbind(sorted, temp)
  }

  mindex <- seq_len(nrow(sorted))
  sorted <- cbind(mindex, sorted)

  class(sorted) <- c("ols_step_all_possible", "data.frame")

  return(sorted)
}


#' @export
#' @rdname ols_step_all_possible
#' @usage NULL
#'
ols_all_subset <- function(model, ...) {
  .Deprecated("ols_step_all_possible()")
}


#' @export
#'
print.ols_step_all_possible <- function(x, ...) {

  mindex <- NULL
  n <- max(x$mindex)
  k <- data.frame(x)[, c(1:5, 7)]

  names(k) <- c("Index", "N", "Predictors", "R-Square", "Adj. R-Square",
                "Mallow's Cp")

  print(k)

}

#' @export
#' @rdname ols_step_all_possible
#'
plot.ols_step_all_possible <- function(x, model = NA, print_plot = TRUE, ...) {

  n       <- NULL
  y       <- NULL
  k       <- NULL
  tx      <- NULL
  size    <- NULL
  shape   <- NULL
  rsquare <- NULL
  cp      <- NULL
  adjr    <- NULL
  cps     <- NULL
  aic     <- NULL
  sbic    <- NULL
  sbc     <- NULL

  d <- data.frame(index = x$mindex, n = x$n, rsquare = x$rsquare, adjr = x$adjr,
                  cp = x$cp, aic = x$aic, sbic = x$sbic, sbc = x$sbc)
  d$cps <- abs(d$n) - d$cp

  p1 <- all_possible_plot(d, "rsquare", title = "R-Square")
  p2 <- all_possible_plot(d, "adjr", title = "Adj. R-Square")
  p3 <- all_possible_plot(d, "cps", title = "Cp")
  p4 <- all_possible_plot(d, "aic", title = "AIC")
  p5 <- all_possible_plot(d, "sbic", title = "SBIC")
  p6 <- all_possible_plot(d, "sbc", title = "SBC")

  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3,
                  plot_4 = p4, plot_5 = p5, plot_6 = p6)

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}

#' All possible regression plot
#'
#' Generate plots for best subset regression.
#'
#' @importFrom ggplot2 ggtitle scale_shape_manual scale_size_manual scale_color_manual ggtitle geom_text
#' @importFrom rlang enquo !!
#'
#' @param d1 A data.frame.
#' @param d2 A data.frame.
#' @param title Plot title.
#'
#' @noRd
#'
all_possible_plot <- function(d, var, title = "R-Square") {

  n     <- NULL
  x     <- NULL
  y     <- NULL
  shape <- NULL
  size  <- NULL
  tx    <- NULL

  d1    <- d[, c("n", var)]
  colnames(d1) <- c("x", "y")
  maxs  <- all_pos_maxs(d, var, title)
  lmaxs <- all_pos_lmaxs(maxs)
  index <- all_pos_index(d, var, title)

  d2 <- data.frame(x = lmaxs, y = maxs, tx = index, shape = 6, size = 4)

  ggplot(d1, aes(x = x, y = y)) + geom_point(color = "blue", size = 2) +
    xlab("") + ylab("") + ggtitle(title) +
    geom_point(data = d2, aes(x = x, y = y, shape = factor(shape),
      color = factor(shape), size = factor(size))) +
    scale_shape_manual(values = c(2), guide = FALSE) +
    scale_size_manual(values = c(4), guide = FALSE) +
    scale_color_manual(values = c("red"), guide = FALSE) +
    geom_text(data = d2, aes(label = tx), hjust = 0, nudge_x = 0.1)

}

#' @importFrom dplyr summarise
#' @import data.table
all_pos_maxs <- function(d, var, title = "R-Square") {

  n       <- NULL
  is_dt   <- is.data.table(d)
  d_class <- class(d)

  if(!is_dt) {
    d <- data.table(d)
  }

  if (title == "R-Square" | title == "Adj. R-Square") {
    out <- d[, list(maximum = max(get(var))), by = n]
  } else {
    out <- d[, list(minimum = min(get(var))), by = n]
  }

  if(!is_dt) {
    class(out) <- d_class
  }
  out[[2]]
}

all_pos_lmaxs <- function(maxs) {

  seq_len(length(maxs))

}

all_pos_index <- function(d, var, title = "R-Square") {

  n       <- NULL
  index   <- c()
  is_dt   <- is.data.table(d)
  d_class <- class(d)

  if(!is_dt) {
    d <- data.table(d)
  }

  if (title == "R-Square" | title == "Adj. R-Square") {
    m <- d[, list(maximum = max(get(var))), by = n]
  } else {
    m <- d[, list(minimum = min(get(var))), by = n]
  }

  colnames(m) <- c("n", var)
  k  <- split(d, by = "n")

  for (i in m$n) {
    j <- which(part_2(m, var, i) == part_3(k, var, i))
    index[i] <- part_1(k, i)[j] 
  }

  return(index)

}


part_1 <- function(k, i) {
  index <- NULL
  k[[i]]$index
}

part_2 <- function(m, var, i) {
  m[[var]][i]
}

part_3 <- function(k, var, i) {
  k[[i]][[var]]
}


#' All possible regression variable coefficients
#'
#' Returns the coefficients for each variable from each model.
#'
#' @param object An object of class \code{lm}.
#' @param ... Other arguments.
#'
#' @return \code{ols_step_all_possible_betas} returns a \code{data.frame} containing:
#'
#' \item{model_index}{model number}
#' \item{predictor}{predictor}
#' \item{beta_coef}{coefficient for the predictor}
#'
#' @examples
#' \dontrun{
#' model <- lm(mpg ~ disp + hp + wt, data = mtcars)
#' ols_step_all_possible_betas(model)
#' }
#'
#' @export
#'
ols_step_all_possible_betas <- function(object, ...) {

  if (!all(class(object) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  if (length(object$coefficients) < 3) {
    stop("Please specify a model with at least 2 predictors.", call. = FALSE)
  }

  betas  <- NULL
  rsq    <- NULL
  lpreds <- NULL

  metrics    <- allpos_helper(object)
  beta_names <- names(metrics$betas)
  mindex     <- seq_len(length(metrics$rsq))
  reps       <- metrics$lpreds + 1
  m_index    <- rep(mindex, reps)
  beta       <- metrics$betas

  data.frame(model = m_index, predictor = beta_names, beta = beta)

}

#' @export
#' @rdname ols_step_all_possible_betas
#' @usage NULL
#'
ols_all_subset_betas <- function(model, ...) {
  .Deprecated("ols_step_all_possible_betas()")
}


#' All possible regression internal
#'
#' Internal function for all possible regression.
#'
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
allpos_helper <- function(model) {

  nam   <- coeff_names(model)
  n     <- length(nam)
  r     <- seq_len(n)
  combs <- list()

  for (i in seq_len(n)) {
    combs[[i]] <- combn(n, r[i])
  }

  predicts  <- nam
  lc        <- length(combs)
  varnames  <- model_colnames(model)
  len_preds <- length(predicts)
  gap       <- len_preds - 1
  data      <- mod_sel_data(model)
  space     <- coeff_length(predicts, gap)
  colas     <- unname(unlist(lapply(combs, ncol)))
  response  <- varnames[1]
  p         <- colas
  t         <- cumsum(colas)
  q         <- c(1, t[-lc] + 1)

  mcount    <- 0
  rsq       <- list()
  adjrsq    <- list()
  predrsq   <- list()
  cp        <- list()
  aic       <- list()
  sbic      <- list()
  sbc       <- list()
  msep      <- list()
  fpe       <- list()
  apc       <- list()
  hsp       <- list()
  preds     <- list()
  lpreds    <- c()
  betas     <- c()

  for (i in seq_len(lc)) {
    for (j in seq_len(colas[i])) {

      predictors <- nam[combs[[i]][, j]]
      lp         <- length(predictors)

      out <- ols_regress(paste(response, "~",
                               paste(predictors, collapse = " + ")),
                         data = data)

      mcount            <- mcount + 1
      lpreds[mcount]    <- lp
      rsq[[mcount]]     <- out$rsq
      adjrsq[[mcount]]  <- out$adjr
      predrsq[[mcount]] <- ols_pred_rsq(out$model)
      cp[[mcount]]      <- ols_mallows_cp(out$model, model)
      aic[[mcount]]     <- ols_aic(out$model)
      sbic[[mcount]]    <- ols_sbic(out$model, model)
      sbc[[mcount]]     <- ols_sbc(out$model)
      msep[[mcount]]    <- ols_msep(out$model)
      fpe[[mcount]]     <- ols_fpe(out$model)
      apc[[mcount]]     <- ols_apc(out$model)
      hsp[[mcount]]     <- ols_hsp(out$model)
      preds[[mcount]]   <- paste(predictors, collapse = " ")
      betas             <- append(betas, out$betas)
    }
  }

  result <- list(
    lpreds = lpreds, rsq = rsq, adjrsq = adjrsq,
    predrsq = predrsq, cp = cp, aic = aic, sbic = sbic,
    sbc = sbc, msep = msep, fpe = fpe, apc = apc, hsp = hsp,
    preds = preds, lc = lc, q = q, t = t, betas = betas
  )

  return(result)
}

#' Coefficient names
#'
#' Returns the names of the coefficients including interaction variables.
#'
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
coeff_names <- function(model) {
  terms <- NULL
  colnames(attr(model$terms, which = "factor"))
}

#' Model data columns
#'
#' Returns the names of the columns in the data used in the model.
#'
#' @param model An object of class \cdoe{lm}.
#'
#' @noRd
#'
model_colnames <- function(model) {
  names(model.frame(model))
}

#' Coefficients length
#'
#' Returns the length of the coefficient names.
#'
#' @param predicts Name of the predictors in the model.
#' @param gap A numeric vector.
#'
#' @noRd
#'
coeff_length <- function(predicts, gap) {
  sum(nchar(predicts)) + gap
}
