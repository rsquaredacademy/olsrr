#' Best subsets regression
#'
#' Select the subset of predictors that do the best at meeting some
#' well-defined objective criterion, such as having the largest R2 value or the
#' smallest MSE, Mallow's Cp or AIC. The default metric used for selecting the
#' model is R2 but the user can choose any of the other available metrics.
#'
#' @param model An object of class \code{lm}.
#' @param metric Metric to select model.
#' @param x An object of class \code{ols_step_best_subset}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other inputs.
#'
#' @return \code{ols_step_best_subset} returns an object of class \code{"ols_step_best_subset"}.
#' An object of class \code{"ols_step_best_subset"} is a data frame containing the
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
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#'
#' @section Deprecated Function:
#' \code{ols_best_subset()} has been deprecated. Instead use \code{ols_step_best_subset()}.
#'
#' @family variable selection procedures
#'
#' @examples
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' ols_step_best_subset(model)
#' ols_step_best_subset(model, metric = "adjr")
#' ols_step_best_subset(model, metric = "cp")
#'
#' # plot
#' model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
#' k <- ols_step_best_subset(model)
#' plot(k)
#'
#' @export
#'
ols_step_best_subset <- function(model, metric = c("rsquare", "adjr", "predrsq",
                                                   "cp", "aic", "sbic", "sbc",
                                                   "msep", "fpe", "apc", "hsp"),
                                 ...) UseMethod("ols_step_best_subset")

#' @export
#'
ols_step_best_subset.default <- function(model,
                                         metric = c("rsquare", "adjr", "predrsq",
                                                    "cp", "aic", "sbic", "sbc",
                                                    "msep", "fpe", "apc", "hsp"),
                                         ...) {

  check_model(model)
  check_npredictors(model, 3)

  nam   <- coeff_names(model)
  n     <- length(nam)
  r     <- seq_len(n)
  combs <- list()

  for (i in seq_len(n)) {
    combs[[i]] <- combn(n, r[i])
  }

  lc        <- length(combs)
  varnames  <- model_colnames(model)
  predicts  <- nam
  len_preds <- length(predicts)
  gap       <- len_preds - 1
  space     <- coeff_length(predicts, gap)
  data      <- model$model
  colas     <- unname(unlist(lapply(combs, ncol)))
  response  <- varnames[1]
  p         <- colas
  t         <- cumsum(colas)
  q         <- c(1, t[-lc] + 1)

  mcount    <- 0
  rsq       <- list()
  adjr      <- list()
  cp        <- list()
  aic       <- list()
  sbic      <- list()
  sbc       <- list()
  mse       <- list()
  gmsep     <- list()
  jp        <- list()
  pc        <- list()
  sp        <- list()
  press     <- list()
  predrsq   <- list()
  preds     <- list()
  lpreds    <- c()

  for (i in seq_len(lc)) {
    for (j in seq_len(colas[i])) {
      predictors        <- nam[combs[[i]][, j]]
      lp                <- length(predictors)
      out               <- ols_regress(paste(response, "~",
                                       paste(predictors, collapse = " + ")),
                                       data = data)
      mcount            <- mcount + 1
      lpreds[mcount]    <- lp
      rsq[[mcount]]     <- out$rsq
      adjr[[mcount]]    <- out$adjr
      cp[[mcount]]      <- ols_mallows_cp(out$model, model)
      aic[[mcount]]     <- ols_aic(out$model)
      sbic[[mcount]]    <- ols_sbic(out$model, model)
      sbc[[mcount]]     <- ols_sbc(out$model)
      gmsep[[mcount]]   <- ols_msep(out$model)
      jp[[mcount]]      <- ols_fpe(out$model)
      pc[[mcount]]      <- ols_apc(out$model)
      sp[[mcount]]      <- ols_hsp(out$model)
      predrsq[[mcount]] <- ols_pred_rsq(out$model)
      preds[[mcount]]   <- paste(predictors, collapse = " ")
    }
  }

  ui <- data.frame(
    n          = lpreds,
    predictors = unlist(preds),
    rsquare    = unlist(rsq),
    adjr       = unlist(adjr),
    predrsq    = unlist(predrsq),
    cp         = unlist(cp),
    aic        = unlist(aic),
    sbic       = unlist(sbic),
    sbc        = unlist(sbc),
    msep       = unlist(gmsep),
    fpe        = unlist(jp),
    apc        = unlist(pc),
    hsp        = unlist(sp),
    stringsAsFactors = F
  )

  metrics <- match.arg(metric)

  sorted <- c()

  for (i in seq_len(lc)) {
    temp   <- ui[q[i]:t[i], ]
    if (metrics == "rsquare" || metrics == "adjr" || metrics == "predrsq") {
      temp   <- temp[order(temp[[metrics]], decreasing = TRUE), ]
    } else {
      temp   <- temp[order(temp[[metrics]]), ]
    }
    sorted <- rbind(sorted, temp[1, ])
  }

  mindex <- seq_len(nrow(sorted))
  sorted <- cbind(mindex, sorted)

  class(sorted) <- c("ols_step_best_subset", "data.frame")
  return(sorted)

}

#' @export
#' @rdname ols_step_best_subset
#' @usage NULL
#'
ols_best_subset <- function(model, ...) {
  .Deprecated("ols_step_best_subset()")
}


#' @export
#'
print.ols_step_best_subset <- function(x, ...) {
  print_best_subset(x)
}

#' @export
#' @rdname ols_step_best_subset
#'
plot.ols_step_best_subset <- function(x, model = NA, print_plot = TRUE, ...) {

  rsquare <- NULL
  adjr    <- NULL
  sbic    <- NULL
  aic     <- NULL
  sbc     <- NULL
  cp      <- NULL
  a       <- NULL
  b       <- NULL


  d <- data.frame(mindex = x$mindex, rsquare = x$rsquare, adjr = x$adjr,
               cp = x$cp, aic = x$aic, sbic = x$sbic, sbc = x$sbc)

  p1 <- best_subset_plot(d, "rsquare")
  p2 <- best_subset_plot(d, "adjr", title = "Adj. R-Square")
  p3 <- best_subset_plot(d, "cp", title = "C(p)")
  p4 <- best_subset_plot(d, "aic", title = "AIC")
  p5 <- best_subset_plot(d, "sbic", title = "SBIC")
  p6 <- best_subset_plot(d, "sbc", title = "SBC")

  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3,
                  plot_4 = p4, plot_5 = p5, plot_6 = p6)

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}

#' Best subset plot
#'
#' Generate plots for best subset regression.
#'
#' @importFrom ggplot2 geom_line theme element_blank
#'
#' @param d A data.frame.
#' @param title Plot title.
#'
#' @noRd
#'
best_subset_plot <- function(d, var, title = "R-Square") {

  mindex <- NULL
  a      <- NULL
  b      <- NULL

  d1 <- d[, c("mindex", var)]
  colnames(d1) <- c("a", "b")

  ggplot(d1, aes(x = a, y = b)) +
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) +
    xlab("") + ylab("") + ggtitle(title) +
    theme(axis.ticks = element_blank())

}
