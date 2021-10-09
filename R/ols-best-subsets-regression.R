#' Best subsets regression
#'
#' Select the subset of predictors that do the best at meeting some
#' well-defined objective criterion, such as having the largest R2 value or the
#' smallest MSE, Mallow's Cp or AIC. The default metric used for selecting the
#' model is R2 but the user can choose any of the other available metrics.
#'
#' @param model An object of class \code{lm}.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param metric Metric to select model.
#' @param x An object of class \code{ols_step_best_subset}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other inputs.
#'
#' @return \code{ols_step_best_subset} returns an object of class \code{"ols_step_best_subset"}.
#' An object of class \code{"ols_step_best_subset"} is a list containing the following:
#'
#' \item{metrics}{selection metrics}
#'
#' @references
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
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
#' # return only models including `qsec`
#' ols_step_best_subset(model, include = c("qsec"))
#'
#' # exclude `hp` from selection process
#' ols_step_best_subset(model, exclude = c("hp"))
#'
#' @export
#'
ols_step_best_subset <- function(model, ...) UseMethod("ols_step_best_subset")

#' @export
#' @rdname ols_step_best_subset
#' 
ols_step_best_subset.default <- function(model, include = NULL, exclude = NULL,
                                         metric = c("rsquare", "adjr", "predrsq",
                                                    "cp", "aic", "sbic", "sbc",
                                                    "msep", "fpe", "apc", "hsp"),
                                         ...) {

  check_model(model)
  check_npredictors(model, 2)

  indterms <- coeff_names(model)
  lenterms <- length(indterms)

  if (is.character(include)) {
    npm <- include %in% indterms
    if (!all(npm)) {
      stop(paste(paste(include[!npm], collapse = ", "), "not part of the model and hence cannot be forcibly included. Please verify the variable names."), call. = FALSE)
    }
  }

  if (is.character(exclude)) {
    npm <- exclude %in% indterms
    if (!all(npm)) {
      stop(paste(paste(exclude[!npm], collapse = ", "), "not part of the model and hence cannot be forcibly excluded. Please verify the variable names."), call. = FALSE)
    }
  }
  
  if (is.numeric(include)) {
    if (any(include > lenterms)) {
      stop(paste0("Index of variable to be included should be between 1 and ", lenterms, "."), call. = FALSE)
    } else {
      include <- indterms[include]
    }
  }

  if (is.numeric(exclude)) {
    if (any(exclude > lenterms)) {
      stop(paste0("Index of variable to be excluded should be between 1 and ", lenterms, "."), call. = FALSE)  
    } else {
      exclude <- indterms[exclude]
    }
  }

  nam   <- setdiff(coeff_names(model), exclude)
  n     <- length(nam)
  r     <- seq_len(n)
  combs <- list()

  for (i in seq_len(n)) {
    combs[[i]] <- combn(n, r[i])
  }

  lc        <- length(combs)
  varnames  <- model_colnames(model)
  data      <- model$model
  colas     <- unname(unlist(lapply(combs, ncol)))
  response  <- varnames[1]
  predicts  <- list()
  k         <- 1

  for (i in seq_len(lc)) {
    for (j in seq_len(colas[i])) {
      predicts[[k]] <- nam[combs[[i]][, j]]
      k <- k + 1
    }
  }

  if(!is.null(include)) {
    y <- grep(include, predicts)
    predicts <- predicts[y]
  }

  len_elig <- length(predicts)

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

  for (i in seq_len(len_elig)) {
      predictors        <- predicts[[i]]
      lp                <- length(predictors)
      out               <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = data)
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

  l <- split(ui, ui$n)

  if (metrics == "rsquare" || metrics == "adjr" || metrics == "predrsq") {
    temp <- lapply(l, function(x) x[order(x[[metrics]], decreasing = TRUE), ][1, ])
  } else {
    temp <- lapply(l, function(x) x[order(x[[metrics]]), ][1, ])
  }
  
  sorted <- do.call(rbind, temp)
  mindex <- seq_len(nrow(sorted))
  sorted <- cbind(mindex, sorted)

  result <- list(metrics = sorted)

  class(result) <- c("ols_step_best_subset")
  return(result)

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


  d <- data.frame(mindex = x$metrics$mindex, rsquare = x$metrics$rsquare, adjr = x$metrics$adjr,
               cp = x$metrics$cp, aic = x$metrics$aic, sbic = x$metrics$sbic, sbc = x$metrics$sbc)

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
