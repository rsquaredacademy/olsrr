#' Stepwise AIC forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on akaike information criterion, in a stepwise
#' manner until there is no variable left to enter any more.
#'
#' @param model An object of class \code{lm}.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_forward_aic}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#' @return \code{ols_step_forward_aic} returns an object of class \code{"ols_step_forward_aic"}.
#' An object of class \code{"ols_step_forward_aic"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#' \item{others}{list; info used for plotting and printing}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_aic(model)
#'
#' # stepwise forward regression plot
#' k <- ols_step_forward_aic(model)
#' plot(k)
#'
#' # extract final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' ols_step_forward_aic(model, include = c("age"))
#'
#' # use index of variable instead of name
#' ols_step_forward_aic(model, include = c(5))
#'
#' # force variable to be excluded from selection process
#' ols_step_forward_aic(model, exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_aic(model, exclude = c(4))
#'
#' # include & exclude variables in the selection process
#' ols_step_forward_aic(model, include = c("age"), exclude = c("liver_test"))
#'
#' # use index of variable instead of name
#' ols_step_forward_aic(model, include = c(5), exclude = c(4))
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_forward_aic <- function(model, ...) UseMethod("ols_step_forward_aic")

#' @export
#' @rdname ols_step_forward_aic
#'
ols_step_forward_aic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

  response <- names(model$model)[1]
  l        <- model$model
  indterms <- coeff_names(model)
  lenterms <- length(indterms)
  
  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  lockterm <- c(include, exclude)
  nam      <- setdiff(indterms, lockterm)
  all_pred <- nam
  mlen_p   <- length(all_pred)
  preds    <- include
  aics     <- c()
  ess      <- c()
  rss      <- c()
  rsq      <- c()
  arsq     <- c()

  base_model <- ols_base_model(include, response, l)
  aic1 <- ols_aic(base_model)

  if (progress || details) {
    ols_candidate_terms(nam, "forward")
  }

  if (details) {
    ols_base_model_stats(response, include, "forward", aic1)
  }

  for (i in seq_len(mlen_p)) {

    predictors <- c(include, all_pred[i])
    k <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)

    aics[i] <- ols_aic(k$model)
    ess[i]  <- k$ess
    rss[i]  <- k$rss
    rsq[i]  <- k$rsq
    arsq[i] <- k$adjr
  }

  da <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
  da2 <- da[order(da$aics), ]

  if (details) {
    ols_stepwise_metrics(da2, "aic", predictors, aics, rss, ess, rsq, arsq)
  }

  minc     <- which(aics == min(aics))
  laic     <- aics[minc]
  less     <- ess[minc]
  lrss     <- rss[minc]
  lrsq     <- rsq[minc]
  larsq    <- arsq[minc]
  preds    <- c(preds, all_pred[minc])
  lpreds   <- length(preds) - length(include)
  all_pred <- all_pred[-minc]
  len_p    <- length(all_pred)
  step     <- 1

  if (progress) {
    ols_progress_init("forward")
    ols_progress_display(preds, "others")
  }

  while (step < mlen_p) {

    aics <- c()
    ess  <- c()
    rss  <- c()
    rsst <- c()
    rsq  <- c()
    arsq <- c()
    mo   <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
    aic1 <- ols_aic(mo$model)

    if (details) {
      ols_stepwise_details(step, preds, preds, response, aic1, "added")
    }

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      k          <- ols_regress(paste(response, "~", paste(predictors, collapse = " + ")), data = l)
      aics[i]    <- ols_aic(k$model)
      ess[i]     <- k$ess
      rsst[i]    <- k$rss
      rss[i]     <- round(k$rss - mo$rss, 3)
      rsq[i]     <- k$rsq
      arsq[i]    <- k$adjr
    }

    if (details) {
      da  <- data.frame(predictors = all_pred, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)
      da2 <- da[order(da$aics), ]
      ols_stepwise_metrics(da2, "aic", predictors, aics, rss, ess, rsq, arsq)
    }

    minaic <- which(aics == min(aics))

    if (aics[minaic] < laic[lpreds]) {

      preds    <- c(preds, all_pred[minaic])
      minc     <- aics[minaic]
      mess     <- ess[minaic]
      mrss     <- round(rsst[minaic], 3)
      mrsq     <- rsq[minaic]
      marsq    <- arsq[minaic]
      laic     <- c(laic, minc)
      less     <- c(less, mess)
      lrss     <- c(lrss, mrss)
      lrsq     <- c(lrsq, mrsq)
      larsq    <- c(larsq, marsq)
      lpreds   <- length(preds) - length(include)
      all_pred <- all_pred[-minaic]
      len_p    <- length(all_pred)
      step     <- step + 1

      if (progress) {
        ols_progress_display(preds, "others")
      }
    } else {
      if (progress || details) {
        cat("\n")
        cat("No more variables to be added.", "\n")
        cat("\n")
      }
      break
    }
  }

  if (details) {
    cat("\n")
    cat("Variables Selected:", "\n\n")
    for (i in seq_len(length(preds))) {
      cat(paste("=>", preds[i]), "\n")
    }
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  if (is.null(include)) {
    var_selected <- preds
  } else {
    var_selected <- preds[-seq_len(length(include))]
  }

  metrics     <- data.frame(step     = seq_len(step),
                            variable = var_selected,
                            r2       = lrsq,
                            adj_r2   = larsq,
                            aic      = laic,
                            rss      = lrss,
                            ess      = less)

  out <- list(metrics = metrics,
              model   = final_model,
              others  = list(base_model = base_model))


  class(out) <- "ols_step_forward_aic"

  return(out)
}

#' @export
#'
print.ols_step_forward_aic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_stepaic_forward(x)
  } else {
    print("No variables have been added to the model.")
  }
}

#' @rdname ols_step_forward_aic
#' @export
#'
plot.ols_step_forward_aic <- function(x, print_plot = TRUE, details = TRUE, ...) {

  tx  <- NULL
  a   <- NULL
  b   <- NULL

  step <- x$metrics$step
  aic  <- x$metrics$aic

  if (details) {
    x$metrics$text <- paste0("[", x$metrics$variable, ", ", round(x$metrics$aic, 2), "]")
    pred <- x$metrics$text
  } else {
    pred <- x$metrics$variable
  }
  
  y    <- step
  xloc <- y
  yloc <- aic
  xmin <- min(y) - 0.4
  xmax <- max(y) + 1
  ymin <- min(aic) - (min(aic) * 0.05)
  ymax <- max(aic) + (max(aic) * 0.05)

  d2 <- data.frame(x = xloc, y = yloc, tx = pred)
  d  <- data.frame(a = y, b = aic)

  # metric info
  base_model_aic  <- round(ols_aic(x$others$base_model), 3)
  final_model_aic <- round(ols_aic(x$model), 3)
  metric_info <- paste0("Base Model AIC  : ", format(base_model_aic, nsmall = 3), "\n",
                        "Final Model AIC : ", format(final_model_aic, nsmall = 3))

  p <-
    ggplot(d, aes(x = a, y = b)) + 
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) + 
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + 
    xlab("Step") + 
    ylab("AIC") +
    ggtitle("Stepwise AIC Forward Selection") +
    geom_text(data = d2, aes(x = x, y = y, label = tx), size = 3,
              hjust = "left", vjust = "bottom", nudge_x = 0.1) +
    annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 2,
             family = "serif", fontface = "italic", size = 3,
             label = metric_info)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
