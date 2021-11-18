#' Stepwise AIC regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering and removing predictors based on akaike information criteria, in a
#' stepwise manner until there is no variable left to enter or remove any more.
#'
#' @param model An object of class \code{lm}.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, details of variable selection will
#'   be printed on screen.
#' @param x An object of class \code{ols_step_both_aic}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#'
#' @return \code{ols_step_both_aic} returns an object of class \code{"ols_step_both_aic"}.
#' An object of class \code{"ols_step_both_aic"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#'
#' @references
#' Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
#'
#' @examples
#' \dontrun{
#' # stepwise regression
#' model <- lm(y ~ ., data = stepdata)
#' ols_step_both_aic(model)
#'
#' # stepwise regression plot
#' model <- lm(y ~ ., data = stepdata)
#' k <- ols_step_both_aic(model)
#' plot(k)
#'
#' # final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' model <- lm(y ~ ., data = stepdata)
#'
#' ols_step_both_aic(model, include = c("x6"))
#'
#' # use index of variable instead of name
#' ols_step_both_aic(model, include = c(6))
#'
#' # force variable to be excluded from selection process
#' ols_step_both_aic(model, exclude = c("x2"))
#'
#' # use index of variable instead of name
#' ols_step_both_aic(model, exclude = c(2))
#'
#' # include & exclude variables in the selection process
#' ols_step_both_aic(model, include = c("x6"), exclude = c("x2"))
#'
#' # use index of variable instead of name
#' ols_step_both_aic(model, include = c(6), exclude = c(2))
#' }
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_both_aic <- function(model, ...) UseMethod("ols_step_both_aic")

#' @export
#' @rdname ols_step_both_aic
#'
ols_step_both_aic.default <- function(model, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)

  response <- names(model$model)[1]
  l        <- model$model
  nam      <- coeff_names(model)
  indterms <- nam
  lenterms <- length(indterms)
  len_inc  <- length(include) + 1

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  if (progress || details) {
    ols_candidate_terms(nam, "both")
  }

  lockterm   <- c(include, exclude)
  predictors <- setdiff(nam, lockterm)
  mlen_p     <- length(predictors)
  tech       <- c("addition", "removal")

  base_model <- ols_base_model(include, response, l)
  aic_c <- ols_aic(base_model)

  if (details) {
    ols_base_model_stats(response, include, "both", aic_c)
  }

  step      <- 0
  all_step  <- 0
  preds     <- include
  var_index <- c()
  method    <- c()
  laic      <- c()
  less      <- c()
  lrss      <- c()
  lrsq      <- c()
  larsq     <- c()

  if (progress) {
    ols_progress_init("both")
  }

  while (step < mlen_p) {

    aics <- c()
    ess  <- c()
    rss  <- c()
    rsq  <- c()
    arsq <- c()
    lpds <- length(predictors)

    for (i in seq_len(lpds)) {

      predn <- c(preds, predictors[i])

      m <- ols_regress(paste(response, "~", paste(predn, collapse = " + ")), data = l)

      aics[i] <- ols_aic(m$model)
      ess[i]  <- m$ess
      rss[i]  <- m$rss
      rsq[i]  <- m$rsq
      arsq[i] <- m$adjr
    }

    da <- data.frame(predictors = predictors, aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)

    if (details) {
      ols_stepwise_metrics(da, "aic", predictors, aics, rss, ess, rsq, arsq)
    }

    minc <- which(aics == min(aics))

    if (aics[minc] < aic_c) {
      aic_c      <- aics[minc]
      preds      <- c(preds, predictors[minc])
      predictors <- predictors[-minc]
      lpds       <- length(predictors)
      method     <- c(method, tech[1])
      lpreds     <- length(preds)
      var_index  <- c(var_index, preds[lpreds])
      step       <- step + 1
      all_step   <- all_step + 1
      maic       <- aics[minc]
      mess       <- ess[minc]
      mrss       <- rss[minc]
      mrsq       <- rsq[minc]
      marsq      <- arsq[minc]
      laic       <- c(laic, maic)
      less       <- c(less, mess)
      lrss       <- c(lrss, mrss)
      lrsq       <- c(lrsq, mrsq)
      larsq      <- c(larsq, marsq)

      if (progress) {
        ols_progress_display(preds, "both", "added")
      }

      if (details) {
        ols_stepwise_details(all_step, preds, preds, response, maic, "added")
      }

      if (lpreds > 1) {

        aics <- c()
        ess  <- c()
        rss  <- c()
        rsq  <- c()
        arsq <- c()
        j    <- 1

        for (i in len_inc:lpreds) {

          preda <- preds[-i]

          m <- ols_regress(paste(response, "~", paste(preda, collapse = " + ")), data = l)

          aics[j] <- ols_aic(m$model)
          ess[j]  <- m$ess
          rss[j]  <- m$rss
          rsq[j]  <- m$rsq
          arsq[j] <- m$adjr

          j <- j + 1
        }

        da <- data.frame(predictors = preds[len_inc: lpreds], aics = aics, ess = ess, rss = rss, rsq = rsq, arsq = arsq)

        if (details) {
          ols_stepwise_metrics(da, "aic", predictors, aics, rss, ess, rsq, arsq)
        }

        minc2 <- which(aics == min(aics))

        if (aics[minc2] < laic[all_step]) {
          aic_c     <- aics[minc2]
          maic      <- aics[minc2]
          mess      <- ess[minc2]
          mrss      <- rss[minc2]
          mrsq      <- rsq[minc2]
          marsq     <- arsq[minc2]
          laic      <- c(laic, maic)
          less      <- c(less, mess)
          lrss      <- c(lrss, mrss)
          lrsq      <- c(lrsq, mrsq)
          larsq     <- c(larsq, marsq)
          var_index <- c(var_index, preds[minc2 + length(include)])
          method    <- c(method, tech[2])
          all_step  <- all_step + 1

          temp <- preds[minc2 + length(include)]

          if (progress) {
            ols_progress_display(temp, "both", "removed")
          }

          preds <- preds[-(minc2 + length(include))]
          lpreds <- length(preds)

          if (details) {
            ols_stepwise_details(all_step, temp, preds, response, maic, "removed")
          }
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      if (progress || details) {
        ols_stepwise_break("both")
      }
      break
    }
  }

  if (details) {
    ols_stepwise_vars(preds, "both")
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  metrics     <- data.frame(step     = seq_len(all_step),
                            variable = var_index,
                            method   = method,
                            r2       = lrsq,
                            adj_r2   = larsq,
                            aic      = laic,
                            rss      = lrss,
                            ess      = less)

  out <- list(metrics = metrics,
              model   = final_model,
              others   = list(base_model = base_model))

  class(out) <- "ols_step_both_aic"

  return(out)
}

#' @export
#'
print.ols_step_both_aic <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "both")
  } else {
    print("No variables have been added to or removed from the model.")
  }
}

#' @rdname ols_step_both_aic
#' @export
#'
plot.ols_step_both_aic <- function(x, print_plot = TRUE, details = TRUE, ...) {

  p <- ols_stepaic_plot(x, "both", details)

  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}
