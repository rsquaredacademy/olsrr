#' Stepwise forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on p values, in a stepwise manner until there is
#' no variable left to enter any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param p_val p value; variables with p value less than \code{p_val} will
#'   enter into the model
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param hierarchical Logical; if \code{TRUE}, performs hierarchical selection.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_forward_p}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#'
#' @return \code{ols_step_forward_p} returns an object of class \code{"ols_step_forward_p"}.
#' An object of class \code{"ols_step_forward_p"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' Kutner, MH, Nachtscheim CJ, Neter J and Li W., 2004, Applied Linear Statistical Models (5th edition).
#' Chicago, IL., McGraw Hill/Irwin.
#'
#' @examples
#' # stepwise forward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_forward_p(model)
#'
#' # stepwise forward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_forward_p(model)
#' plot(k)
#'
#' # selection metrics
#' k$metrics
#'
#' # final model
#' k$model
#'
#' # include or exclude variables
#' # force variable to be included in selection process
#' ols_step_forward_p(model, include = c("age", "alc_mod"))
#'
#' # use index of variable instead of name
#' ols_step_forward_p(model, include = c(5, 7))
#'
#' # force variable to be excluded from selection process
#' ols_step_forward_p(model, exclude = c("pindex"))
#'
#' # use index of variable instead of name
#' ols_step_forward_p(model, exclude = c(2))
#'
#' # hierarchical selection
#' model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test, data = surgical)
#' ols_step_forward_p(model, 0.1, hierarchical = TRUE)
#'
#' # plot
#' k <- ols_step_forward_p(model, 0.1, hierarchical = TRUE)
#' plot(k)
#'
#' # selection metrics
#' k$metrics
#'
#' # final model
#' k$model
#'
#' @importFrom stats qt
#' @importFrom car Anova
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_forward_p <- function(model, ...) UseMethod("ols_step_forward_p")

#' @export
#' @rdname ols_step_forward_p
#'
ols_step_forward_p.default <- function(model, p_val = 0.3, include = NULL, exclude = NULL, hierarchical = FALSE, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)
  check_values(p_val, 0, 1)

  indterms <- coeff_names(model)
  lenterms <- length(indterms)

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  if (hierarchical) {
    ols_step_hierarchical(model, p_val, TRUE, progress, details)
  } else {
    l        <- model$model
    nam      <- colnames(attr(model$terms, "factors"))
    lockterm <- c(include, exclude)
    cterms   <- setdiff(nam, exclude)

    if (progress || details) {
      ols_candidate_terms(cterms, "forward")
    }

    nam      <- setdiff(nam, lockterm)
    n        <- ncol(l)
    response <- names(model$model)[1]
    all_pred <- nam
    mlen_p   <- length(all_pred)

    step     <- 0
    ppos     <- step + 1 + length(include)
    preds    <- include
    pvals    <- c()
    fvals    <- c()
    aicvals  <- c()
    rsqvals  <- c()
    arsqvals <- c()
    rsq      <- c()
    adjrsq   <- c()
    aic      <- c()
    sbic     <- c()
    cp       <- c()
    sbc      <- c()
    rmse     <- c()

    base_model <- ols_base_model(include, response, l)
    rsq_base   <- summary(base_model)$r.squared
    
    if (details) {
      ols_rsquared_init(include, "r2", response, rsq_base)
    }

    for (i in seq_len(mlen_p)) {
      predictors  <- c(include, all_pred[i])
      m           <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
      m_sum       <- Anova(m)
      fvals[i]    <- m_sum$`F value`[ppos]
      pvals[i]    <- m_sum$`Pr(>F)`[ppos]
      aicvals[i]  <- ols_aic(m)
      rsqvals[i]  <- summary(m)$r.squared
      arsqvals[i] <- summary(m)$adj.r.squared
    }

    if (details) {
      d <- data.frame(predictor = all_pred, p_val = pvals, rsq = rsqvals,
                       arsq = arsqvals, aic = aicvals)
      d <- d[order(d$p_val), ]
      ols_stepwise_table(d, all_pred, pvals, rsqvals, arsqvals, aicvals)
    }

    maxf     <- which(fvals == max(fvals, na.rm = TRUE))
    minp     <- which(pvals == min(pvals, na.rm = TRUE))
    len_minp <- length(minp)

    if (len_minp > 1) {
      minp <- minp[1]
    }

    if (pvals[minp] > p_val) {
      stop("None of the variables satisfy the criteria for entering the model.", call. = FALSE)
    } else {
      if (progress) {
        ols_progress_init("forward")
      }
    }

    preds  <- c(preds, all_pred[maxf])
    lpreds <- length(preds)

    if (lpreds > 1) {

      for (i in seq_len(lpreds)) {

        step   <- step + 1
        npreds <- preds[1:i]
        fr     <- ols_regress(paste(response, "~", paste(npreds, collapse = " + ")), l)
        rsq    <- c(rsq, fr$rsq)
        adjrsq <- c(adjrsq, fr$adjr)
        aic    <- c(aic, ols_aic(fr$model))
        sbc    <- c(sbc, ols_sbc(fr$model))
        sbic   <- c(sbic, ols_sbic(fr$model, model))
        cp     <- c(cp, ols_mallows_cp(fr$model, model))
        rmse   <- c(rmse, fr$rmse)
        rsq1   <- tail(rsq, n = 1)

        if (progress) {
          ols_progress_display(npreds, "others")
        }

        if (details) {
          ols_rsquared_selected("r2", step, npreds, response, rsq1)
        }

      }

    } else {

      step   <- step + 1
      fr     <- ols_regress(paste(response, "~", preds), l)
      rsq    <- c(rsq, fr$rsq)
      adjrsq <- c(adjrsq, fr$adjr)
      aic    <- c(aic, ols_aic(fr$model))
      sbc    <- c(sbc, ols_sbc(fr$model))
      sbic   <- c(sbic, ols_sbic(fr$model, model))
      cp     <- c(cp, ols_mallows_cp(fr$model, model))
      rmse   <- c(rmse, fr$rmse)
      rsq1   <- tail(rsq, n = 1)

      if (progress) {
        ols_progress_display(preds, "others")
      }

      if (details) {
        ols_rsquared_selected("r2", step, preds, response, rsq1)
      }

    }

    while (step < mlen_p) {

      all_pred <- all_pred[-maxf]
      len_p    <- length(all_pred)

      if (len_p == 0) {
        break
      }

      ppos     <- ppos + length(maxf)
      pvals    <- c()
      fvals    <- c()
      aicvals  <- c()
      rsqvals  <- c()
      arsqvals <- c()

      for (i in seq_len(len_p)) {

        predictors  <- c(preds, all_pred[i])
        m           <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
        m_sum       <- Anova(m)
        fvals[i]    <- m_sum$`F value`[ppos]
        pvals[i]    <- m_sum$`Pr(>F)`[ppos]
        aicvals[i]  <- ols_aic(m)
        rsqvals[i]  <- summary(m)$r.squared
        arsqvals[i] <- summary(m)$adj.r.squared
      }

      if (details) {
        d <- data.frame(predictor = all_pred, p_val = pvals, rsq = rsqvals,
                         arsq = arsqvals, aic = aicvals)
        d <- d[order(d$p_val), ]
        ols_stepwise_table(d, all_pred, pvals, rsqvals, arsqvals, aicvals)
      }

      maxf  <- which(fvals == max(fvals, na.rm = TRUE))
      minp  <- pvals[maxf]

      if (minp <= p_val) {

        step   <- step + 1
        preds  <- c(preds, all_pred[maxf])
        lpreds <- length(preds)
        fr     <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        rsq    <- c(rsq, fr$rsq)
        adjrsq <- c(adjrsq, fr$adjr)
        aic    <- c(aic, ols_aic(fr$model))
        sbc    <- c(sbc, ols_sbc(fr$model))
        sbic   <- c(sbic, ols_sbic(fr$model, model))
        cp     <- c(cp, ols_mallows_cp(fr$model, model))
        rmse   <- c(rmse, fr$rmse)
        rsq1   <- tail(rsq, n = 1)

        if (progress) {
          ols_progress_display(preds, "others")
        }

        if (details) {
          ols_rsquared_selected("r2", step, preds, response, rsq1)
        }
      } else {
        if (progress || details) {
          ols_stepwise_break(direction = "forward")
        }
        break
      }
    }

    prsq <- c(rsq[1], diff(rsq))

    if (details) {
      ols_stepwise_vars(preds, "forward")
    }

    final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

    metrics     <- data.frame(step       = seq_len(step),
                              variable   = preds,
                              r2         = rsq,
                              adj_r2     = adjrsq,
                              aic        = aic,
                              sbic       = sbic,
                              sbc        = sbc,
                              mallows_cp = cp,
                              rmse       = rmse)

    out <- list(metrics = metrics,
                model   = final_model,
                others  = list(base_model = base_model))

    class(out) <- "ols_step_forward_p"

    return(out)
  }

}

#' @export
#'
print.ols_step_forward_p <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "forward")
  } else {
    print("No variables have been added to the model.")
  }
}

#' @export
#' @rdname ols_step_forward_p
#'
plot.ols_step_forward_p <- function(x, model = NA, print_plot = TRUE, details = TRUE, ...) {

  a <- NULL

  p1 <- ols_plot_stepwise(x, "r2", "R-Square", details, "forward")
  p2 <- ols_plot_stepwise(x, "adj_r2", "Adjusted R-Square", details, "forward")
  p3 <- ols_plot_stepwise(x, "aic", "AIC", details, "forward")
  p4 <- ols_plot_stepwise(x, "rmse", "RMSE", details, "forward")

  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3, plot_4 = p4)

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}

