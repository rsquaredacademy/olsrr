#' Stepwise backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on p values, in a stepwise manner until there is
#' no variable left to remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param p_val p value; variables with p more than \code{p_val} will be removed
#'   from the model.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param hierarchical Logical; if \code{TRUE}, performs hierarchical selection.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#'   each step.
#' @param x An object of class \code{ols_step_backward_p}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other inputs.
#'
#' @return \code{ols_step_backward_p} returns an object of class \code{"ols_step_backward_p"}.
#' An object of class \code{"ols_step_backward_p"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @examples
#' # stepwise backward regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_backward_p(model)
#'
#' # stepwise backward regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_backward_p(model)
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
#' ols_step_backward_p(model, include = c("age", "alc_mod"))
#'
#' # use index of variable instead of name
#' ols_step_backward_p(model, include = c(5, 7))
#'
#' # force variable to be excluded from selection process
#' ols_step_backward_p(model, exclude = c("pindex"))
#'
#' # use index of variable instead of name
#' ols_step_backward_p(model, exclude = c(2))
#'
#' # hierarchical selection
#' model <- lm(y ~ bcs + alc_heavy + pindex + age + alc_mod, data = surgical)
#' ols_step_backward_p(model, 0.1, hierarchical = TRUE)
#'
#' # plot
#' k <- ols_step_backward_p(model, 0.1, hierarchical = TRUE)
#' plot(k)
#'
#' @family backward selection procedures
#'
#' @export
#'
ols_step_backward_p <- function(model, ...) UseMethod("ols_step_backward_p")

#' @export
#' @rdname ols_step_backward_p
#'
ols_step_backward_p.default <- function(model, p_val = 0.3, include = NULL, exclude = NULL, hierarchical = FALSE, progress = FALSE, details = FALSE, ...) {

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
    ols_step_hierarchical(model, p_val, FALSE, progress, details)
  } else {
    l        <- model$model
    nam      <- colnames(attr(model$terms, "factors"))
    lockterm <- c(include, exclude)
    response <- names(model$model)[1]
    preds    <- setdiff(nam, lockterm)
    cterms   <- c(include, preds)

    if (progress || details) {
      ols_candidate_terms(cterms, "backward")
    }

    end   <- FALSE
    step  <- 0
    rpred <- c()
    rsq   <- c()
    arsq  <- c()
    aic   <- c()
    ess   <- c()
    rss   <- c()
    sbic  <- c()
    sbc   <- c()
    cp    <- c()
    rmse  <- c()
    ppos  <- step + 1 + length(include)

    rsq_base   <- summary(model)$r.squared
    
    if (details) {
      ols_rsquared_init(indterms, "r2", response, rsq_base)
    }

    if (progress) {
      ols_progress_init("backward")
    }

    while (!end) {
      m     <- lm(paste(response, "~", paste(c(include, cterms), collapse = " + ")), l)
      m_sum <- Anova(m)
      nm    <- length(m_sum$`F value`)
      fvals <- m_sum$`F value`[ppos:nm]
      pvals <- m_sum$`Pr(>F)`
      min_f <- which(fvals == min(fvals, na.rm = TRUE))

      len_minf <- length(min_f)

      if (len_minf > 1) {
        min_f <- min_f[1]
      }

      minf <- min_f + ppos - 1


        if (pvals[minf] > p_val) {

          step   <- step + 1
          rpred  <- c(rpred, cterms[minf])
          cterms <- cterms[-minf]
          lp     <- length(rpred)
          fr     <- ols_regress(paste(response, "~", paste(c(include, cterms), collapse = " + ")), l)
          rsq    <- c(rsq, fr$rsq)
          arsq   <- c(arsq, fr$adjr)
          rss    <- c(rss, fr$rss)
          ess    <- c(ess, fr$ess)
          aic    <- c(aic, ols_aic(fr$model))
          sbc    <- c(sbc, ols_sbc(fr$model))
          sbic   <- c(sbic, ols_sbic(fr$model, model))
          cp     <- c(cp, ols_mallows_cp(fr$model, model))
          rmse   <- c(rmse, fr$rmse)

          if (progress) {
            ols_progress_display(rpred, "others")
          }

          if (details) {
            mypred <- c(include, cterms)
            rsq1   <- tail(rsq, n = 1)
            ols_stepwise_details(step, rpred, mypred, response, rsq1, "removed", "rsq")
          }
        } else {
          end <- TRUE
          if (progress || details) {
            ols_stepwise_break(direction = "backward")
          }
        }

    }

    if (details) {
      ols_stepwise_vars(rpred, "backward")
    }

    final_model <- lm(paste(response, "~", paste(c(include, cterms), collapse = " + ")), data = l)

    metrics     <- data.frame(step       = seq_len(step),
                              variable   = rpred,
                              r2         = rsq,
                              adj_r2     = arsq,
                              aic        = aic,
                              sbc        = sbc,
                              sbic       = sbic,
                              mallows_cp = cp,
                              rmse       = rmse)

    out <- list(metrics = metrics,
                model   = final_model,
                others  = list(full_model = model))

    class(out) <- "ols_step_backward_p"

    return(out)
  }

}

#' @export
#'
print.ols_step_backward_p <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "backward")
  } else {
    print("No variables have been removed from the model.")
  }
}



#' @export
#' @rdname ols_step_backward_p
#'
plot.ols_step_backward_p <- function(x, model = NA, print_plot = TRUE, details = TRUE, ...) {

  p1 <- ols_plot_stepwise(x, "r2", "R-Square", details, "backward")
  p2 <- ols_plot_stepwise(x, "adj_r2", "Adjusted R-Square", details, "backward")
  p3 <- ols_plot_stepwise(x, "aic", "Adjusted R-Square", details, "backward")
  p4 <- ols_plot_stepwise(x, "rmse", "RMSE", details, "backward")

  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3, plot_4 = p4)

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2, top = "Stepwise Backward Regression")
  } else {
    return(myplots)
  }

}
