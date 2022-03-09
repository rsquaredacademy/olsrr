#' Stepwise regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering and removing predictors based on p values, in a stepwise manner
#' until there is no variable left to enter or remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param p_enter p value; variables with p value less than \code{p_enter} will enter
#'   into the model.
#' @param p_remove p value; variables with p more than \code{p_remove} will be removed
#'   from the model.
#' @param include Character or numeric vector; variables to be included in selection process.
#' @param exclude Character or numeric vector; variables to be excluded from selection process.
#' @param progress Logical; if \code{TRUE}, will display variable selection progress.
#' @param details Logical; if \code{TRUE}, will print the regression result at
#' each step.
#' @param x An object of class \code{ols_step_both_p}.
#' @param print_plot logical; if \code{TRUE}, prints the plot else returns a plot object.
#' @param ... Other arguments.
#' @return \code{ols_step_both_p} returns an object of class \code{"ols_step_both_p"}.
#' An object of class \code{"ols_step_both_p"} is a list containing the
#' following components:
#'
#' \item{model}{final model; an object of class \code{lm}}
#' \item{metrics}{selection metrics}
#' \item{beta_pval}{beta and p values of models in each selection step}
#'
#' @references
#' Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th ed. N.p.: John Wiley & Sons, 2012. Print.
#'
#' @examples
#' \dontrun{
#' # stepwise regression
#' model <- lm(y ~ ., data = surgical)
#' ols_step_both_p(model)
#'
#' # stepwise regression plot
#' model <- lm(y ~ ., data = surgical)
#' k <- ols_step_both_p(model)
#' plot(k)
#'
#' # selection metrics
#' k$metrics
#' 
#' # final model
#' k$model
#'
#' # include or exclude variables
#' model <- lm(y ~ ., data = stepdata)
#'
#' # force variable to be included in selection process
#' ols_step_both_p(model, include = c("x6"))
#'
#' # use index of variable instead of name
#' ols_step_both_p(model, include = c(6))
#'
#' # force variable to be excluded from selection process
#' ols_step_both_p(model, exclude = c("x1"))
#'
#' # use index of variable instead of name
#' ols_step_both_p(model, exclude = c(1))
#' }
#'
#' @family both direction selection_procedures
#'
#' @export
#'
ols_step_both_p <- function(model, ...) UseMethod("ols_step_both_p")

#' @export
#' @rdname ols_step_both_p
#'
ols_step_both_p.default <- function(model, p_enter = 0.1, p_remove = 0.3, include = NULL, exclude = NULL, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_inputs(model, include, exclude, progress, details)
  check_values(p_enter, 0, 1)
  check_values(p_remove, 0, 1)

  indterms <- coeff_names(model)
  lenterms <- length(indterms)

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
  }

  response <- names(model$model)[1]
  l        <- model$model
  lockterm <- c(include, exclude)
  nam      <- colnames(attr(model$terms, "factors"))
  cterms   <- setdiff(nam, exclude)

  if (progress || details) {
    ols_candidate_terms(cterms, "both")
  }

  nam      <- setdiff(nam, lockterm)
  all_pred <- nam
  mlen_p   <- length(all_pred)
  tech     <- c("addition", "removal")

  pvalues <- c()
  lbetas  <- c()
  betas   <- c()
  preds   <- include
  pvals   <- c()
  fvals   <- c()
  step    <- 0
  ppos    <- length(include) + 1
  rsq     <- c()
  adjrsq  <- c()
  aic     <- c()
  sbc     <- c()
  sbic    <- c()
  rmse    <- c()
  cp      <- c()
  f       <- c()
  fp      <- c()
  method  <- c()

  base_model <- ols_base_model(include, response, l)
  rsq_base   <- summary(base_model)$r.squared
    
  if (details) {
    ols_rsquared_init(include, "r2", response, rsq_base)
  }

  for (i in seq_len(mlen_p)) {
    predictors <- c(include, all_pred[i])
    m          <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
    m_sum      <- Anova(m)
    fvals[i]   <- m_sum$`F value`[ppos]
    pvals[i]   <- m_sum$`Pr(>F)`[ppos]
  }

  maxf     <- which(fvals == max(fvals, na.rm = TRUE))
  minp     <- which(pvals == min(pvals, na.rm = TRUE))
  len_minp <- length(minp)

  if (len_minp > 1) {
    minp <- minp[1]
  }

  if (pvals[minp] > p_enter) {
    stop("None of the variables satisfy the criteria for entering the model.", call. = FALSE)
  }

  preds   <- c(preds, all_pred[maxf])
  lpreds  <- length(preds)
  step    <- step + 1
  fr      <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
  rsq     <- c(rsq, fr$rsq)
  adjrsq  <- c(adjrsq, fr$adjr)
  cp      <- c(cp, ols_mallows_cp(fr$model, model))
  aic     <- c(aic, ols_aic(fr$model))
  sbc     <- c(sbc, ols_sbc(fr$model))
  sbic    <- c(sbic, ols_sbic(fr$model, model))
  rmse    <- c(rmse, fr$rmse)
  betas   <- append(betas, fr$betas)
  lbetas  <- append(lbetas, length(fr$betas))
  pvalues <- append(pvalues, fr$pvalues)
  method  <- c(method, tech[1])

  if (progress) {
    ols_progress_init("both")
    ols_progress_display(preds, "both", "added")
  }

  if (details) {
    rsq1 <- tail(rsq, n = 1)
    ols_rsquared_selected("r2", step, preds, response, rsq1)
  }

  all_step  <- step
  var_index <- tail(preds, n = 1)

  while (step < mlen_p) {

    all_pred <- all_pred[-maxf]
    len_p    <- length(all_pred)

    if (len_p == 0) {
      break
    }

    ppos     <- ppos + length(maxf)
    pvals    <- c()
    fvals    <- c()

    for (i in seq_len(len_p)) {

      predictors <- c(preds, all_pred[i])
      m          <- lm(paste(response, "~", paste(predictors, collapse = " + ")), l)
      m_sum      <- Anova(m)
      fvals[i]   <- m_sum$`F value`[ppos]
      pvals[i]   <- m_sum$`Pr(>F)`[ppos]
    }

    maxf  <- which(fvals == max(fvals, na.rm = TRUE))
    minp  <- pvals[maxf]

    if (minp <= p_enter) {

      preds     <- c(preds, all_pred[maxf])
      var_index <- c(var_index, all_pred[maxf])
      method    <- c(method, tech[1])
      lpreds    <- length(preds)
      all_step  <- all_step + 1
      step      <- step + 1
      fr        <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
      rsq       <- c(rsq, fr$rsq)
      adjrsq    <- c(adjrsq, fr$adjr)
      aic       <- c(aic, ols_aic(fr$model))
      sbc       <- c(sbc, ols_sbc(fr$model))
      sbic      <- c(sbic, ols_sbic(fr$model, model))
      cp        <- c(cp, ols_mallows_cp(fr$model, model))
      rmse      <- c(rmse, fr$rmse)
      betas     <- append(betas, fr$betas)
      lbetas    <- append(lbetas, length(fr$betas))
      pvalues   <- append(pvalues, fr$pvalues)

      if (progress) {
        ols_progress_display(preds, "both", "added")
      }

      if (details) {
        rsq1 <- tail(rsq, n = 1)
        ols_rsquared_selected("r2", all_step, preds, response, rsq1)
      }

      # check p value of predictors previously added in above step
      m2      <- lm(paste(response, "~", paste(preds, collapse = " + ")), l)
      m2_sum  <- Anova(m2)
      len_inc <- length(include) + 1
      fvals_r <- m2_sum$`F value`[len_inc:lpreds]
      pvals_r <- m2_sum$`Pr(>F)`[len_inc:lpreds]

      minf    <- which(fvals_r == min(fvals_r, na.rm = TRUE))
      maxp    <- pvals_r[minf]

      if (maxp > p_remove) {

        var_index <- c(var_index, preds[minf])
        lvar      <- length(var_index)
        method    <- c(method, tech[2])
        preds     <- preds[-(minf + length(include))]
        all_step  <- all_step + 1
        ppos      <- ppos - length(minf)
        fr        <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
        rsq       <- c(rsq, fr$rsq)
        adjrsq    <- c(adjrsq, fr$adjr)
        aic       <- c(aic, ols_aic(fr$model))
        sbc       <- c(sbc, ols_sbc(fr$model))
        sbic      <- c(sbic, ols_sbic(fr$model, model))
        cp        <- c(cp, ols_mallows_cp(fr$model, model))
        rmse      <- c(rmse, fr$rmse)
        betas     <- append(betas, fr$betas)
        lbetas    <- append(lbetas, length(fr$betas))
        pvalues   <- append(pvalues, fr$pvalues)

        if (progress) {
          ols_progress_display(var_index, "both", "removed")
        }

        if (details) {
          rsq1 <- tail(rsq, n = 1)
          ols_stepwise_details(all_step, var_index, preds, response, rsq1, "removed", "R-Squared")
        }
      } else {
        preds <- preds
        all_step <- all_step
      }
    } else {
      if (progress || details) {
        ols_stepwise_break(direction = "both")
      }
      break
    }
  }

  final_model <- lm(paste(response, "~", paste(preds, collapse = " + ")), data = l)

  metrics     <- data.frame(step       = seq_len(all_step),
                            variable   = var_index,
                            method     = method,
                            r2         = rsq,
                            adj_r2     = adjrsq,
                            aic        = aic,
                            sbic       = sbic,
                            sbc        = sbc,
                            mallows_cp = cp,
                            rmse       = rmse)

  beta_pval <- data.frame(
    model     = rep(seq_len(all_step), lbetas),
    predictor = names(betas),
    beta      = betas,
    pval      = pvalues
  )

  out <- list(metrics = metrics,
              model   = final_model,
              others  = list(base_model = base_model,
                             beta_pval = beta_pval))

  class(out) <- "ols_step_both_p"

  return(out)
}

#' @export
#'
print.ols_step_both_p <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_output(x, "both")
  } else {
    print("No variables have been added to or removed from the model.")
  }
}

#' @export
#' @rdname ols_step_both_p
#'
plot.ols_step_both_p <- function(x, model = NA, print_plot = TRUE, details = TRUE, ...) {

  p1 <- ols_plot_stepwise(x, "r2", "R-Square", details, "both")
  p2 <- ols_plot_stepwise(x, "adj_r2", "Adj. R-Square", details, "both")
  p3 <- ols_plot_stepwise(x, "aic", "AIC", details, "both")
  p4 <- ols_plot_stepwise(x, "rmse", "RMSE", details, "both")

  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3, plot_4 = p4)

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}

