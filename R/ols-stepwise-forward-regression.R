#' Stepwise forward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' entering predictors based on p values, in a stepwise manner until there is
#' no variable left to enter any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param p_enter p value; variables with p value less than \code{p_enter} will
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
ols_step_forward_p.default <- function(model, p_enter = 0.3, include = NULL, exclude = NULL, hierarchical = FALSE, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_model(model)
  check_logic(details)
  check_values(p_enter, 0, 1)
  check_npredictors(model, 3)

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

  if (hierarchical) {
    ols_step_hierarchical(model, p_enter, TRUE, progress, details)
  } else {
    l        <- model$model
    nam      <- colnames(attr(model$terms, "factors"))
    lockterm <- c(include, exclude)
    cterms   <- setdiff(nam, exclude)
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
    rsq      <- c()
    adjrsq   <- c()
    aic      <- c()
    sbic     <- c()
    cp       <- c()
    sbc      <- c()
    rmse     <- c()

    if (progress || details) {
      cat(format("Forward Selection Method", justify = "left", width = 27), "\n")
      cat(rep("-", 27), sep = "", "\n\n")
      cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
      for (i in seq_len(length(cterms))) {
        cat(paste0(i, ". ", cterms[i]), "\n")
      }
      cat("\n")

      cat("We are selecting variables based on p value...")
      cat("\n")

    }

    if (is.null(include)) {
      base_model <- lm(paste(response, "~", 1), data = l)
    } else {
      base_model <- lm(paste(response, "~", paste(include, collapse = " + ")), data = l)
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
    } else {
      if (progress) {
        cat("\n")
        cat("Variables Entered:", "\n\n")
      }
    }

    preds  <- c(preds, all_pred[maxf])
    lpreds <- length(preds)

    if (lpreds > 1) {

      for (i in seq_len(lpreds)) {

        step <- step + 1

        if (details) {
          cat("\n")
          cat(paste("Forward Selection: Step", step), "\n\n")
        }

        npreds <- preds[1:i]
        fr     <- ols_regress(paste(response, "~", paste(npreds, collapse = " + ")), l)
        rsq    <- c(rsq, fr$rsq)
        adjrsq <- c(adjrsq, fr$adjr)
        aic    <- c(aic, ols_aic(fr$model))
        sbc    <- c(sbc, ols_sbc(fr$model))
        sbic   <- c(sbic, ols_sbic(fr$model, model))
        cp     <- c(cp, ols_mallows_cp(fr$model, model))
        rmse   <- c(rmse, fr$rmse)

        if (progress) {
          cat("-", tail(npreds, n = 1), "\n")
        }

        if (details) {
          cat("Variable entered =>", tail(npreds, n = 1))
          cat("\n")
          m <- ols_regress(paste(response, "~", paste(npreds, collapse = " + ")), l)
          print(m)
          cat("\n\n")
        }
      }

    } else {

      step <- step + 1

      if (details) {
        cat("\n")
        cat(paste("Forward Selection: Step", step), "\n\n")
      }

      fr     <- ols_regress(paste(response, "~", preds), l)
      rsq    <- c(rsq, fr$rsq)
      adjrsq <- c(adjrsq, fr$adjr)
      aic    <- c(aic, ols_aic(fr$model))
      sbc    <- c(sbc, ols_sbc(fr$model))
      sbic   <- c(sbic, ols_sbic(fr$model, model))
      cp     <- c(cp, ols_mallows_cp(fr$model, model))
      rmse   <- c(rmse, fr$rmse)

      if (progress) {
        cat(paste("-", tail(preds, n = 1)), "\n")
      }

      if (details) {
        cat("Variable entered =>", tail(preds, n = 1))
        cat("\n")
        m <- ols_regress(paste(response, "~", preds), l)
        print(m)
        cat("\n\n")
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

        if (details) {
          cat("\n")
          cat(paste("Forward Selection: Step", step), "\n\n")
        }

        if (progress) {
          len_maxf <- length(maxf)
          if (len_maxf > 1) {
            cat("=>", paste(tail(preds, n = len_maxf), collapse = " & "), "\n")
          } else {
            cat("=>", tail(preds, n = 1), "\n")
          }
        }

        if (details) {
          len_maxf <- length(maxf)
          if (len_maxf > 1) {
            cat("Variable entered =>", paste(tail(preds, n = len_maxf), collapse = " & "), "\n")
          } else {
            cat("Variable entered =>", tail(preds, n = 1), "\n")
          }
          cat("\n")
          m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
          print(m)
          cat("\n\n")
        }
      } else {
        if (progress || details) {
          cat("\n")
          cat("No more variables to be added.")
        }
        break
      }
    }

    prsq <- c(rsq[1], diff(rsq))

    if (details) {
      cat("\n\n")
      len_pred <- length(preds)
      if (len_pred < 1) {
        cat("Variables Entered => None", "\n\n")
      } else if (len_pred == 1) {
        cat(paste("Variables Entered =>", preds[1]), "\n\n")
      } else {
        cat("Variables Entered:", "\n\n")
        for (i in seq_len(length(preds))) {
          if (details) {
            cat("=>", preds[i], "\n")
          } else {
            cat(paste("=>", preds[i]), "\n")
          }
        }
      }
    }

    if (progress || details) {
      cat("\n\n")
      cat("Final Model Output", "\n")
      cat(rep("-", 18), sep = "", "\n\n")

      fi <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), data = l)
      print(fi)
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
    print_step_forward(x)
  } else {
    print("No variables have been added to the model.")
  }
}

#' @export
#' @rdname ols_step_forward_p
#'
plot.ols_step_forward_p <- function(x, model = NA, print_plot = TRUE, ...) {

  a <- NULL
  b <- NULL

  y  <- c(0, x$metrics$step)
  np <- coeff_names(x$others$base_model)

  if (is.null(np)) {
    mi <- null_model_metrics(x$others$base_model)
  } else {
    mi <- ols_regress(x$others$base_model)
  }
  
  r2   <- c(mi$rsq, x$metrics$r2)
  adjr <- c(mi$adjr, x$metrics$adj_r2)
  aic  <- c(mi$aic, x$metrics$aic)
  rmse <- c(mi$rmse, x$metrics$rmse)
  
  predictors <- c("Base Model", x$metrics$variable)

  d1 <- data.frame(a = y, b = r2)
  d2 <- data.frame(a = y, b = adjr)
  d4 <- data.frame(a = y, b = aic)
  d5 <- data.frame(a = y, b = rmse)

  p1 <- plot_stepwise(d1, "R-Square")
  p2 <- plot_stepwise(d2, "Adj. R-Square")
  p3 <- plot_stepwise(d4, "AIC")
  p4 <- plot_stepwise(d5, "RMSE")

  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3, plot_4 = p4)

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}
