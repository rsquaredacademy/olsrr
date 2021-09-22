#' Stepwise backward regression
#'
#' @description
#' Build regression model from a set of candidate predictor variables by
#' removing predictors based on p values, in a stepwise manner until there is
#' no variable left to remove any more.
#'
#' @param model An object of class \code{lm}; the model should include all
#'   candidate predictor variables.
#' @param p_remove p value; variables with p more than \code{p_remove} will be removed
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
#' # selection metrics
#' k$metrics
#'
#' # final model
#' k$model
#'
#' @family variable selection procedures
#'
#' @export
#'
ols_step_backward_p <- function(model, ...) UseMethod("ols_step_backward_p")

#' @export
#' @rdname ols_step_backward_p
#'
ols_step_backward_p.default <- function(model, p_remove = 0.3, include = NULL, exclude = NULL, hierarchical = FALSE, progress = FALSE, details = FALSE, ...) {

  if (details) {
    progress <- FALSE
  }

  check_model(model)
  check_logic(details)
  check_values(p_remove, 0, 1)
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
    ols_step_hierarchical(model, p_remove, FALSE, progress, details)
  } else {
    l        <- model$model
    nam      <- colnames(attr(model$terms, "factors"))
    lockterm <- c(include, exclude)
    response <- names(model$model)[1]
    preds    <- setdiff(nam, lockterm)
    cterms   <- c(include, preds)
    end      <- FALSE
    step     <- 0
    rpred    <- c()
    rsq      <- c()
    adjrsq   <- c()
    aic      <- c()
    sbic     <- c()
    sbc      <- c()
    cp       <- c()
    rmse     <- c()
    ppos     <- step + 1 + length(include)

    if (progress || details) {
      cat(format("Backward Elimination Method", justify = "left", width = 27), "\n")
      cat(rep("-", 27), sep = "", "\n\n")
      cat(format("Candidate Terms:", justify = "left", width = 16), "\n\n")
      for (i in seq_len(length(cterms))) {
        cat(paste(i, ".", cterms[i]), "\n")
      }
      cat("\n")

      cat("We are eliminating variables based on p value...")
      cat("\n")
    }

    while (!end) {
      m     <- lm(paste(response, "~", paste(c(include, cterms), collapse = " + ")), l)
      m_sum <- Anova(m)
      nm    <- length(m_sum$`F value`)
      fvals <- m_sum$`F value`[ppos:nm]
      pvals <- m_sum$`Pr(>F)`
      minf  <- which(fvals == min(fvals, na.rm = TRUE)) + ppos - 1


        if (pvals[minf] > p_remove) {

          step   <- step + 1
          rpred  <- c(rpred, cterms[minf])
          cterms  <- cterms[-minf]
          lp     <- length(rpred)
          fr     <- ols_regress(paste(response, "~", paste(c(include, cterms), collapse = " + ")), l)
          rsq    <- c(rsq, fr$rsq)
          adjrsq <- c(adjrsq, fr$adjr)
          aic    <- c(aic, ols_aic(fr$model))
          sbc    <- c(sbc, ols_sbc(fr$model))
          sbic   <- c(sbic, ols_sbic(fr$model, model))
          cp     <- c(cp, ols_mallows_cp(fr$model, model))
          rmse   <- c(rmse, fr$rmse)

          if (progress) {
            cat("\n")
            cat("Variables Removed:", "\n\n")
            cat(paste("-", tail(rpred, n = 1)), "\n")
          }

          if (details) {
            cat("\n")
            cat(paste("Backward Elimination: Step", step), "\n\n", paste("Variable", rpred[lp], "Removed"), "\n\n")
            m <- ols_regress(paste(response, "~", paste(c(include, cterms), collapse = " + ")), l)
            print(m)
            cat("\n\n")
          }
        } else {
          end <- TRUE
          if (progress || details) {
            cat("\n")
            cat(paste0("No more variables satisfy the condition of p value = ", p_remove))
            cat("\n")
          }
        }

    }

    if (details) {
      cat("\n\n")
      len_pred <- length(rpred)
      if (len_pred < 1) {
        cat("Variables Removed: None", "\n\n")
      } else if (len_pred == 1) {
        cat(paste("Variables Removed:", rpred[1]), "\n\n")
      } else {
        cat("Variables Removed:", "\n\n")
        for (i in seq_len(len_pred)) {
          cat(paste("-", rpred[i]), "\n")
        }
      }
    }

    if (progress || details) {
      cat("\n\n")
      cat("Final Model Output", "\n")
      cat(rep("-", 18), sep = "", "\n\n")

      fi <- ols_regress(paste(response, "~", paste(c(include, cterms), collapse = " + ")), data = l)
      print(fi)
    }

    final_model <- lm(paste(response, "~", paste(c(include, cterms), collapse = " + ")), data = l)

    metrics     <- data.frame(step       = seq_len(step),
                              variable   = rpred,
                              r2         = rsq,
                              adj_r2     = adjrsq,
                              aic        = aic,
                              sbic       = sbic,
                              sbc        = sbc,
                              mallows_cp = cp,
                              rmse       = rmse)

    out <- list(metrics    = metrics,
                model      = final_model,
                others     = list(full_model = model))

    class(out) <- "ols_step_backward_p"

    return(out)
  }

}

#' @export
#'
print.ols_step_backward_p <- function(x, ...) {
  if (length(x$metrics$step) > 0) {
    print_step_backward(x)
  } else {
    print("No variables have been removed from the model.")
  }
}



#' @export
#' @rdname ols_step_backward_p
#'
plot.ols_step_backward_p <- function(x, model = NA, print_plot = TRUE, ...) {

  a <- NULL
  b <- NULL

  y <- c(0, seq_len(length(x$metrics$step)))

  mi   <- ols_regress(x$others$full_model)
  r2   <- c(mi$rsq, x$metrics$r2)
  adjr <- c(mi$adjr, x$metrics$adj_r2)
  aic  <- c(mi$aic, x$metrics$aic)
  rmse <- c(mi$rmse, x$metrics$rmse)

  d1 <- data.frame(a = y, b = r2)
  d2 <- data.frame(a = y, b = adjr)
  d3 <- data.frame(a = y, b = aic)
  d4 <- data.frame(a = y, b = rmse)

  p1 <- plot_stepwise(d1, "R-Square")
  p2 <- plot_stepwise(d2, "Adj. R-Square")
  p3 <- plot_stepwise(d3, "AIC")
  p4 <- plot_stepwise(d4, "RMSE")

  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3, plot_4 = p4)

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}
