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

  check_inputs(model, include, exclude, progress, details)
  check_values(p_enter, 0, 1)

  indterms <- coeff_names(model)
  lenterms <- length(indterms)

  if (is.numeric(include)) {
    include <- indterms[include]
  }

  if (is.numeric(exclude)) {
    exclude <- indterms[exclude]
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
          cat("=>", tail(npreds, n = 1), "\n")
        }

        if (details) {
          cat("Variable entered =>", tail(npreds, n = 1))
          cat("\n\n")
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
        cat(paste("=>", tail(preds, n = 1)), "\n")
      }

      if (details) {
        cat("Variable entered =>", tail(preds, n = 1))
        cat("\n\n")
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
          cat("\n\n")
          m <- ols_regress(paste(response, "~", paste(preds, collapse = " + ")), l)
          print(m)
          cat("\n\n")
        }
      } else {
        if (progress || details) {
          cat("\n")
          cat("No more variables to be added.")
          cat("\n")
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
plot.ols_step_forward_p <- function(x, model = NA, print_plot = TRUE, details = TRUE, ...) {

  a <- NULL

  p1 <- plot_stepwise(x, metric = "r2", y_lab = "R-Square", details =  details)
  p2 <- plot_stepwise(x, metric = "adj_r2", y_lab = "Adjusted R-Square", details = details)
  p3 <- plot_stepwise(x, metric = "aic", y_lab = "AIC", details = details)
  p4 <- plot_stepwise(x, metric = "rmse", y_lab = "RMSE", details = details)

  myplots <- list(plot_1 = p1, plot_2 = p2, plot_3 = p3, plot_4 = p4)

  if (print_plot) {
    marrangeGrob(myplots, nrow = 2, ncol = 2)
  } else {
    return(myplots)
  }

}

plot_stepwise <- function(x, metric = "r2", y_lab = "R-Square", details = TRUE, direction = "forward") {
  
  step <- x$metrics$step
  r2   <- x$metrics[[metric]]
  
  if (details) {
    x$metrics$text <- paste0("[", x$metrics$variable, ", ", round(x$metrics[[metric]], 2), "]")
    pred <- x$metrics$text
  } else {
    pred <- x$metrics$variable
  }

  if (metric == "r2") {
    met <- "rsq"
  } else if (metric == "adj_r2") {
    met <- "adjr"
  } else {
    met <- metric
  }

  if (direction == "forward") {
    the_model <- x$others$base_model
    the_info  <- " Base Model  : "
  } else {
    the_model <- x$others$full_model
    the_info  <- " Full Model  : "
  }

  np <- coeff_names(the_model)
  if (is.null(np)) {
    mi <- null_model_metrics(the_model)
  } else {
    mi <- ols_regress(the_model)
  }

  base_model_met  <- round(mi[[met]], 3)
  final_model_met <- round(ols_regress(x$model)[[met]], 3)
  metric_info <- paste0(the_info, format(base_model_met, nsmall = 3), "\n",
                        "Final Model : ", format(final_model_met, nsmall = 3))
  
  y    <- step
  xloc <- y
  yloc <- r2
  xmin <- min(y) - 1
  xmax <- max(y) + 1
  ymin <- min(r2) - (min(r2) * 0.03)
  ymax <- max(r2) + (max(r2) * 0.03)

  a <- NULL
  b <- NULL
  tx <- NULL
  
  d  <- data.frame(a = y, b = r2)
  d2 <- data.frame(x = xloc, y = yloc, tx = pred)
  
  v_just <- ifelse(metric %in% c("aic", "rmse"), "bottom", "top")
  h_just <- ifelse(metric %in% c("aic", "rmse"), 1.2, 0)
  ann_x  <- ifelse(metric %in% c("aic", "rmse"), Inf, 0)

  if (metric == "r2") {
    title <- "R-Square"
  } else if (metric == "adj_r2") {
    title <- "Adjusted R-Square"
  } else if (metric == "aic") {
    title <- "Akaike Information Criteria"
  } else {
    title <- "Root Mean Squared Error"
  }
  
  ggplot(d, aes(x = a, y = b)) +
    geom_line(color = "blue") +
    geom_point(color = "blue", shape = 1, size = 2) +
    xlim(c(xmin, xmax)) +
    ylim(c(ymin, ymax)) + 
    xlab("Step") + 
    ylab(y_lab) + 
    ggtitle(title) +
    geom_text(data = d2, aes(x = x, y = y, label = tx), size = 3,
              hjust = "left", vjust = v_just, nudge_x = 0.05) +
    annotate("text", x = ann_x, y = Inf, hjust = h_just, vjust = 2,
             family = "serif", fontface = "bold", size = 3,
             label = metric_info)

}
