fs <- function() {
  x <- rep("  ")
  return(x)
}

fg <- function(x, w) {
  z <- as.character(x)
  y <- format(z, width = w, justify = "right")
  return(y)
}

fl <- function(x, w) {
  x <- as.character(x)
  ret <- format(x, width = w, justify = "left")
  return(ret)
}

fc <- function(x, w) {
  x <- as.character(x)
  ret <- format(x, width = w, justify = "centre")
  return(ret)
}

fw <- function(x, w) {
  z <- format(as.character(x), width = w, justify = "right")
  return(z)
}

formatter_t <- function(x, w) {
  ret <- format(x, width = w, justify = "centre")
  return(ret)
}

formatter_n <- function(x, w) {
  ret <- format(x, nsmall = 3)
  ret1 <- format(ret, width = w, justify = "centre")
  return(ret1)
}

format_cil <- function(x, w) {
  ret <- format(x, nsmall = 3)
  ret1 <- format(ret, width = w, justify = "centre")
  return(ret1)
}

format_ciu <- function(x, w) {
  ret <- format(x, nsmall = 3)
  ret1 <- format(ret, width = w, justify = "centre")
  return(ret1)
}

formats_t <- function() {
  x <- rep("  ")
  return(x)
}

l <- function(x) {
  x <- as.character(x)
  k <- grep("\\$", x)
  if (length(k) == 1) {
    temp <- strsplit(x, "\\$")
    out <- temp[[1]][2]
  } else {
    out <- x
  }
  return(out)
}

null_model_metrics <- function(model, full_model) {

  output <- summary(model)
  anovam <- anova(model)
  aic    <- ols_aic(model)
  sbc    <- ols_sbc(model)
  sbic   <- ols_sbic(model, full_model)
  n      <- length(anovam$Df)
  ess    <- anovam$`Sum Sq`[n]
  tss    <- sum(anovam$`Sum Sq`)
  rss    <- tss - ess
  rsq    <- output$r.squared
  adjr   <- output$adj.r.squared
  ems    <- deviance(model) / model$df.residual
  rmse   <- sqrt(ems)

  list(adjr = adjr, aic = aic, sbc = sbc,  sbic = sbic, ess = ess,
       rsq = rsq, rss = rss, rmse = rmse)

}

max_nchar <- function(char, val, rn = 3, ns = 3) {
  max(nchar(char), nchar(format(round(val, rn), nsmall = ns)))
}

# data used to fit a model, so that its original formula can be re-evaluated
#
# `model$model` stores term labels (e.g. "log(mpg)", "factor(cyl)") as column
# names, so refitting a formula against it fails for any call term. When the
# data are still available from the model call, return them restricted to the
# rows used by the model (NA handling, subset, ...). Otherwise fall back to
# `model$model`, which is safe for plain variable terms.
ols_model_data <- function(model) {

  env  <- environment(formula(model))
  call <- model$call

  if (is.null(env)) {
    env <- parent.frame()
  }

  data <- NULL
  if (!is.null(call) && !is.null(call$data)) {
    data <- tryCatch(eval(call$data, envir = env), error = function(e) NULL)
    if (is.null(data)) {
      data <- tryCatch(eval(call$data, envir = parent.frame()), error = function(e) NULL)
    }
  }

  if (!is.data.frame(data) && !is.matrix(data)) {
    return(model$model)
  }

  data <- as.data.frame(data)
  rn   <- attr(model$model, "row.names")

  if (!is.null(rn)) {
    rn  <- as.character(rn)
    idx <- match(rn, rownames(data))
    if (anyNA(idx) || anyDuplicated(idx) || length(idx) != nrow(model$model)) {
      return(model$model)
    }
    data <- data[idx, , drop = FALSE]
  }

  data
}

# Model-frame columns that are not predictors: the synthetic prior weights and
# offset columns and the offset terms themselves.
synthetic_model_columns <- function(model) {

  off_terms <- attr(model$terms, "offset")
  drop      <- c("(weights)", "(offset)")

  if (length(off_terms) > 0) {
    variables <- as.list(attr(model$terms, "variables"))[-1]
    drop      <- c(drop, vapply(variables[off_terms], function(x) {
      paste(deparse(x), collapse = "")
    }, character(1)))
  }

  drop
}

# Sum of the offset() terms of the model formula on the model-frame rows.
# Returns NULL when the formula has no offset terms.
model_formula_offset <- function(model, data = NULL) {

  off_idx <- attr(model$terms, "offset")

  if (length(off_idx) == 0) {
    return(NULL)
  }

  variables <- as.list(attr(model$terms, "variables"))[-1]
  offs      <- variables[off_idx]
  nms       <- vapply(offs, function(x) paste(deparse(x), collapse = ""), character(1))

  # the model frame keeps the offset columns exactly as the formula found them
  mf <- model$model

  if (!is.null(mf) && all(nms %in% names(mf))) {
    return(unname(rowSums(as.data.frame(mf)[, nms, drop = FALSE])))
  }

  if (is.null(data)) {
    data <- ols_model_data(model)
  }

  d   <- as.data.frame(data)
  env <- environment(formula(model))
  if (is.null(env)) env <- parent.frame()

  vals <- lapply(offs, function(x) {
    tryCatch(eval(x, envir = d, enclos = env), error = function(e) NULL)
  })

  if (any(vapply(vals, is.null, logical(1))) || any(lengths(vals) != nrow(d))) {
    return(NULL)
  }

  unname(rowSums(as.data.frame(vals)))

}

# Offset supplied through the model call, as opposed to offset() terms in the
# formula, on the model-frame rows. lm() stores the evaluated total offset in
# model$offset, so the part that belongs to the call is the total minus the
# formula part; passing only one of them changes the model.
model_call_offset <- function(model, data = NULL) {

  total <- model$offset

  if (is.null(total) || length(total) == 0) {
    return(NULL)
  }

  if (length(attr(model$terms, "offset")) == 0) {
    return(unname(total))
  }

  foff <- model_formula_offset(model, data)

  if (is.null(foff) || length(foff) != length(total)) {
    return(NULL)
  }

  unname(total - foff)

}

# Rows of the model frame actually used in the fit. lm() drops observations
# with zero prior weights, but keeps their residuals (equal to zero) in
# model$residuals, so residuals, weights and leverages must be aligned.
model_used_rows <- function(model) {

  n <- length(model$residuals)
  w <- model$weights

  if (is.null(w) || length(w) != n) {
    return(rep(TRUE, n))
  }

  w != 0
}

# Residuals, leverages and prior weights of the rows used in the fit.
# residuals() pads these to the original number of rows for na.exclude fits;
# model$residuals is always unpadded and hatvalues() reports leverages for the
# rows used in the fit.
model_residuals_used <- function(model) {

  model$residuals[model_used_rows(model)]

}

model_hatvalues_used <- function(model) {

  lev <- model_hatvalues_frame(model)
  lev[model_used_rows(model)]

}

# Leverages of every model-frame row. lm() keeps one row of the fit QR per
# observation used in the fit, so the leverages are read by row index of the
# model frame and are never matched by row name. Duplicated, missing, padded or
# absent row names therefore cannot misalign residuals, weights and leverages.
# Rows with a zero prior weight contribute nothing to the fit and get 0.
model_hatvalues_frame <- function(model) {

  used <- model_used_rows(model)
  out  <- numeric(length(used))
  qr   <- model$qr

  if (!is.null(qr) && !isFALSE(qr) && length(qr$qr) > 0) {
    lev <- rowSums(qr.Q(qr) ^ 2)
    if (length(lev) == sum(used)) {
      out[used] <- lev
      return(out)
    }
  }

  # models fitted without a QR decomposition: derive the leverages from the
  # design matrix, which also keeps the model-frame row order
  x <- model.matrix(model)
  w <- model$weights

  if (is.null(w) || length(w) != nrow(x)) {
    w <- rep(1, nrow(x))
  }

  rowSums(qr.Q(qr(sqrt(w) * x)) ^ 2)

}

# prior weights of the rows used in the fit; all ones for unweighted models
model_weights_used <- function(model) {

  w <- model$weights

  if (is.null(w)) {
    return(rep(1, length(model$residuals)))
  }

  w[model_used_rows(model)]

}

# fitted values of the rows used in the fit; fitted() pads na.exclude fits
model_fitted_used <- function(model) {

  fv <- model$fitted.values

  if (is.null(fv)) {
    fv <- unname(fitted(model))
    if (length(fv) > length(model$residuals)) {
      fv <- fv[!is.na(fitted(model))]
    }
  }

  fv

}
