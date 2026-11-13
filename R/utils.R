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

# Contrasts of `model` that apply to the terms of `form`. model.matrix() warns
# when a contrasts entry names a variable the formula does not contain, which
# happens whenever selection drops a factor from the candidate terms.
model_contrasts <- function(model, form) {

  con <- model$contrasts

  if (length(con) == 0) {
    return(NULL)
  }

  vars <- rownames(attr(stats::terms(form), "factors"))
  con  <- con[names(con) %in% vars]

  if (length(con) == 0) {
    return(NULL)
  }

  con

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
    lev <- qr_hatvalues(qr)
    if (length(lev) == sum(used)) {
      out[used] <- lev
      return(out)
    }
  }

  # a rank-zero fit (no estimated direction) has zero leverage everywhere
  if (identical(as.integer(model$rank), 0L)) {
    return(out)
  }

  # The fitted projection can only be read from the fit's own QR (its pivot and
  # estimable columns). Rebuilding it from the design matrix would have to guess
  # the tolerance and the pivot, which can give a different fitted space, so a
  # fit without its QR is rejected rather than approximated.
  stop("the model has no 'qr' component, so its leverages cannot be ",
       "recovered exactly; refit with qr = TRUE.", call. = FALSE)

}

# Leverages of a QR decomposition. Only the first `rank` columns of Q span the
# fitted space; for a rank-deficient design the remaining orthonormal columns
# are arbitrary directions that were not estimated, so summing over all of
# them reports leverages that do not belong to the fitted model.
qr_hatvalues <- function(qr) {

  n    <- nrow(qr$qr)
  rank <- qr$rank

  if (is.null(rank) || rank < 1L) {
    return(numeric(n))
  }

  rowSums(qr.Q(qr)[, seq_len(rank), drop = FALSE] ^ 2)

}

# Response metadata attributes (e.g. Stata 'label'/'format.stata') that base R's
# influence functions carry through their arithmetic. The used-row subsetting in
# model_residuals_used() drops them, so they are restored on the per-observation
# statistics below for parity with rstandard()/rstudent()/cooks.distance()/dffits().
model_residual_attrs <- function(model) {

  a <- attributes(model$residuals)

  if (is.null(a)) {
    return(list())
  }

  a[setdiff(names(a), c("names", "dim", "dimnames"))]

}

restore_residual_attrs <- function(x, model) {

  a <- model_residual_attrs(model)

  if (length(a) == 0) {
    return(x)
  }

  for (nm in names(a)) {
    attr(x, nm) <- a[[nm]]
  }

  x

}

# Weighted residuals and leverages of the used rows, with base R's boundary
# conventions from lm.influence(): leverages within 10 * eps of 1 are set to 1
# and residuals that are rounding error relative to the median are set to 0.
model_influence_basis <- function(model) {

  e <- model_residuals_used(model) * sqrt(model_weights_used(model))
  h <- model_hatvalues_used(model)

  if (length(e) > 0) {
    e[abs(e) < 100 * .Machine$double.eps * stats::median(abs(e))] <- 0
  }

  h[h > 1 - 10 * .Machine$double.eps] <- 1

  list(e = e, h = h)

}

# Externally studentized residuals of the rows used in the fit. Base
# rstudent() goes through lm.influence(), whose NA padding can misalign (and
# fail) when zero prior weights and na.exclude omissions occur together, so the
# statistic is computed from the same used-row residuals, weights and leverages
# as PRESS.
model_rstudent_used <- function(model) {

  ib   <- model_influence_basis(model)
  ew   <- ib$e
  h    <- ib$h
  rss  <- sum(ew ^ 2)
  df   <- model$df.residual
  rank <- model$rank

  if (rank == 0L) {
    return(restore_residual_attrs(ew / sqrt(rss / df), model))
  }

  if (df <= 1L) {
    return(restore_residual_attrs(rep(NA_real_, length(ew)), model))
  }

  sigma <- model_sigma_loocv_used(model)
  out   <- ew / (sigma * sqrt(1 - h))
  out[is.infinite(out)] <- NaN

  restore_residual_attrs(out, model)

}

# Leave-one-out residual standard error of the rows used in the fit:
# sqrt((RSS - e_i^2 / (1 - h_i)) / (df - 1)), and sqrt(RSS / (df - 1)) where
# the leverage is 1, as lm.influence() does.
model_sigma_loocv_used <- function(model) {

  ib  <- model_influence_basis(model)
  ew  <- ib$e
  h   <- ib$h
  rss <- sum(ew ^ 2)
  df  <- model$df.residual

  if (df <= 1L) {
    return(rep(NA_real_, length(ew)))
  }

  loo <- ifelse(h < 1, ew ^ 2 / (1 - h), 0)

  sqrt((rss - loo) / (df - 1))

}

# Internally studentized residuals (rstandard()) of the rows used in the fit.
# Undefined (NaN) where the leverage is 1, as in base R.
model_rstandard_used <- function(model) {

  ib    <- model_influence_basis(model)
  rss   <- sum(ib$e ^ 2)
  sigma <- sqrt(rss / model$df.residual)
  out   <- ib$e / (sigma * sqrt(1 - ib$h))
  out[is.infinite(out)] <- NaN

  restore_residual_attrs(out, model)

}

# Cook's distances of the rows used in the fit; NaN where the leverage is 1.
model_cooksd_used <- function(model) {

  ib    <- model_influence_basis(model)
  rss   <- sum(ib$e ^ 2)
  sigma <- sqrt(rss / model$df.residual)
  p     <- model$rank
  out   <- ((ib$e / (sigma * (1 - ib$h))) ^ 2 * ib$h) / p
  out[is.infinite(out)] <- NaN

  restore_residual_attrs(out, model)

}

# DFFITS of the rows used in the fit: e_i sqrt(h_i) / (sigma_(-i) (1 - h_i));
# NaN where the leverage is 1, as base R's dffits().
model_dffits_used <- function(model) {

  ib    <- model_influence_basis(model)
  s_i   <- model_sigma_loocv_used(model)
  out   <- ib$e * sqrt(ib$h) / (s_i * (1 - ib$h))
  out[is.infinite(out)] <- NaN

  restore_residual_attrs(out, model)

}

# DFBETAS of the rows used in the fit. Base R derives these from lm.influence(),
# whose NA padding fails on zero weights plus na.exclude omissions, so the same
# coefficient-change computation is reproduced from the fit QR and the used-row
# residuals/weights/leverages. Columns follow the estimable coefficients.
model_dfbetas_used <- function(model) {

  qr <- model$qr

  if (is.null(qr) || isFALSE(qr) || length(qr$qr) == 0) {
    stop("model does not have a proper 'qr' component.", call. = FALSE)
  }

  rank <- model$rank

  if (rank < 1L) {
    return(matrix(numeric(0), nrow = length(model_residuals_used(model)),
                  ncol = 0L))
  }

  ib      <- model_influence_basis(model)
  e       <- ib$e
  h       <- ib$h
  q       <- qr.Q(qr)[, seq_len(rank), drop = FALSE]
  r       <- qr.R(qr)[seq_len(rank), seq_len(rank), drop = FALSE]
  invRQtt <- t(backsolve(r, t(q)))
  cf      <- invRQtt * ifelse(h == 1, 0, e / (1 - h))
  xxi     <- chol2inv(qr$qr, rank)
  s_i     <- model_sigma_loocv_used(model)

  out <- cf / outer(s_i, sqrt(diag(xxi)))

  cn <- names(coefficients(model))
  if (!is.null(cn)) {
    cn <- cn[!is.na(coefficients(model))]
    if (length(cn) == ncol(out)) {
      colnames(out) <- cn
    }
  }

  out

}

# Original observation identifiers for the rows of the model frame. na.action
# records the omitted positions, while zero-weight rows remain in the frame;
# inverse probability weights, omitted rows and repeated row names therefore
# cannot shift the identifiers.
model_obs_ids <- function(model) {

  n    <- length(model$residuals)
  omit <- model$na.action

  if (is.null(omit)) {
    return(seq_len(n))
  }

  omit <- as.integer(omit)

  if (length(omit) == 0 || anyNA(omit)) {
    return(seq_len(n))
  }

  seq_len(n + length(omit))[-omit]

}

# prior weights of the rows used in the fit; all ones for unweighted models
model_weights_used <- function(model) {

  w <- model$weights

  if (is.null(w)) {
    return(rep(1, length(model$residuals)))
  }

  w[model_used_rows(model)]

}

# fitted values of the rows used in the fit; fitted() pads na.exclude fits and
# lm() keeps a placeholder fitted value for zero-weight rows, so both are
# dropped here to align with the used-row residuals and leverages
model_fitted_used <- function(model) {

  fv <- model$fitted.values

  if (is.null(fv)) {
    fv <- unname(fitted(model))
    if (length(fv) > length(model$residuals)) {
      fv <- fv[!is.na(fitted(model))]
    }
  }

  fv[model_used_rows(model)]

}
