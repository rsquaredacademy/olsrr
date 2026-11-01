#' @importFrom stats model.matrix confint.lm
#' @importFrom stats as.formula deviance
reg_comp <- function(formula, data, conf.level = 0.95, iterm, title = "model", model = NULL) {

  if (is.null(model)) {
    model <- lm(formula = formula, data = data)
  }
  nam        <- names(model.frame(model))
  response   <- nam[1]

  # a model frame adds synthetic columns for prior weights and offsets; neither
  # is a predictor
  predictors <- setdiff(nam[-1], synthetic_model_columns(model))
  output     <- summary(model)
  anovam     <- anova(model)
  dep        <- model.frame(model)[1]
  rsq        <- output$r.squared
  r          <- sqrt(rsq)
  adjr       <- output$adj.r.squared
  ems        <- deviance(model) / model$df.residual
  mse        <- ems
  rmse       <- sqrt(ems)
  cv         <- (output$sigma / mean(dep[[1]])) * 100
  mae        <- mean(abs(model_residuals_used(model)))
  aic        <- ols_aic(model)
  sbc        <- ols_sbc(model)
  sbic       <- ols_sbic(model, model)
  prsq       <- ols_pred_rsq(model)
  n          <- length(anovam$Df)
  error_df   <- anovam$Df[n]
  model_df   <- sum(anovam$Df) - error_df
  total_df   <- sum(anovam$Df)
  ess        <- anovam$`Sum Sq`[n]
  tss        <- sum(anovam$`Sum Sq`)
  rss        <- tss - ess
  rms        <- rss / model_df
  ems        <- ess / error_df
  f          <- rms / ems
  p          <- pf(f, model_df, error_df, lower.tail = F)

  # standardised betas
  int <- attr(model$terms, "intercept")

  if ((ncol(model.matrix(model)) - int) < 1) {

    # intercept-only model: no predictors to standardise
    sbetas <- numeric(0)

  } else if (iterm) {

    # standardise the modelled numeric terms and refit. Terms are standardised
    # on the scale at which they enter the model (after any transformation),
    # factor terms are left alone, and the response (which may be a call such
    # as log(y)) and any offsets are carried through unchanged
    sc  <- ols_scale_terms(model, data)

    fit_args <- list(formula = sc$formula, data = sc$data)
    if (!is.null(model$weights)) {
      fit_args$weights <- unname(model$weights)
    }
    # the formula offset terms are kept in the formula and the call offset (the
    # part not already in the formula) is supplied separately, so neither is
    # dropped and neither is counted twice
    if (!is.null(sc$offset)) {
      fit_args$offset <- sc$offset
    }

    model2  <- do.call(stats::lm, fit_args)
    output2 <- summary(model2)
    b       <- output2$coef[, 1]
    names(b) <- rownames(output2$coef)
    g       <- as.data.frame(model.matrix(model2))
    if (int) {
      b <- b[-1]
      g <- g[, -1, drop = FALSE]
    }

    # align by name: aliased columns have no estimate
    keep    <- names(b) %in% names(g)
    b       <- b[keep]
    g       <- g[, names(b), drop = FALSE]
    sx      <- vapply(g, sd, numeric(1))
    sy      <- sd(model.response(model.frame(model)))
    sbeta   <- b * sx / sy
    sbetas  <- sbeta
    names(sbetas) <- ols_term_names(names(sbeta), sc$map)

  } else {

    # keep every coefficient when there is no intercept
    b        <- output$coef[, 1]
    names(b) <- rownames(output$coef)
    g        <- as.data.frame(model.matrix(model))
    if (int) {
      b <- b[-1]
      g <- g[, -1, drop = FALSE]
    }

    # align by name: aliased columns have no estimate
    keep     <- names(b) %in% names(g)
    b        <- b[keep]
    g        <- g[, names(b), drop = FALSE]
    sx       <- vapply(g, sd, numeric(1))
    sy       <- sapply(model$model[1], sd)
    sbeta    <- b * sx / sy
    sbetas   <- sbeta

  }

  # coefficient table aligned by name; aliased coefficients have no estimate,
  # standard error, t statistic or p-value
  betas      <- coefficients(model)
  cf         <- output$coefficients
  k          <- length(betas)
  std_errors <- rep(NA_real_, k)
  tvalues    <- rep(NA_real_, k)
  pvalues    <- rep(NA_real_, k)
  names(std_errors) <- names(tvalues) <- names(pvalues) <- names(betas)

  idx        <- match(rownames(cf), names(betas))
  ok         <- !is.na(idx)
  std_errors[idx[ok]] <- cf[ok, 2]
  tvalues[idx[ok]]    <- cf[ok, 3]
  pvalues[idx[ok]]    <- cf[ok, 4]

  conf_lm    <- confint.lm(model, level = conf.level)
  conf_full  <- matrix(NA_real_, k, 2L,
                       dimnames = list(names(betas), colnames(conf_lm)))
  cidx       <- match(rownames(conf_lm), names(betas))
  cok        <- !is.na(cidx)
  conf_full[cidx[cok], ] <- conf_lm[cok, , drop = FALSE]
  conf_lm    <- conf_full

  df         <- rep(1, n)
  mvars      <- names(model$coefficients)


  result <- list(
    r          = r,
    rsq        = rsq,
    adjr       = adjr,
    rmse       = rmse,
    cv         = cv,
    mse        = mse,
    mae        = mae,
    aic        = aic,
    sbc        = sbc,
    sbic       = sbic,
    prsq       = prsq,
    error_df   = error_df,
    model_df   = model_df,
    total_df   = total_df,
    ess        = ess,
    rss        = rss,
    tss        = tss,
    rms        = rms,
    ems        = ems,
    f          = f,
    p          = p,
    n          = n,
    betas      = betas,
    sbetas     = sbetas,
    std_errors = std_errors,
    tvalues    = tvalues,
    pvalues    = pvalues,
    df         = df,
    conf_lm    = conf_lm,
    title      = title,
    dependent  = response,
    predictors = predictors,
    mvars      = mvars,
    model      = model
  )

  return(result)
}

# Data and formula for the standardised-beta refit of a model with interaction
# terms. Each numeric component of the model terms is standardised on the scale
# at which it enters the model (so log(wt) is standardised after the
# transformation), factor components are left untouched, and offsets are
# carried through unchanged. `map` maps the substituted column names back to the
# original term components.
ols_scale_terms <- function(model, data) {

  fml     <- formula(model)
  labels  <- attr(model$terms, "term.labels")
  off_idx <- attr(model$terms, "offset")
  vars    <- as.list(attr(model$terms, "variables"))[-1]
  off_var <- vars[off_idx]
  off_lbl <- vapply(off_var, function(x) paste(deparse(x), collapse = ""), character(1))
  resp    <- paste(deparse(fml[[2]]), collapse = "")
  env     <- environment(fml)

  if (is.null(env)) {
    env <- parent.frame()
  }

  d <- as.data.frame(data)

  # components of the terms; interactions are rebuilt from their components
  comps <- unique(unlist(strsplit(labels, ":", fixed = TRUE)))
  comps <- comps[nzchar(comps)]

  # the response is never standardised. An offset variable is: the offset is
  # carried as a frozen vector evaluated on the original columns, while the
  # predictor occurrence is a separate column of the refit design.
  skip <- all.vars(fml[[2]])

  map <- character(0)

  for (cmp in comps) {

    if (cmp %in% skip) {
      next
    }

    val <- tryCatch(
      eval(parse(text = cmp), envir = d, enclos = env),
      error = function(e) NULL
    )

    # factors, characters and any term that cannot be evaluated are left alone
    if (is.null(val) || !is.numeric(val)) {
      next
    }

    if (!is.null(dim(val))) {
      stop("standardised betas are not available for matrix terms such as poly() in models with interactions.", call. = FALSE)
    }

    if (length(val) != nrow(d)) {
      next
    }

    # a constant term has no scale to standardise; it is aliased anyway
    sv <- tryCatch(stats::sd(val), error = function(e) NA_real_)
    if (!is.finite(sv) || sv == 0) {
      next
    }

    new <- ".ols_sb"
    i   <- 1
    while (new %in% names(d)) {
      new <- paste0(".ols_sb_", i)
      i   <- i + 1
    }

    d[[new]]   <- as.numeric(scale(val))
    map[new]   <- cmp

  }

  new_labels <- vapply(labels, function(lbl) {
    parts <- strsplit(lbl, ":", fixed = TRUE)[[1]]
    hit   <- parts %in% map
    parts[hit] <- names(map)[match(parts[hit], map)]
    paste(parts, collapse = ":")
  }, character(1))

  rhs <- c(if (attr(model$terms, "intercept") == 0) "0", new_labels, off_lbl)
  if (length(rhs) == 0) {
    rhs <- "1"
  }

  fml2 <- as.formula(paste(resp, "~", paste(rhs, collapse = " + ")), env = env)

  # offset supplied through the model call; the offset() terms of the original
  # formula stay in the refit formula, where they are evaluated on the original
  # columns and never standardised
  off <- model_call_offset(model, d)

  if (!is.null(off) && length(off) != nrow(d)) {
    off <- NULL
  }

  list(data = d, formula = fml2, map = map, offset = off)

}

# Translate the refitted coefficient names back to the original term labels
ols_term_names <- function(nms, map) {

  if (length(map) == 0) {
    return(nms)
  }

  vapply(strsplit(nms, ":", fixed = TRUE), function(parts) {
    idx <- match(parts, names(map))
    parts[!is.na(idx)] <- unname(map[idx[!is.na(idx)]])
    paste(parts, collapse = ":")
  }, character(1))

}
