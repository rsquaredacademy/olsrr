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
