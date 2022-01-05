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

null_model_metrics <- function(model) {
  
  output <- summary(model)
  anovam <- anova(model)
  aic    <- ols_aic(model)
  sbc    <- ols_sbc(model)
  sbic   <- ols_sbic(model, model)
  n      <- length(anovam$Df)
  ess    <- anovam$`Sum Sq`[n]
  tss    <- sum(anovam$`Sum Sq`)
  rss    <- tss - ess
  rsq    <- output$r.squared
  adjr   <- output$adj.r.squared
  rmse   <- sqrt(mean(model$residuals ^ 2))
  
  list(adjr = adjr,
       aic  = aic,
       sbc  = sbc, 
       sbic = sbic,
       ess  = ess,
       rsq  = rsq,
       rss  = rss,
       rmse = rmse)
  
}

max_nchar <- function(char, val, rn = 3, ns = 3) {
  max(nchar(char), nchar(format(round(val, rn), nsmall = ns)))
}

#' @importFrom utils packageVersion menu install.packages
check_suggests <- function(pkg) {
  
  pkg_flag <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)
  
  if (is.na(pkg_flag)) {
    
    msg <- message(paste0('\n', pkg, ' must be installed for this functionality.'))
    
    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::install.packages(pkg)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    } 
  }

}

