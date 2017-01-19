# helper functions: regress
fs <- function() {
    x <- rep("  ")
    return(x)
}

fg <- function(x, w) {
    z <- as.character(x)
    y <- format(z, width = w, justify = 'right')
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

# var-test
fw <- function(x, w) {
    z <- format(as.character(x), width = w, justify = 'right')
    return(z)
}

# one-samp-var-test
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


# helper functions: fitted line properties
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

# best-subsets
# combinations
combinations <- function(n, r) {
    factorial(n) / (factorial(n - r) * factorial(r))
}

# added variable Plot
advarx <- function(data , i) {
    j <- i - 1
    k <- names(data)
  fla <- as.formula(paste(k[j], "~ ."))
  out <- residuals(lm(fla, data = data))
  return(out)
}

advary <- function(data, i) {
  dat <- data[-i]
    k <- names(dat)
  fla <- as.formula(paste(k[1], "~ ."))
  out <- residuals(lm(fla, data = dat))
  return(out)
}

# bartlett test
#' @importFrom stats complete.cases var
bartlett_fstat <- function(variable, grp_var) {

  n     <- length(variable)
	k     <- nlevels(grp_var)
	comp  <- complete.cases(variable, grp_var)
	vars  <- tapply(variable[comp], grp_var[comp], var)
	lens  <- tapply(variable[comp], grp_var[comp], length)
	v     <- lens - 1
	sumv  <- sum(v)
	isumv <- sum(1 / v)
	c     <- 1 + (1 / (3 * (k - 1))) * (isumv - (1 / sumv))
	n2    <- sum(v * log10(vars))
	l     <- length(vars)
	ps    <- ((lens - 1) * vars) / (n - k)

	pvar  <- sum(ps)
  result <- ((1 / c) * (sumv * log10(pvar) - n2)) * 2.3026
  return(result)
}


# eigen cindex
evalue <- function(x) {

               y <- cbind(1,x)
	colnames(y)[1] <- "intercept"
	             z <- scale(y, scale = T, center = F)
	            tu <- t(z) %*% z
	             e <- eigen(tu / diag(tu))$values

          result <- list(e = e, pvdata = z)

  return(result)

}


cindx <- function(e) {
  return(sqrt(e[1] / e))
}


pveindex <- function(z) {
  svdx <- svd(z)
	 phi <- svdx$v %*% diag(1/svdx$d)
	  ph <- t(phi ^ 2)
	  pv <- prop.table(ph %*% diag(rowSums(ph, 1)), 2)
    return(pv)
}

# vif tol
fmrsq <- function(nam, data, i) {
   fm <- as.formula(paste(nam[i], "~ ."))
   m1 <- lm(fm, data = data)
  rsq <- 1 - (summary(m1)$r.squared)
  return(rsq)
}

# component plus residual plot
cpdata <- function(data, mc, e, i) {
  x <- data[i]
  y <- (mc[i] * data[i]) + e
  d <- tibble(x = x[[1]], y = y[[1]])
  return(d)
}

cpout <- function(model) {
  e      <- residuals(model)
	mc     <- model$coefficients[-1]
	data   <- model.frame(model)[-1]
	lmc    <- length(mc)
	nam    <- names(data)
	indvar <- names(model.frame(model))[1]
  out <- list(e = e, mc = mc, data = data, lmc = lmc, nam = nam, indvar = indvar)
  return(out)
}

# cook's d bar plot
cdplot <- function(model){

           cooksd <- cooks.distance(model)
            	  n <- length(cooksd)
	            ckd <- tibble(obs = seq_len(n), cd = cooksd)
	      ckd$color <- ifelse(ckd$cd >= ts, c("outlier"), c("normal"))
	     ckd$color1 <- factor(ckd$color)
	ckd$Observation <- ordered(ckd$color1, levels = c("normal", "outlier"))
               ts <- 4 / length(ckd$cd)
	           maxx <- max(ckd$cd) + 0.1
           result <- list(ckd = ckd, maxx = maxx, ts = ts)
  return(result)

}

# cook's d chart
cdchart <- function(model) {
  ckd <- cooks.distance(model)
	 ts <- 4 / length(ckd)
	  d <- tibble(obs = seq_len(length(ckd)), ckd = ckd)
  out <- list(d = d, ts = ts)
  return(out)
}

# correlations
cordata <- function(model) {
  d <- model %>%
    model.frame() %>%
    as_data_frame() %>%
    map_df(as.numeric)
  return(d)
}

cmdata <- function(mdata) {
  d <- mdata %>%
    cor() %>%
    `[`(-1, 1)
  return(d)
}

rtwo <- function(i, mdata) {
  dat <- mdata[, c(-1, -i)]
  out <- lm(mdata[[1]] ~ ., data = dat) %>%
    summary() %>%
    `$`(r.squared)
  return(out)
}

corsign <- function(data) {
  d <- data %>% sign()
  return(d)
}

corout <- function(model, r2) {
      mdata <- cordata(model)
  cor_mdata <- cmdata(mdata)
         r1 <- summary(model)$r.squared
          n <- ncol(mdata)
      ksign <- corsign(cor_mdata)
         n2 <- n - 1
      parts <- ksign * sqrt(r1 - r2)
   partials <- parts / sqrt(1 - r2)
     result <- data.frame(cor_mdata, partials, parts) %>%
      round(3)
   rownames(result) <- names(ksign)
   colnames(result) <- c('Zero-order', 'Partial', 'Part')

  return(result)

}

corm2 <- function(model) {
  mdata <- cordata(model)
      n <- ncol(mdata)
     r2 <- c()

  for (i in 2:n) {
    out   <- rtwo(i, mdata)
    r2    <- c(r2, out)
  }

  return(r2)
}


# deleted studentized residu vs Predicted
dpred <- function(model) {

       pred <- model %>% fitted()
  	dsresid <- model %>% rstudent() %>% unname()
  	      n <- length(dsresid)
	       ds <- tibble(obs = seq_len(n), dsr = dsresid)
	       ds <- ds %>%
           mutate(color = ifelse((abs(dsr) >= 2), "outlier", "normal"))
  ds$color1 <- factor(ds$color)
	ds$color2 <- ordered(dr$color1, levels = c("normal", "outlier"))
	       d  <- tibble(pred = pred, dsr = ds$dsr, Observation = ds$color2)

    	 minx <- min(ds$dsr) - 1
      cminx <- ifelse(minx < -2, minx, -2.5)
    	 maxx <- max(ds$dsr) + 1
    	cmaxx <- ifelse(maxx > 2, maxx, 2.5)

        out <- list(ds = ds, cminx = cminx, cmaxx = cmaxx)
      return(out)
}

# f test
frhs <- function(nam, model) {
         np <- length(nam)
  var_resid <- sum(residuals(model) ^ 2) / n
        ind <- residuals(model) ^ 2 / var_resid - 1
          l <- cbind(l, ind)
      mdata <- l[-1]
     model1 <- lm(ind ~ ., data = mdata)
          k <- summary(model1)
    return(k$fstatistic)
}

fvar <- function(n, l, model, vars) {
  var_resid <- sum(residuals(model) ^ 2) / n
        ind <- residuals(model) ^ 2 / var_resid - 1
      mdata <- l[-1]
         dl <- mdata[, vars]
         dk <- as.data.frame(cbind(ind, dl))
         nd <- ncol(dk) - 1
     model1 <- lm(ind ~ ., data = dk)
          k <- summary(model1)
  return(k$fstatistic)
}

ffit <- function(model) {
          pred <- model$fitted.values
         resid <- model$residuals ^ 2
     avg_resid <- sum(resid) / length(pred)
  scaled_resid <- resid / avg_resid
        model1 <- lm(scaled_resid ~ pred)
             k <- summary(model1)
  return(k$fstatistic)
}
