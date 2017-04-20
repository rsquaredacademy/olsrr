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
  fla <- as.formula(paste0('`', k[j], '`', " ~ ."))
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

               y <- x
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
   fm <- as.formula(paste0('`', nam[i], '` ', "~ ."))
   m1 <- lm(fm, data = data)
  rsq <- 1 - (summary(m1)$r.squared)
  return(rsq)
}

viftol <- function(model) {
    m    <- tibble::as_data_frame(model.matrix(model))[-1]
    nam  <- names(m)
    p    <- length(model$coefficients) - 1
    tol  <- c()

    for (i in seq_len(p)) {
        tol[i]  <- fmrsq(nam, m, i)
    }

    vifs <- 1 / tol

    result <- list(nam = names(m), tol = tol, vifs = vifs)
    return(result)
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
  data   <- tibble::as_data_frame(model.matrix(model))[-1]
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
               ts <- 4 / length(ckd$cd)
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
  d$color <- ifelse(d$ckd >= ts, c("outlier"), c("normal"))
  d$color1 <- factor(d$color)
  d$Observation <- ordered(d$color1, levels = c("normal", "outlier"))
  out <- list(d = d, ts = ts)
  return(out)
}

# correlations
#' @importFrom tibble as_data_frame
#' @importFrom purrr map_df
cordata <- function(model) {
  m1 <- tibble::as_data_frame(model.frame(model))
  m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
   l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
  # d <- model %>%
  #   model.frame() %>%
  #   as_data_frame() %>%
  #   map_df(as.numeric)
  return(l)
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
    `[[`(8)
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
        dsr <- NULL
	       ds <- tibble(obs = seq_len(n), dsr = dsresid)
	       ds <- ds %>%
           mutate(color = ifelse((abs(dsr) >= 2), "outlier", "normal"))
  ds$color1 <- factor(ds$color)
	ds$color2 <- ordered(ds$color1, levels = c("normal", "outlier"))
	      ds2  <- tibble(obs = seq_len(n), pred = pred, dsr = ds$dsr, Observation = ds$color2)

    	 minx <- min(ds2$dsr) - 1
      cminx <- ifelse(minx < -2, minx, -2.5)
    	 maxx <- max(ds2$dsr) + 1
    	cmaxx <- ifelse(maxx > 2, maxx, 2.5)

        out <- list(ds = ds2, cminx = cminx, cmaxx = cmaxx)
      return(out)
}

# f test
frhs <- function(nam, model, n, l) {
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


hadipot <- function(model) {
  lev       <- ols_leverage(model)
  pii       <- 1 - lev
  potential <- lev / pii
  return(potential)
}

hadires <- function(model) {
  pii       <- 1 - ols_leverage(model)
  q         <- model$rank 
  p         <- q - 1
  aov_m     <- anova(model)
  j         <- length(aov_m$Df)
  dii       <- (model$residuals / sqrt(aov_m[j, 2])) ^ 2
  residual  <- ((p + 1) / pii) * (dii / (1 - dii))
  return(residual)
}


# influence measures
q1 <- function(full_model, r) {
    out <- full_model %>%
        anova() %>%
        `[[`(3) %>%
        `[`(r)
    return(out)
}

q2 <- function(model, p) {
    out <- model %>%
        anova() %>%
        `[[`(2) %>%
        `[`(p)
    return(out)
}

sbicout <- function(model, n, p, q) {
    a <- (2 * (p + 2) * q)
    b <- (2 * (q ^ 2))
    out <- model %>%
        residuals() %>%
        `^`(2) %>%
        sum() %>%
        `/`(n) %>%
        log() %>%
        `*`(n) %>%
        `+`(a) %>%
        `-`(b)
    return(out)
}

mcpout <- function(model, fullmodel, n, p, q) {
    sse <- model %>%
        residuals() %>%
        `^`(2) %>%
        sum()

    mse <- fullmodel %>%
        anova() %>%
        `[[`(3) %>%
        `[`(q)

    sec <- (n - (2 * p))

    out <- sse %>%
        `/`(mse) %>%
        `-`(sec) %>%
        round(4)

    return(out)
}


sepout <- function(model) {

    n <- model %>% model.frame() %>% nrow()
    # p <- model %>% coefficients() %>% length()
    p <- model %>% anova() %>% `[[`(1) %>% length()

    mse <- model %>%
        anova() %>%
        `[[`(3) %>%
        `[`(p)

    num <- (n + 1) %>%
        `*`(n - 2) %>%
        `*`(mse)

    den <- n * (n - p -1)

    out <- num %>%
        `/`(den) %>%
        round(5)

    return(out)
}

jpout <- function(model) {

    n <- model %>% model.frame() %>% nrow()
    # p <- model %>% coefficients() %>% length()
    p <- model %>% anova() %>% `[[`(1) %>% length()

    mse <- model %>%
        anova() %>%
        `[[`(3) %>%
        `[`(p)

    out <- (n + p) %>%
        `/`(n) %>%
        `*`(mse) %>%
        round(5)

    return(out)
}


pcout <- function(model) {

    n <- model %>% model.frame() %>% nrow()
    # p <- model %>% coefficients() %>% length()
    p <- model %>% anova() %>% `[[`(1) %>% length()

    rse <- model %>%
        summary() %>%
        `[[`(8)

    out <- (n + p) %>%
        `/`(n - p) %>%
        `*`(1 - rse) %>%
        round(5)

    return(out)
}

spout <- function(model) {

    n <- model %>% model.frame() %>% nrow()
    # p <- model %>% coefficients() %>% length()
    p <- model %>% anova() %>% `[[`(1) %>% length()

    mse <- model %>%
        anova() %>%
        `[[`(3) %>%
        `[`(p)

    out <- mse %>%
        `/`(n - p - 1) %>%
        round(5)

    return(out)
}


pc2out <- function(model) {

    n <- model %>% model.frame() %>% nrow()
    p <- model %>% coefficients() %>% length()

    sse <- model %>%
        residuals() %>%
        `^`(2) %>%
        sum()

    sst <- model %>%
        anova() %>%
        `[[`(2) %>%
        sum()

    out <- (n + p) %>%
        `/`(n * (n - p)) %>%
        `*`(sse) %>%
        `/`(sst) %>%
        round(5)

    return(out)
}

ka <- function(k, stderr, n) {
  out <- stderr * qnorm((k - 0.375) / (n + 0.25))
  return(out)
}


corrout <- function(model) {
    n <- model %>% model.frame() %>% nrow()
    stderr <- model %>% summary() %>% `[[`(6)
    h1 <- n %>% seq_len()
    h <- ka(h1, stderr, n)
    out <- model %>% residuals() %>% sort()
    result <- cor(h, out)
    return(result)
}

# observed vs predicted plot
obspred <- function(model) {
    y <- model %>% fitted.values()
    x <- model %>% model.frame() %>% `[[`(1)
    d <- tibble(x, y)
    return(d)
}

# hadi plot
hadio <- function(model, n) {
    model %>% ols_hadi() %>% `[[`(n)
}

# residual fit spread plot
rsdata <- function(model) {
        y <- model %>% residuals()
residtile <- y %>% ecdf()
        x <- y %>% residtile
        d <- tibble(x, y)
  return(d)
}

fmdata <- function(model){
  predicted <- model %>% fitted.values()
     pred_m <- predicted %>% mean()
          y <- predicted - pred_m
   percenti <- y %>% ecdf()
          x <- y %>% percenti
          d <- tibble(x, y)
  return(d)
}

# residual vs predicted plot
rvspdata <- function(model){
      resid <- residuals(model)
  predicted <- fitted(model)
          d <- tibble(predicted = predicted, resid = resid)
  return(d)
}

# residual vs regressor plot
rvsrdata <- function(model){
      np <- length(model$coefficients) - 1
	   dat <- model.frame(model)[-1]
	pnames <- names(model$coefficients)[-1]
  result <- list(np = np, dat = dat, pnames = pnames)
  return(result)
}

# score test
rhsout <- function(model) {
          # l <- model.frame(model)
         m1 <- tibble::as_data_frame(model.frame(model))
         m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
          l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
          n <- nrow(l)
        nam <- names(l)[-1]
         np <- length(nam)
  var_resid <- sum(residuals(model) ^ 2) / n
        ind <- residuals(model) ^ 2 / var_resid - 1
          l <- cbind(l, ind)
      mdata <- l[-1]
     model1 <- lm(ind ~ ., data = mdata)
      score <- summary(model1)$r.squared * n
          p <- pchisq(score, np, lower.tail = F)
      preds <- nam
     result <- list(score = score, p = p, np = np, preds = preds)
 return(result)
}

fitout <- function(model, resp) {

             # l <- model.frame(model)
            m1 <- tibble::as_data_frame(model.frame(model))
            m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
             l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
             n <- nrow(l)
          pred <- model$fitted.values
         resid <- model$residuals ^ 2
     avg_resid <- sum(resid) / length(pred)
  scaled_resid <- resid / avg_resid
        model1 <- lm(scaled_resid ~ pred)
         score <- summary(model1)$r.squared * n
            np <- 1
             p <- pchisq(score, 1, lower.tail = F)
         preds <- paste('fitted values of', resp)
        result <- list(score = score, p = p, np = np, preds = preds)
  return(result)
}

varout <- function(model, vars) {
          # l <- model.frame(model)
         m1 <- tibble::as_data_frame(model.frame(model))
         m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
          l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
          n <- nrow(l)
  var_resid <- sum(residuals(model) ^ 2) / n
  ind       <- residuals(model) ^ 2 / var_resid - 1
  mdata     <- l[-1]
  dl        <- mdata[, vars]
  dk        <- as.data.frame(cbind(ind, dl))
  nd        <- ncol(dk) - 1
  model1    <- lm(ind ~ ., data = dk)
  score     <- summary(model1)$r.squared * n
  p         <- pchisq(score, nd, lower.tail = F)
  np        <- nd
  preds     <- vars
     result <- list(score = score, p = p, np = np, preds = preds)
  return(result)

}


# studentized residual plot
srdata <- function(model) {

  dstud <- unname(rstudent(model))
	n     <- length(dstud)
	dsr   <- tibble(obs = seq_len(n), dsr = dstud)
	dsr <- dsr %>%
        mutate(color = ifelse((abs(dsr) >= 3), "outlier", "normal"))

	dsr$color1 <- factor(dsr$color)
	dsr$Observation <- ordered(dsr$color1, levels = c("normal", "outlier"))
  cminx <- dsr$dsr %>% min() %>% `-`(1) %>% floor()
  cmaxx <- dsr$dsr %>% max() %>% `-`(1) %>% floor()
	nseq  <- seq_len(abs(0 + cminx + 1)) * -1
	pseq  <- seq_len(0 + cmaxx - 1)

  result <- list(dsr = dsr, cminx = cminx, cmaxx = cmaxx, nseq = nseq, pseq = pseq)
  return(result)
}


# residual Histogram
histdata <- function(model) {
  resid <- residuals(model)
	minx  <- min(resid) - 1
	maxx  <- max(resid) + 1
  result <- list(resid = resid, minx = minx, maxx = maxx)
  return(result)
}

histn <- function(resid, h) {
  xfit  <- seq(min(resid), max(resid), length = 80)
	yfit  <- dnorm(xfit, mean = mean(resid), sd = sd(resid))
	yfit1  <- yfit * diff(h$mids[1:2]) * length(resid)
  result <- list(xfit = xfit, yfit = yfit1)
  return(result)
}

# pure error anova
#' @importFrom dplyr arrange
peanova <- function(model) {

  data      <- model.frame(model)
	dep       <- data[[1]]
	pred      <- data[[2]]
	n         <- nrow(data)
	nam       <- names(data)
	dep_name  <- nam[1]
	pred_name <- nam[2]
	yhat      <- model$fitted.values
	pred_u    <- table(pred)
	nd        <- length(pred_u)

	mean_pred <- data %>%
	    group_by_(pred_name) %>%
	    select_(dep_name, pred_name) %>%
	    summarise_each(funs(mean))

	mean_rep        <- rep(mean_pred[[2]], as.vector(pred_u))
	fin             <- data.frame(dep, yhat, pred)
	finl            <- arrange(fin, pred)
	final           <- cbind(finl, mean_rep)
	colnames(final) <- c("y", "yhat", "pred", "ybar")

	final$lfit <- (final$ybar - final$yhat) ^ 2
	final$rerror <- (final$y - final$ybar) ^ 2
	# final <- mutate(final,
	#   lfit   = (ybar - yhat) ^ 2,
	#   rerror = (y - ybar) ^ 2
	# )

	lackoffit    <- sum(final$lfit)
	random_error <- sum(final$rerror)
	rss          <- anova(model)[1, 2]
	ess          <- sum(lackoffit, random_error)
	total        <- sum(ess, rss)
	df_rss       <- 1
	df_lof       <- nd - 2
	df_error     <- n - nd
	df_ess       <- sum(df_lof, df_error)
	rms          <- rss / df_rss
	ems          <- ess / df_ess
	lms          <- lackoffit / df_lof
	pms          <- random_error / df_error
	rf           <- rms / pms
	lf           <- lms / pms
	pr           <- pf(rf, df_rss, df_ess, lower.tail = F)
	pl           <- pf(lf, df_lof, df_error, lower.tail = F)

  result <- list(lackoffit  = round(lackoffit, 2),
		             pure_error = round(random_error, 2),
		             rss        = round(rss, 2),
		             ess        = round(ess, 2),
		             total      = round(total, 2),
		             rms        = round(rms, 2),
		             ems        = round(ems, 2),
		             lms        = round(lms, 2),
		             pms        = round(pms, 2),
		             rf         = round(rf, 2),
		             lf         = round(lf, 2),
		             pr         = round(pr, 2),
		             pl         = round(pl, 2),
		             mpred      = round(mean_pred, 2),
		             df_rss     = df_rss,
		             df_ess     = df_ess,
		             df_lof     = df_lof,
		             df_error   = df_error,
		             final      = final,
		             resp       = dep_name,
		             preds      = pred_name)
  return(result)
}


# studentized residuals vs leverage plot
rstudlev <- function(model) {

  leverage  <- unname(hatvalues(model))
  rstudent  <- unname(rstudent(model))
  k         <- length(model$coefficients)
  n         <- nrow(model.frame(model))
  lev_thrsh <- ((2 * k) + 2) / n
  rst_thrsh <- 2
  miny      <- min(rstudent) - 3
  maxy      <- max(rstudent) + 3
  minx      <- min(leverage)
  maxx      <- ifelse((max(leverage) > lev_thrsh), max(leverage), (lev_thrsh + 0.05))
  levrstud  <- data.frame(obs = seq_len(n), leverage, rstudent)


  levrstud$color[(leverage < lev_thrsh & abs(rstudent) < 2)] <- "normal"
  levrstud$color[(leverage > lev_thrsh & abs(rstudent) < 2)] <- "leverage"
  levrstud$color[(leverage < lev_thrsh & abs(rstudent) > 2)] <- "outlier"
  levrstud$color[(leverage > lev_thrsh & abs(rstudent) > 2)] <- "outlier & leverage"
  levrstud$color3 <- factor(levrstud$color)
  levrstud$Observation <- ordered(levrstud$color3,
   	                           levels = c("normal", "leverage", "outlier",
   	                                      "outlier & leverage"))

  result <- list(levrstud = levrstud, lev_thrsh = lev_thrsh, minx = minx,
    miny = miny, maxx = maxx, maxy = maxy)
  return(result)
}

# model selection data
mod_sel_data <- function(model) {
    mf <- model.frame(model)
    nf <- mf[[1]]
    nam <- names(mf)
    mtrix <- model.matrix(model)[, -1]
    d <- as.data.frame(cbind(nf, mtrix))
    colnames(d)[1] <- nam[1]
    return(d)
}
