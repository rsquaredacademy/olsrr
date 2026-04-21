# one sample t test
xpl_os_t_test <- function(data, x, mu = 0, alpha = 0.05,
                                    alternative = c("both", "less", "greater", "all"), ...) {

  xone <- data[[x]]

  if (!is.numeric(xone)) {
    stop("x must be numeric")
  }
  if (!is.numeric(mu)) {
    stop("mu must be numeric")
  }
  if (!is.numeric(alpha)) {
    stop("alpha must be numeric")
  }

  type <- match.arg(alternative)
  var_name <- x
  k <- ttest_comp(xone, mu, alpha, type)

  result <-
    list(conf        = k$conf,
         confint     = k$confint,
         df          = k$df,
         Mean        = k$Mean,
         mean_diff   = k$mean_diff,
         mean_diff_l = k$mean_diff_l,
         mean_diff_u = k$mean_diff_u,
         mu          = k$mu,
         n           = k$n,
         p           = k$p,
         p_l         = k$p_l,
         p_u         = k$p_u,
         stddev      = k$stddev,
         std_err     = k$std_err,
         test_stat   = k$test_stat,
         type        = type,
         var_name    = var_name)

  print_ttest(result)

}

ttest_comp <- function(x, mu, alpha, type) {

  n         <- length(x)
  a         <- (alpha / 2)
  df        <- n - 1
  conf      <- 1 - alpha
  Mean      <- round(mean(x), 4)
  stddev    <- round(stats::sd(x), 4)
  std_err   <- round(stddev / sqrt(n), 4)
  test_stat <- round((Mean - mu) / std_err, 3)

  if (type == "less") {
    cint <- c(-Inf, test_stat + stats::qt(1 - alpha, df))
  } else if (type == "greater") {
    cint <- c(test_stat - stats::qt(1 - alpha, df), Inf)
  } else {
    cint <- stats::qt(1 - a, df)
    cint <- test_stat + c(-cint, cint)
  }

  confint     <- round(mu + cint * std_err, 4)
  mean_diff   <- round((Mean - mu), 4)
  mean_diff_l <- confint[1] - mu
  mean_diff_u <- confint[2] - mu
  p_l         <- stats::pt(test_stat, df)
  p_u         <- stats::pt(test_stat, df, lower.tail = FALSE)

  if (p_l < 0.5) {
    p <- p_l * 2
  } else {
    p <- p_u * 2
  }


  out <-
    list(conf        = conf,
         confint     = confint,
         df          = df,
         Mean        = Mean,
         mean_diff   = mean_diff,
         mean_diff_l = mean_diff_l,
         mean_diff_u = mean_diff_u,
         mu          = mu,
         n           = n,
         p           = p,
         p_l         = p_l,
         p_u         = p_u,
         stddev      = stddev,
         std_err     = std_err,
         test_stat   = test_stat)

  return(out)
}

# one way anova
xpl_oneway_anova <- function(data, x, y, ...) {

  fdata        <- data[c(x, y)]
  sample_mean  <- anova_avg(fdata, x)
  sample_stats <- anova_split(fdata, x, y, sample_mean)
  k            <- anova_calc(fdata, sample_stats, x, y)

  result <-
    list(
      adjusted_r2 = round(k$reg$adj.r.squared, 4),
      df_btw      = k$df_sstr,
      df_total    = k$df_sst,
      df_within   = k$df_sse,
      fstat       = k$f,
      group_stats = sample_stats[, c(1, 2, 3, 5)],
      ms_btw      = k$mstr,
      ms_within   = k$mse,
      obs         = k$obs,
      pval        = k$sig,
      r2          = round(k$reg$r.squared, 4),
      rmse        = round(k$reg$sigma, 4),
      ss_between  = k$sstr,
      ss_total    = k$total,
      ss_within   = k$ssee)

  print_owanova(result)
}

anova_split <- function(data, x, y, sample_mean) {

  dat <- data[c(y, x)]
  dm  <- data.table(dat)

  by_factor <- dm[, .(length = length(get(x)),
                     mean = mean(get(x)),
                     var = stats::var(get(x)),
                     sd = stats::sd(get(x))),
                  by = y]

  by_factor[, ':='(sst = length * ((mean - sample_mean) ^ 2),
                   sse = (length - 1) * var)]

  setDF(by_factor)
  by_factor <- by_factor[order(by_factor[, 1]),]

  return(by_factor)
}

anova_avg <- function(data, y) {

  mean(data[[y]])

}

anova_calc <- function(data, sample_stats, x, y) {

  var_names <- names(data[c(x, y)])

  sstr <-
    sample_stats %>%
    magrittr::use_series(sst) %>%
    sum() %>%
    round(3)

  ssee <-
    sample_stats %>%
    magrittr::use_series(sse) %>%
    sum() %>%
    round(3)

  total   <- round(sstr + ssee, 3)
  df_sstr <- nrow(sample_stats) - 1
  df_sse  <- nrow(data) - nrow(sample_stats)
  df_sst  <- nrow(data) - 1
  mstr    <- round(sstr / df_sstr, 3)
  mse     <- round(ssee / df_sse, 3)
  f       <- round(mstr / mse, 3)
  sig     <- round(1 - stats::pf(f, df_sstr, df_sse), 3)
  obs     <- nrow(data)
  regs    <- paste(var_names[1], "~ as.factor(", var_names[2], ")")
  model   <- stats::lm(stats::as.formula(regs), data = data)
  reg     <- summary(model)

  out <- list(
    sstr = sstr, ssee = ssee, total = total, df_sstr = df_sstr,
    df_sse = df_sse, df_sst = df_sst, mstr = mstr, mse = mse, f = f,
    sig = sig, obs = obs, model = model, reg = reg
  )

  return(out)
}

# chi square association test
xpl_chisq_assoc_test <- function(data, x, y) {

  xone <- data[[x]]
  yone <- data[[y]]

  if (!is.factor(xone)) {
    stop("x must be a categorical variable")
  }

  if (!is.factor(yone)) {
    stop("y must be a categorical variable")
  }

  # dimensions
  k  <- table(xone, yone)
  dk <- dim(k)
  ds <- prod(dk)
  nr <- dk[1]
  nc <- dk[2]


  if (ds == 4) {
    twoway <- matrix(table(xone, yone), nrow = 2)
    df <- df_chi(twoway)
    ef <- efmat(twoway)
    k  <- pear_chsq(twoway, df, ef)
    m  <- lr_chsq(twoway, df, ef)
    n  <- yates_chsq(twoway)
    p  <- mh_chsq(twoway, n$total, n$prod_totals)
  } else {
    twoway <- matrix(table(xone, yone), nrow = dk[1])
    ef <- efm(twoway, dk)
    df <- df_chi(twoway)
    k  <- pear_chi(twoway, df, ef)
    m  <- lr_chsq2(twoway, df, ef, ds)
  }

  j <- chigf(xone, yone, k$chi)

  result <- if (ds == 4) {
    list(
      chisquare                      = k$chi,
      chisquare_adjusted             = n$chi_y,
      chisquare_lr                   = m$chilr,
      chisquare_mantel_haenszel      = p$chimh,
      contingency_coefficient        = j$cc,
      cramers_v                      = j$cv,
      df                             = df,
      ds                             = ds,
      phi_coefficient                = j$phi,
      pval_chisquare                 = k$sig,
      pval_chisquare_adjusted        = n$sig_y,
      pval_chisquare_lr              = m$sig_lr,
      pval_chisquare_mantel_haenszel = p$sig_mh
    )
  } else {
    list(
      chisquare               = k$chi,
      chisquare_lr            = m$chilr,
      contingency_coefficient = j$cc,
      cramers_v               = j$cv,
      df                      = df,
      ds                      = ds,
      phi_coefficient         = j$phi,
      pval_chisquare          = k$sig,
      pval_chisquare_lr       = m$sig_lr
    )
  }

  print_chisq_test(result)
}


df_chi <- function(twoway) {
  (nrow(twoway) - 1) * (ncol(twoway) - 1)
}

efmat <- function(twoway) {
  mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = 2)
  mat2 <- matrix(colSums(twoway), nrow = 1)

  mat1 %*% mat2
}

pear_chsq <- function(twoway, df, ef) {
  chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
  sig <- round(stats::pchisq(chi, df, lower.tail = F), 4)

  list(chi = chi, sig = sig)
}

lr_chsq <- function(twoway, df, ef) {
  chilr  <- round(2 * sum(matrix(log(twoway / ef), nrow = 1) %*% matrix(twoway, nrow = 4)), 4)
  sig_lr <- round(stats::pchisq(chilr, df, lower.tail = F), 4)

  list(chilr = chilr, sig_lr = sig_lr)
}

lr_chsq2 <- function(twoway, df, ef, ds) {
  chilr  <- round(2 * sum(matrix(twoway, ncol = ds) %*% matrix(log(twoway / ef), nrow = ds)), 4)
  sig_lr <- round(stats::pchisq(chilr, df, lower.tail = F), 4)

  list(chilr = chilr, sig_lr = sig_lr)
}

yates_chsq <- function(twoway) {
  way2        <- twoway[, c(2, 1)]
  total       <- sum(twoway)
  prods       <- prod(diag(twoway)) - prod(diag(way2))
  prod_totals <- prod(rowSums(twoway)) * prod(colSums(twoway))
  chi_y       <- round((total * (abs(prods) - (total / 2)) ^ 2) / prod_totals, 4)
  sig_y       <- round(stats::pchisq(chi_y, 1, lower.tail = F), 4)

  list(chi_y = chi_y, sig_y = sig_y, total = total, prod_totals = prod_totals)
}

mh_chsq <- function(twoway, total, prod_totals) {
  num    <- twoway[1] - ((rowSums(twoway)[1] * colSums(twoway)[1]) / total)
  den    <- prod_totals / ((total ^ 3) - (total ^ 2))
  chimh  <- round((num ^ 2) / den, 4)
  sig_mh <- round(stats::pchisq(chimh, 1, lower.tail = F), 4)

  list(chimh = chimh, sig_mh = sig_mh)
}

efm <- function(twoway, dk) {
  mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = dk[1])
  mat2 <- matrix(colSums(twoway), ncol = dk[2])

  mat1 %*% mat2
}

pear_chi <- function(twoway, df, ef) {
  chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
  sig <- round(stats::pchisq(chi, df, lower.tail = F), 4)

  list(chi = chi, sig = sig)
}

chigf <- function(x, y, chi) {
  twoway <- matrix(table(x, y),
    nrow = nlevels(as.factor(x)),
    ncol = nlevels(as.factor(y))
  )
  total <- sum(twoway)
  phi   <- round(sqrt(chi / total), 4)
  cc    <- round(sqrt(chi / (chi + total)), 4)
  q     <- min(nrow(twoway), ncol(twoway))
  cv    <- round(sqrt(chi / (total * (q - 1))), 4)

  list(phi = phi, cc = cc, cv = cv)
}

# chi square goodness of fit test
xpl_chisq_gof_test <- function(data, x, y, correct = FALSE) {

  xcheck <- data[[x]]
  xlen   <- length(data[[x]])
  xone   <- as.vector(table(data[[x]]))

  if (!is.factor(xcheck)) {
    stop("x must be an object of class factor")
  }

  if (!is.numeric(y)) {
    stop("y must be numeric")
  }

  if (!is.logical(correct)) {
    stop("correct must be either TRUE or FALSE")
  }

  varname <- names(data[x])
  n <- length(xone)

  if (length(y) != n) {
    stop("Length of y must be equal to the number of categories in x")
  }

  df <- n - 1

  if (sum(y) == 1) {
    y <- xlen * y
  }

  if ((df == 1) || (correct == TRUE)) {
    k <- chi_cort(xone, y)
  } else {
    k <- chigof(xone, y)
  }

  sig <- round(stats::pchisq(k$chi, df, lower.tail = FALSE), 4)

  result <-
    list(
      categories         = levels(xcheck),
      chisquare          = k$chi,
      deviation          = format(k$dev, nsmall = 2),
      degrees_of_freedom = df,
      expected_frequency = y,
      n_levels           = nlevels(xcheck),
      observed_frequency = xone,
      pvalue             = sig,
      sample_size        = length(xcheck),
      std_residuals      = format(k$std, nsmall = 2),
      varname            = varname
  )

  print_chisq_gof(result)
}

chi_cort <- function(x, y) {

  diff <- x - y - 0.5
  dif  <- abs(x - y) - 0.5
  dif2 <- dif ^ 2
  dev  <- round((diff / y) * 100, 2)
  std  <- round(diff / sqrt(y), 2)
  chi  <- round(sum(dif2 / y), 4)

  list(dev = dev, std = std, chi = chi)
}

chigof <- function(x, y) {

  dif  <- x - y
  dif2 <- dif ^ 2
  dev  <- round((dif / y) * 100, 2)
  std  <- round(dif / sqrt(y), 2)
  chi  <- round(sum(dif2 / y), 4)

  list(dev = dev, std = std, chi = chi)
}

# cochran's q test
xpl_cochran_qtest <- function(data, vars) {

  fdata <- data[vars]

  if (ncol(fdata) < 3) {
    stop("Please specify at least 3 variables.")
  }

  if (any(sapply(lapply(fdata, as.factor), nlevels) > 2)) {
    stop("Please specify dichotomous/binary variables only.")
  }

  k <- cochran_comp(fdata)

  result <-
    list(
      df     = k$df,
      n      = k$n,
      pvalue = k$pvalue,
      q      = k$q)

  print_cochran_test(result)
}

coch_data <- function(x, ...) {

  if (is.data.frame(x)) {
    data <- x %>%
      lapply(as.numeric) %>%
      as.data.frame() %>%
      `-`(1)
  } else {
    data <- cbind(x, ...) %>%
      apply(2, as.numeric) %>%
      `-`(1) %>%
      as.data.frame()
  }

  return(data)
}

cochran_comp <- function(data) {

  n  <- nrow(data)
  k  <- ncol(data)
  df <- k - 1

  cs <-
    data %>%
    lapply(as.numeric) %>%
    as.data.frame() %>%
    magrittr::subtract(1) %>%
    sums()

  q <- coch(k, cs$cls_sum, cs$cl, cs$g, cs$gs_sum)

  pvalue <- 1 - stats::pchisq(q, df)

  list(
    df = df,
    n = n,
    pvalue = round(pvalue, 4),
    q = q)

}

sums <- function(data) {

  cl      <- colSums(data)
  cls_sum <- sum(cl ^ 2)
  g       <- rowSums(data)
  gs_sum  <- sum(g ^ 2)

  list(
    cl      = cl,
    cls_sum = cls_sum,
    g       = g,
    gs_sum  = gs_sum)

}

coch <- function(k, cls_sum, cl, g, gs_sum) {
  ((k - 1) * ((k * cls_sum) - (sum(cl) ^ 2))) / ((k * sum(g)) - gs_sum)
}

# independent sample t test
xpl_ts_ind_ttest <- function(data, x, y, confint = 0.95,
                                       alternative = c("both", "less", "greater", "all"), ...) {

  yone <- names(data[y])

  if (check_x(data, x)) {
    stop("x must be a binary factor variable", call. = FALSE)
  }

  if (check_level(data, x) > 2) {
    stop("x must be a binary factor variable", call. = FALSE)
  }

  method   <- match.arg(alternative)
  var_y    <- yone
  alpha    <- 1 - confint
  a        <- alpha / 2
  h        <- indth(data, x, y, a)
  grp_stat <- h
  g_stat   <- as.matrix(h)
  comb     <- indcomb(data, y, a)
  k        <- indcomp(grp_stat, alpha)
  j        <- indsig(k$n1, k$n2, k$s1, k$s2, k$mean_diff)
  m        <- indpool(k$n1, k$n2, k$mean_diff, k$se_dif)

  result <- list(alternative      = method,
                 combined         = comb,
                 confint          = confint,
                 conf_diff        = round(k$conf_diff, 5),
                 den_df           = k$n2 - 1,
                 df_pooled        = m$df_pooled,
                 df_satterthwaite = j$d_f,
                 f                = round(k$s1 / k$s2, 4),
                 f_sig            = fsig(k$s1, k$s2, k$n1, k$n2),
                 levels           = g_stat[, 1],
                 lower            = g_stat[, 8],
                 mean             = g_stat[, 3],
                 mean_diff        = round(k$mean_diff, 3),
                 n                = k$n,
                 num_df           = k$n1 - 1,
                 obs              = g_stat[, 2],
                 sd               = g_stat[, 4],
                 sd_dif           = round(k$sd_dif, 3),
                 se               = g_stat[, 5],
                 se_dif           = round(k$se_dif, 3),
                 sig              = j$sig,
                 sig_l            = j$sig_l,
                 sig_pooled_l     = m$sig_pooled_l,
                 sig_pooled_u     = m$sig_pooled_u,
                 sig_pooled       = m$sig_pooled,
                 sig_u            = j$sig_u,
                 t_pooled         = round(m$t_pooled, 4),
                 t_satterthwaite  = round(j$t, 4),
                 upper            = g_stat[, 9],
                 var_y            = var_y)

  print_two_ttest(result)

}

indth <- function(data, x, y, a) {

  h       <- data_split(data, x, y)
  h$df    <- h$length - 1
  h$error <- stats::qt(a, h$df) * -1
  h$lower <- h$mean_t - (h$error * h$std_err)
  h$upper <- h$mean_t + (h$error * h$std_err)

  return(h)
}

data_split <- function(data, x, y) {

  dat <- data.table(data[c(x, y)])
  out <- dat[, .(length = length(get(y)),
                 mean_t = mean_t(get(y)),
                 sd_t = sd_t(get(y)),
                 std_err = std_err(get(y))),
            by = x]

  setDF(out)

}

indcomb <- function(data, y, a) {

  comb        <- da(data, y)
  comb$df     <- comb$length - 1
  comb$error  <- stats::qt(a, comb$df) * -1
  comb$lower  <- round(comb$mean_t - (comb$error * comb$std_err), 5)
  comb$upper  <- round(comb$mean_t + (comb$error * comb$std_err), 5)
  names(comb) <- NULL

  return(comb)

}

da <- function(data, y) {

  dat <- data[[y]]
  data.frame(length  = length(dat),
             mean_t  = mean_t(dat),
             sd_t    = sd_t(dat),
             std_err = std_err(dat))

}

mean_t <- function(x) {

  x %>%
    mean() %>%
    round(3)

}

sd_t <- function(x) {

  x %>%
    stats::sd() %>%
    round(3)

}

std_err <- function(x) {

  x %>%
    stats::sd() %>%
    divide_by(x %>%
                length() %>%
                sqrt()) %>%
    round(3)

}

indcomp <- function(grp_stat, alpha) {

  n1        <- grp_stat[1, 2]
  n2        <- grp_stat[2, 2]
  n         <- n1 + n2
  means     <- grp_stat[, 3]
  mean_diff <- means[1] - means[2]
  sd1       <- grp_stat[1, 4]
  sd2       <- grp_stat[2, 4]
  s1        <- grp_stat[1, 4] ^ 2
  s2        <- grp_stat[2, 4] ^ 2
  sd_dif    <- sd_diff(n1, n2, s1, s2)
  se_dif    <- se_diff(n1, n2, s1, s2)
  conf_diff <- conf_int_p(mean_diff, se_dif, alpha = alpha)

  list(conf_diff = conf_diff,
       mean_diff = mean_diff,
       n         = n,
       n1        = n1,
       n2        = n2,
       s1        = s1,
       s2        = s2,
       sd1       = sd1,
       sd2       = sd2,
       sd_dif    = sd_dif,
       se_dif    = se_dif)

}

sd_diff <- function(n1, n2, s1, s2) {

  n1 <- n1 - 1
  n2 <- n2 - 1
  n  <- (n1 + n2) - 2

  (n1 * s1) %>%
    add(n2 * s2) %>%
    divide_by(n) %>%
    raise_to_power(0.5)

}

se_diff <- function(n1, n2, s1, s2) {

  df  <- n1 + n2 - 2
  n_1 <- n1 - 1
  n_2 <- n2 - 1

  (n_1 * s1) %>%
    add(n_2 * s2) %>%
    divide_by(df) -> v

  (1 / n1) %>%
    add(1 / n2) %>%
    multiply_by(v) %>%
    sqrt()

}

conf_int_p <- function(u, se, alpha = 0.05) {

  a     <- alpha / 2
  error <- round(stats::qnorm(a), 3) * -1
  lower <- u - (error * se)
  upper <- u + (error * se)
  c(lower, upper)

}

indsig <- function(n1, n2, s1, s2, mean_diff) {

  d_f   <- as.vector(df(n1, n2, s1, s2))
  t     <- mean_diff / (((s1 / n1) + (s2 / n2)) ^ 0.5)
  sig_l <- round(stats::pt(t, d_f), 4)
  sig_u <- round(stats::pt(t, d_f, lower.tail = FALSE), 4)

  if (sig_l < 0.5) {
    sig <- round(stats::pt(t, d_f) * 2, 4)
  } else {
    sig <- round(stats::pt(t, d_f, lower.tail = FALSE) * 2, 4)
  }

  list(d_f   = d_f,
       sig_l = sig_l,
       sig_u = sig_u,
       sig   = sig,
       t     = t)

}

df <- function(n1, n2, s1, s2) {

  sn1 <- s1 / n1
  sn2 <- s2 / n2
  m1  <- 1 / (n1 - 1)
  m2  <- 1 / (n2 - 1)
  num <- (sn1 + sn2) ^ 2
  den <- (m1 * (sn1 ^ 2)) + (m2 * (sn2 ^ 2))

  round(num / den)

}

fsig <- function(s1, s2, n1, n2) {

  round(min(
    stats::pf((s1 / s2), (n1 - 1), (n2 - 1)),
    stats::pf((s1 / s2), (n1 - 1), (n2 - 1),
      lower.tail = FALSE
    )
  ) * 2, 4)

}


indpool <- function(n1, n2, mean_diff, se_dif) {

  df_pooled    <- (n1 + n2) - 2
  t_pooled     <- mean_diff / se_dif
  sig_pooled_l <- round(stats::pt(t_pooled, df_pooled), 4)
  sig_pooled_u <- round(stats::pt(t_pooled, df_pooled, lower.tail = FALSE), 4)

  if (sig_pooled_l < 0.5) {
    sig_pooled <- round(stats::pt(t_pooled, df_pooled) * 2, 4)
  } else {
    sig_pooled <- round(stats::pt(t_pooled, df_pooled, lower.tail = FALSE) * 2, 4)
  }

  list(df_pooled    = df_pooled,
       sig_pooled_l = sig_pooled_l,
       sig_pooled_u = sig_pooled_u,
       sig_pooled   = sig_pooled,
       t_pooled     = t_pooled)

}

check_x <- function(data, x) {

  !is.factor(data[[x]])

}

check_level <- function(data, x) {

  nlevels(data[[x]])

}

# levene test
xpl_levene_test <- function(data, variables = NULL, group_var = "NULL",
                                      trim_mean = 0.1) {

  groupvar  <- group_var
  varyables <- variables
  fdata     <- data[varyables]

  if (groupvar == "NULL") {
    z  <- as.list(fdata)
    ln <- unlist(lapply(z, length))
    ly <- seq_len(length(z))

    if (length(z) < 2) {
      stop("Please specify at least two variables.", call. = FALSE)
    }

    out   <- xpl_gvar(ln, ly)
    fdata <- unlist(z)

    groupvars <-
      out %>%
      unlist() %>%
      as.factor()

  } else {

    fdata <- fdata[[1]]
    groupvars <- data[[groupvar]]

    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }
  }

  k <- lev_comp(fdata, groupvars, trim_mean)

  out <-
    list(avg   = k$avg,
         avgs  = k$avgs,
         bf    = k$bf,
         bft   = k$bft,
         d_df  = k$d_df,
         lens  = k$lens,
         lev   = k$lev,
         levs  = k$levs,
         n     = k$n,
         n_df  = k$n_df,
         p_bf  = k$p_bf,
         p_bft = k$p_bft,
         p_lev = k$p_lev,
         sd    = k$sd,
         sds   = k$sds)

  print_levene_test(out)
}

lev_metric <- function(cvar, gvar, loc, ...) {

  metric <- tapply(cvar, gvar, loc, ...)
  y      <- abs(cvar - metric[gvar])
  result <- stats::anova(stats::lm(y ~ gvar))

  list(
    fstat = result$`F value`[1],
    p     = result$`Pr(>F)`[1]
  )

}

lev_comp <- function(variable, group_var, trim.mean) {

  comp <- stats::complete.cases(variable, group_var)
  n    <- length(comp)
  k    <- nlevels(group_var)

  cvar <- variable[comp]
  gvar <- group_var[comp]

  lens <- tapply(cvar, gvar, length)
  avgs <- tapply(cvar, gvar, mean)
  sds  <- tapply(cvar, gvar, stats::sd)

  bf   <- lev_metric(cvar, gvar, mean)
  lev  <- lev_metric(cvar, gvar, stats::median)
  bft  <- lev_metric(cvar, gvar, mean, trim = trim.mean)

  list(
    avg   = round(mean(cvar), 2),
    avgs  = round(avgs, 2),
    bf    = round(bf$fstat, 4),
    bft   = round(bft$fstat, 4),
    d_df  = (n - k),
    lens  = lens,
    lev   = round(lev$fstat, 4),
    levs  = levels(gvar),
    n     = n,
    n_df  = (k - 1),
    p_bf  = round(bf$p, 4),
    p_bft = round(bft$p, 4),
    p_lev = round(lev$p, 4),
    sd    = round(stats::sd(cvar), 2),
    sds   = round(sds, 2))

}

# mcnemar test
xpl_mcnemar_test <- function(data, x = NULL, y = NULL) {

  if (is.matrix(data) | is.table(data)) {
    dat <- mcdata(data)
  } else {

  dat <- table(data[c(x, y)])

  }

  k <- mccomp(dat)

  result <-
    list(cases     = k$cases,
         controls  = k$controls,
         cpvalue   = k$cpvalue,
         cstat     = k$cstat,
         df        = k$df,
         exactp    = k$exactp,
         kappa     = k$kappa,
         kappa_cil = k$kappa_cil,
         kappa_ciu = k$kappa_ciu,
         odratio   = k$odratio,
         pvalue    = k$pvalue,
         ratio     = k$ratio,
         statistic = k$statistic,
         std_err   = k$std_err,
         tbl       = dat)

  print_mcnemar_test(result)
}

mcdata <- function(x, y) {
  if (!is.matrix(x)) {
    stop("x must be either a table or a matrix")
  }

  if (is.matrix(x)) {
    if (length(x) != 4) {
      stop("x must be a 2 x 2 matrix")
    }
  }

  dat <- x
  return(dat)
}


mctestp <- function(dat) {
  retrieve <- matrix(c(1, 2, 2, 1), nrow = 2)
  dat[retrieve]
}

tetat <- function(p) {
  ((p[1] - p[2]) ^ 2) / sum(p)
}

mcpval <- function(test_stat, df) {
  1 - stats::pchisq(test_stat, df)
}

mcpex <- function(dat) {
  2 * min(stats::pbinom(dat[2], sum(dat[2], dat[3]), 0.5), stats::pbinom(dat[3], sum(dat[2], dat[3]), 0.5))
}

mcstat <- function(p) {
  ((abs(p[1] - p[2]) - 1) ^ 2) / sum(p)
}

mccpval <- function(cstat, df) {
  1 - stats::pchisq(cstat, df)
}

mckappa <- function(dat) {

  agreement <- sum(diag(dat)) / sum(dat)
  expected  <- sum(rowSums(dat) * colSums(dat)) / (sum(dat) ^ 2)
  (agreement - expected) / (1 - expected)

}

mcserr <- function(dat, kappa) {
  expected <- sum(rowSums(dat) * colSums(dat)) / (sum(dat) ^ 2)
  serr(dat, kappa, expected)
}

mcconf <- function(std_err, kappa) {

  alpha    <- 0.05
  interval <- stats::qnorm(1 - (alpha / 2)) * std_err
  ci_lower <- kappa - interval
  ci_upper <- kappa + interval

  list(ci_lower = ci_lower, ci_upper = ci_upper)

}

prop_fact <- function(dat, p) {

  dat_per    <- dat / sum(dat)
  row_sum    <- rowSums(dat_per)
  col_sum    <- colSums(dat_per)
  controls   <- 1 - col_sum[2]
  cases      <- 1 - row_sum[2]
  ratio      <- cases / controls
  odds_ratio <- p[1] / p[2]

  list(cases      = cases,
       controls   = controls,
       odds_ratio = odds_ratio,
       ratio      = ratio

  )

}

serr <- function(dat, kappa, expected) {

  dat_per    <- dat / sum(dat)
  row_sum    <- rowSums(dat_per)
  row_sum[3] <- sum(row_sum)
  col_sum    <- colSums(dat_per)
  dat_per    <- rbind(dat_per, col_sum)
  dat_per    <- cbind(dat_per, row_sum)
  d1         <- dim(dat_per)

  dat_per[d1[1], d1[2]] <- 1.0
  diagonal <- diag(dat_per)

  a <- diagonal[1] * (1 - (row_sum[1] + col_sum[1]) * (1 - kappa)) ^ 2 +
    diagonal[2] * (1 - (row_sum[2] + col_sum[2]) * (1 - kappa)) ^ 2

  x1 <- dat_per[lower.tri(dat_per)][1]
  x2 <- dat_per[upper.tri(dat_per)][1]

  b <- ((1 - kappa) ^ 2) * ((x1 * (row_sum[1] + col_sum[2]) ^ 2) +
    (x2 * (row_sum[2] + col_sum[1]) ^ 2))

  c <- ((kappa) - expected * (1 - kappa)) ^ 2
  variance <- ((a + b - c) / ((1 - expected) ^ 2)) / sum(dat)

  sqrt(variance)
}

mccomp <- function(dat) {

  p         <- mctestp(dat)
  test_stat <- tetat(p)
  df        <- nrow(dat) - 1
  pvalue    <- mcpval(test_stat, df)
  exactp    <- mcpex(dat)
  cstat     <- mcstat(p)
  cpvalue   <- mccpval(cstat, df)
  kappa     <- mckappa(dat)
  std_err   <- mcserr(dat, kappa)
  clu       <- mcconf(std_err, kappa)
  k         <- prop_fact(dat, p)

  list(cases     = round(k$cases, 4),
       controls  = round(k$controls, 4),
       cpvalue   = cpvalue,
       cstat     = cstat,
       df        = df,
       exactp    = round(exactp, 4),
       kappa     = round(kappa, 4),
       kappa_cil = round(clu$ci_lower, 4),
       kappa_ciu = round(clu$ci_upper, 4),
       odratio   = round(k$odds_ratio, 4),
       pvalue    = round(pvalue, 4),
       ratio     = round(k$ratio, 4),
       statistic = round(test_stat, 4),
       std_err   = round(std_err, 4))

}

# one sample proportion test
xpl_os_prop_test <- function(data, variable = NULL, prob = 0.5, phat = 0.5,
                                       alternative = c("both", "less", "greater", "all")) {
  if (is.numeric(data)) {

    method <- match.arg(alternative)
    k <- prop_comp(
      data, prob = prob, phat = phat,
      alternative = method
    )

  } else {

    fdata     <- data[[variable]]
    n1        <- length(fdata)

    n2 <-
      fdata %>%
      table() %>%
      `[[`(2)

    phat   <- round(n2 / n1, 4)
    prob   <- prob
    method <- match.arg(alternative)

    k <- prop_comp(
      n1, prob = prob, phat = phat,
      alternative = method
    )
  }

  result <-
    list(alt       = k$alt,
         deviation = k$deviation,
         exp       = k$exp,
         n         = k$n,
         obs       = k$obs,
         p         = k$p,
         phat      = k$phat,
         sig       = k$sig,
         std       = k$std,
         z         = k$z)

  print_prop_test(result)
}

prop_comp <- function(n, prob, alternative, phat) {

  n    <- n
  phat <- phat
  p    <- prob
  q    <- 1 - p
  obs  <- c(n * (1 - phat), n * phat)
  exp  <- n * c(q, p)
  dif  <- obs - exp
  dev  <- round((dif / exp) * 100, 2)
  std  <- round(dif / sqrt(exp), 2)
  num  <- phat - prob
  den  <- sqrt((p * q) / n)
  z    <- round(num / den, 4)
  lt   <- round(stats::pnorm(z), 4)
  ut   <- round(1 - stats::pnorm(z), 4)
  tt   <- round((1 - stats::pnorm(abs(z))) * 2, 4)
  alt  <- alternative

  if (alt == "all") {
    sig <- c("two-both" = tt, "less" = lt, "greater" = ut)
  } else if (alt == "greater") {
    sig <- ut
  } else if (alt == "less") {
    sig <- lt
  } else {
    sig <- tt
  }

  out <-
    list(alt       = alt,
         deviation = format(dev, nsmall = 2),
         exp       = exp,
         n         = n,
         obs       = obs,
         p         = prob,
         phat      = phat,
         sig       = sig,
         std       = format(std, nsmall = 2),
         z         = z)

  return(out)
}

# one sample variance test
xpl_os_var_test <- function(data, x, sd, confint = 0.95,
                                      alternative = c("both", "less", "greater", "all"), ...) {

  xone <- data[[x]]

  if (!is.numeric(xone)) {
    stop("x must be numeric")
  }

  if (!is.numeric(sd)) {
    stop("sd must be numeric")
  }

  if (!is.numeric(confint)) {
    stop("confint must be numeric")
  }

  type <- match.arg(alternative)
  varname <- names(data[x])
  k <- osvar_comp(xone, sd, confint)

  result <-
    list(chi      = round(k$chi, 4),
         c_lwr    = k$c_lwr,
         conf     = k$conf,
         c_upr    = k$c_upr,
         df       = k$df,
         n        = k$n,
         p_lower  = k$p_lower,
         p_two    = k$p_two,
         p_upper  = k$p_upper,
         sd       = k$sd,
         se       = round(k$se, 4),
         sigma    = round(k$sigma, 4),
         type     = type,
         var_name = varname,
         xbar     = round(k$xbar, 4))

  print_os_vartest(result)
}

osvar_comp <- function(x, sd, confint) {

  n     <- length(x)
  df    <- n - 1
  xbar  <- mean(x)
  sigma <- stats::sd(x)
  se    <- sigma / sqrt(n)
  chi   <- df * ((sigma / sd) ^ 2)

  p_lower <- stats::pchisq(chi, df)
  p_upper <- stats::pchisq(chi, df, lower.tail = F)

  if (p_lower < 0.5) {
    p_two <- stats::pchisq(chi, df) * 2
  } else {
    p_two <- stats::pchisq(chi, df, lower.tail = F) * 2
  }

  conf  <- confint
  a     <- (1 - conf) / 2
  al    <- 1 - a
  tv    <- df * sigma
  c_lwr <- round(tv / stats::qchisq(al, df), 4)
  c_upr <- round(tv / stats::qchisq(a, df), 4)

  list(chi     = chi,
       c_lwr   = c_lwr,
       conf    = conf,
       c_upr   = c_upr,
       df      = df,
       n       = n,
       p_lower = p_lower,
       p_two   = p_two,
       p_upper = p_upper,
       sd      = sd,
       se      = se,
       sigma   = sigma,
       xbar    = xbar)

}

# paired t test
xpl_ts_paired_ttest <- function(data, x, y, confint = 0.95,
                                          alternative = c("both", "less", "greater", "all")) {

  xone <- data[[x]]
  yone <- data[[y]]

  method    <- match.arg(alternative)
  var_names <- names(data[c(x, y)])

  k <- paired_comp(xone, yone, confint, var_names)

  result <- list(
    Obs = k$Obs, b = k$b, conf_int1 = k$conf_int1,
    conf_int2 = k$conf_int2, conf_int_diff = k$conf_int_diff, corr = k$corr,
    corsig = k$corsig, tstat = k$tstat, p_lower = k$p_lower,
    p_upper = k$p_upper, p_two_tail = k$p_two_tail, var_names = var_names,
    xy = k$xy, df = k$df, alternative = method, confint = confint
  )

  print_paired_ttest(result)
}

paired_comp <- function(x, y, confint, var_names) {

  n         <- length(x)
  df        <- (n - 1)
  xy        <- paste(var_names[1], "-", var_names[2])

  data_prep <- paired_data(x, y)
  b         <- paired_stats(data_prep, "key", "value")
  corr      <- round(stats::cor(x, y), 4)
  corsig    <- cor_sig(corr, n)

  alpha     <- 1 - confint
  confint1  <- conf_int_t(b[[1, 1]], b[[1, 2]], n, alpha = alpha) %>% round(2)
  confint2  <- conf_int_t(b[[2, 1]], b[[2, 2]], n, alpha = alpha) %>% round(2)
  confint3  <- conf_int_t(b[[3, 1]], b[[3, 2]], n, alpha = alpha) %>% round(2)

  t         <- round(b[[3, 1]] / b[[3, 3]], 4)

  p_l       <- stats::pt(t, df)
  p_u       <- stats::pt(t, df, lower.tail = FALSE)
  p         <- stats::pt(abs(t), df, lower.tail = FALSE) * 2

  list(
    Obs = n, b = b, conf_int1 = confint1, conf_int2 = confint2,
    conf_int_diff = confint3, corr = round(corr, 2), corsig = round(corsig, 2),
    tstat = t, p_lower = p_l, p_upper = p_u, p_two_tail = p, xy = xy, df = df
  )

}

paired_data <- function(x, y) {

  j <- data.frame(x = x, y = y)
  j$z <- j$x - j$y
  val <- data.frame(value = c(j$x, j$y, j$z))
  key <- rep(c("x", "y", "z"), each = nrow(j))
  cbind(key = key, value = val)

}

paired_stats <- function(data, key, value) {

  dat <- data.table(data[c("value", "key")])

  out <- dat[, .(length = length(value),
                 mean = mean(value),
                 sd = stats::sd(value)),
            by = key]

  out[, ':='(se = sd / sqrt(length))]
  setDF(out)
  out[, c(-1, -2)]

}

cor_sig <- function(corr, n) {

  t   <- corr / ((1 - (corr ^ 2)) / (n - 2)) ^ 0.5
  df  <- n - 2
  sig <- (1 - stats::pt(t, df)) * 2
  round(sig, 4)

}

conf_int_t <- function(u, s, n, alpha = 0.05) {

  a     <- alpha / 2
  df    <- n - 1
  error <- round(stats::qt(a, df), 3) * -1
  lower <- u - (error * samp_err(s, n))
  upper <- u + (error * samp_err(s, n))
  c(lower, upper)

}

samp_err <- function(sigma, n) {
  sigma / (n ^ 0.5)
}

# runs test
xpl_runs_test <- function(data, x, drop = FALSE,
                                    split = FALSE, mean = FALSE,
                                    threshold = NA) {

  xone <- data[[x]]
  n    <- length(xone)

  if (is.na(threshold)) {
    y <- unique(xone)
    if (sum(y) == 1) {
      stop("Use 0 as threshold if the data is coded as a binary.")
    }
  }

  if (!(is.na(threshold))) {
    thresh <- threshold
  } else if (mean) {
    thresh <- mean(xone)
  } else {
    thresh <- stats::median(xone, na.rm = TRUE)
  }

  if (drop) {
    xone <- xone[xone != thresh]
  }

  if (split) {
    x_binary <- ifelse(xone > thresh, 1, 0)
  } else {

    x_binary <-
      xone %>%
      lapply(nruns2, thresh) %>%
      unlist(use.names = FALSE)
  }

  n_runs   <- xpl_nsignC(x_binary)
  n1       <- sum(x_binary)
  n0       <- length(x_binary) - n1
  exp_runs <- expruns(n0, n1)
  sd_runs  <- sdruns(n0, n1)

  test_stat <- (n_runs - exp_runs) / (sd_runs ^ 0.5)
  sig <- 2 * (1 - stats::pnorm(abs(test_stat), lower.tail = TRUE))

  result <-
    list(mean      = exp_runs,
         n         = n,
         n_above   = n1,
         n_below   = n0,
         n_runs    = n_runs,
         p         = sig,
         threshold = thresh,
         var       = sd_runs,
         z         = test_stat)

  print_runs_test(result)
}

# expected runs
expruns <- function(n0, n1) {
  N <- n0 + n1
  return(((2 * n0 * n1) / N) + 1)
}

# standard deviation of runs
sdruns <- function(n0, n1) {
  N <- n0 + n1
  n <- 2 * n0 * n1
  return(((n * (n - N)) / ((N ^ 2) * (N - 1))))
}

nruns2 <- function(data, value) {
  if (data <= value) {
    return(0)
  } else {
    return(1)
  }
}

# two sample proportion test
xpl_ts_prop_test <- function(data, var1, var2,
                                       alternative = c("both", "less", "greater", "all"), ...) {

  varone <- data[[var1]]
  vartwo <- data[[var2]]

  alt    <- match.arg(alternative)
  k      <- prop_comp2(varone, vartwo, alt)

  result <-
    list(alt   = alt,
         n1    = k$n1,
         n2    = k$n2,
         phat1 = k$phat1,
         phat2 = k$phat2,
         sig   = k$sig,
         z     = k$z)

  print_ts_prop_test(result)
}

xpl_ts_prop_grp <- function(data, var, group,
                              alternative = c("both", "less", "greater", "all")) {

  varone   <- data[[var]]
  groupone <- data[[group]]

  if (nlevels(groupone) > 2) {
    stop("Grouping variable must be a binary factor variables.", call. = FALSE)
  }

  n     <- tapply(varone, groupone, length)
  n1    <- n[[1]]
  n2    <- n[[2]]
  y     <- tapply(varone, groupone, table)
  y1    <- y[[1]][[2]]
  y2    <- y[[2]][[2]]
  phat1 <- y1 / n1
  phat2 <- y2 / n2
  phat  <- sum(y1, y2) / sum(n1, n2)
  num   <- (phat1 - phat2)
  den1  <- phat * (1 - phat)
  den2  <- (1 / n1) + (1 / n2)
  den   <- sqrt(den1 * den2)
  z     <- num / den


  lt    <- stats::pnorm(z)
  ut    <- round(stats::pnorm(z, lower.tail = FALSE), 4)
  tt    <- round(stats::pnorm(abs(z), lower.tail = FALSE) * 2, 4)

  alt   <- match.arg(alternative)

  if (alt == "all") {
    sig <- c("both" = tt, "less" = lt, "greater" = ut)
  } else if (alt == "greater") {
    sig <- ut
  } else if (alt == "less") {
    sig <- lt
  } else {
    sig <- tt
  }

  out <-
    list(alt   = alt,
         n1    = n1,
         n2    = n2,
         phat1 = phat1,
         phat2 = phat2,
         sig   = round(sig, 3),
         z     = round(z, 3))

  print_ts_prop_test(out)
}

xpl_ts_prop_calc <- function(n1, n2, p1, p2,
                               alternative = c("both", "less", "greater", "all"), ...) {
  n1    <- n1
  n2    <- n2
  phat1 <- p1
  phat2 <- p2
  phat  <- sum(n1 * p1, n2 * p2) / sum(n1, n2)
  num   <- (phat1 - phat2)
  den1  <- phat * (1 - phat)
  den2  <- (1 / n1) + (1 / n2)
  den   <- sqrt(den1 * den2)
  z     <- num / den

  lt    <- stats::pnorm(z)
  ut    <- round(stats::pnorm(z, lower.tail = FALSE), 4)
  tt    <- round(stats::pnorm(abs(z), lower.tail = FALSE) * 2, 4)

  alt   <- match.arg(alternative)

  if (alt == "all") {
    sig <- c("both" = tt, "less" = lt, "greater" = ut)
  } else if (alt == "greater") {
    sig <- ut
  } else if (alt == "less") {
    sig <- lt
  } else {
    sig <- tt
  }

  out <-
    list(alt   = alt,
         n1    = n1,
         n2    = n2,
         phat1 = round(phat1, 3),
         phat2 = round(phat2, 3),
         sig   = round(sig, 3),
         z     = round(z, 3))

  print_ts_prop_test(out)
}

prop_comp2 <- function(var1, var2, alt) {

  n1    <- length(var1)
  n2    <- length(var2)
  y1    <- table(var1)[[2]]
  y2    <- table(var2)[[2]]

  phat1 <- round(y1 / n1, 4)
  phat2 <- round(y2 / n2, 4)
  phat  <- sum(y1, y2) / sum(n1, n2)

  num   <- (phat1 - phat2)
  den1  <- phat * (1 - phat)
  den2  <- (1 / n1) + (1 / n2)
  den   <- sqrt(den1 * den2)
  z     <- round(num / den, 4)

  lt    <- round(stats::pnorm(z), 4)
  ut    <- round(stats::pnorm(z, lower.tail = FALSE), 4)
  tt    <- round(stats::pnorm(abs(z), lower.tail = FALSE) * 2, 4)

  if (alt == "all") {
    sig <- c("two-tail" = tt, "lower-tail" = lt, "upper-tail" = ut)
  } else if (alt == "greater") {
    sig <- ut
  } else if (alt == "less") {
    sig <- lt
  } else {
    sig <- tt
  }

  list(n1    = n1,
       n2    = n2,
       phat1 = phat1,
       phat2 = phat2,
       sig   = round(sig, 3),
       z     = round(z, 3))

}

# two sample variance test
xpl_ts_var_test <- function(data, variables = NULL, group_var = "NULL",
                                      alternative = c("less", "greater", "all")) {

  groupvar  <- group_var
  varyables <- variables
  fdata     <- data[varyables]

  if (groupvar == "NULL") {
    z  <- as.list(fdata)
    ln <- unlist(lapply(z, length))
    ly <- seq_len(length(z))

    if (length(z) < 2) {
      stop("Please specify at least two variables.", call. = FALSE)
    }

    out   <- xpl_gvar(ln, ly)
    fdata <- unlist(z)

    groupvars <-
      out %>%
      unlist() %>%
      as.factor()

    lev <- names(data[varyables])

  } else {

    fdata     <- fdata[[1]]
    groupvars <- data[[groupvar]]
    lev       <- levels(groupvars)

    if (length(fdata) != length(groupvars)) {
      stop("Length of variable and group_var do not match.", call. = FALSE)
    }
  }


  type <- match.arg(alternative)
  k    <- var_comp(fdata, groupvars)

  out <- list(avg   = k$avg,
              avgs  = k$avgs,
              f     = k$f,
              len   = k$len,
              lens  = k$lens,
              lev   = lev,
              lower = k$lower,
              n1    = k$n1,
              n2    = k$n2,
              sd    = k$sd,
              sds   = k$sds,
              se    = k$se,
              ses   = k$ses,
              type  = type,
              upper = k$upper,
              vars  = k$vars)

  print_var_test(out)
}

var_comp <- function(variable, group_var) {

  comp  <- stats::complete.cases(variable, group_var)
  cvar  <- variable[comp]
  gvar  <- group_var[comp]

  d     <- data.frame(cvar, gvar)
  vals  <- tibble_stats(d, "cvar", "gvar")
  lass  <- tbl_stats(d, "cvar")

  lens  <- vals[[2]]
  vars  <- vals[[4]]

  f     <- vars[1] / vars[2]
  n1    <- lens[1] - 1
  n2    <- lens[2] - 1
  lower <- stats::pf(f, n1, n2)
  upper <- stats::pf(f, n1, n2, lower.tail = FALSE)

  list(avg   = round(lass[2], 2),
       avgs  = round(vals[[3]], 2),
       f     = round(f, 4),
       len   = lass[1],
       lens  = lens,
       lower = round(lower, 4),
       n1    = n1,
       n2    = n2,
       sd    = round(lass[3], 2),
       sds   = round(vals[[5]], 2),
       se    = round(lass[4], 2),
       ses   = round(vals[[6]], 2),
       upper = round(upper, 4),
       vars  = round(vars, 2))

}

tibble_stats <- function(data, x, y) {

  dat <- data.table(data[c(x, y)])

  out <- dat[, .(length = length(get(x)),
                     mean = mean(get(x)),
                     var = stats::var(get(x)),
                     sd = stats::sd(get(x))),
                  by = y]

  out[, ':='(ses = sd / sqrt(length))]
  setDF(out)
  out <- out[order(out[, 1]),]
  return(out)

}

tbl_stats <- function(data, y) {

  dat <- data[[y]]
  c(length(dat), mean(dat), sd(dat), (sd(dat) / sqrt(length(dat))))

}

# binomial test
xpl_binom_calc <- function(n, success, prob = 0.5, ...) {

  if (!is.numeric(n)) {
    stop("n must be an integer")
  }

  if (!is.numeric(success)) {
    stop("success must be an integer")
  }

  if (!is.numeric(prob)) {
    stop("prob must be numeric")
  }

  if ((prob < 0) | (prob > 1)) {
    stop("prob must be between 0 and 1")
  }

  k <- binom_comp(n, success, prob)

  out <-
    list(
      exp_k      = k$exp_k,
      exp_p      = k$exp_p,
      k          = k$k,
      n          = n,
      obs_p      = k$obs_p,
      pval_lower = k$lower,
      pval_upper = k$upper
  )

  print_binom(out)
}

xpl_binom_test <- function(data, variable, prob = 0.5) {

  varyable <- variable
  fdata    <- data[[varyable]]

  if (!is.factor(fdata)) {
    stop("variable must be of type factor", call. = FALSE)
  }

  if (nlevels(fdata) > 2) {
    stop("Binomial test is applicable only to binary data i.e. categorical data with 2 levels.", call. = FALSE)
  }

  if (!is.numeric(prob)) {
    stop("prob must be numeric", call. = FALSE)
  }

  if ((prob < 0) | (prob > 1)) {
    stop("prob must be between 0 and 1", call. = FALSE)
  }

  n <- length(fdata)
  k <- table(fdata)[[2]]
  xpl_binom_calc(n, k, prob)
}

binom_comp <- function(n, success, prob) {

  n     <- n
  k     <- success
  obs_p <- k / n
  exp_k <- round(n * prob)
  lt    <- stats::pbinom(k, n, prob, lower.tail = T)
  ut    <- stats::pbinom(k - 1, n, prob, lower.tail = F)
  p_opp <- round(stats::dbinom(k, n, prob), 9)
  i_p   <- stats::dbinom(exp_k, n, prob)
  i_k   <- exp_k

  if (k < exp_k) {
    while (i_p > p_opp) {
      i_k <- i_k + 1
      i_p <- round(stats::dbinom(i_k, n, prob), 9)
      if (round(i_p) == p_opp) {
        break
      }
    }

    ttf <- stats::pbinom(k, n, prob, lower.tail = T) +
      stats::pbinom(i_k - 1, n, prob, lower.tail = F)
  } else {
    while (p_opp <= i_p) {
      i_k <- i_k - 1
      i_p <- stats::dbinom(i_k, n, prob)
      if (round(i_p) == p_opp) {
        break
      }
    }

    i_k <- i_k

    tt <- stats::pbinom(i_k, n, prob, lower.tail = T) +
      stats::pbinom(k - 1, n, prob, lower.tail = F)

    ttf <- ifelse(tt <= 1, tt, 1)
  }

  list(
    n = n, k = k, exp_k = exp_k, obs_p = obs_p, exp_p = prob, ik = i_k,
    lower = round(lt, 6), upper = round(ut, 6), two_tail = round(ttf, 6)
  )

}
