print_owanova <- function(data) {

  # width
  w1 <- nchar("Between Groups")
  w2 <- max(nchar("Squares"), nchar(data$ss_between), nchar(data$ss_within), nchar(data$ss_total))
  w3 <- max(nchar("DF"), nchar(data$df_btw), nchar(data$df_btw), nchar(data$df_within), nchar(data$df_total))
  w4 <- max(nchar("Mean Square"), nchar(data$ms_btw), nchar(data$ms_within))
  w5 <- max(nchar("F"), nchar(data$fstat))
  w6 <- max(nchar("Sig."), nchar(format(data$pval, nsmall = 4)))
  w <- sum(w1, w2, w3, w4, w5, w6, 21)
  w7 <- nchar(data$rmse)

  dc <- as.vector(data$group_stats[, 1])

  w8 <- max(nchar("Category"), max(nchar(dc)))
  w9 <- max(nchar("N"), max(nchar(data$group_stats[[2]])))
  w10 <- max(nchar("Mean"), max(nchar(format(data$group_stats[[3]], nsmall = 3))))
  w11 <- max(nchar("Std. Dev."), max(nchar(format(data$group_stats[[4]], nsmall = 3))))
  wr <- sum(w8, w9, w10, w11, 13)


  p <- format(data$pval, nsmall = 4)
  q <- nrow(data$group_stats)
  s <- length(data$group_stats)


  cat(fg("ANOVA", w), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(fg("", w1), fs(), fg("Sum of", w2), fs(), fg("", w3), fs(), fg("", w4), fs(), fg("", w5), fs(), fg("", w6), "\n")
  cat(fg("", w1), fs(), fg("Squares", w2), fs(), fg("DF", w3), fs(), fg("Mean Square", w4), fs(), fg("F", w5), fs(), fg("Sig.", w6), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(fl("Between Groups", w1), fs(), fg(data$ss_between, w2), fs(), fg(data$df_btw, w3), fs(), fg(data$ms_btw, w4), fs(), fg(data$fstat, w5), fs(), fg(data$pval, w6), "\n")
  cat(fl("Within Groups", w1), fs(), fg(data$ss_within, w2), fs(), fg(data$df_within, w3), fs(), fg(data$ms_within, w4), fs(), fg("", w5), fs(), fg("", w6), "\n")
  cat(fl("Total", w1), fs(), fg(data$ss_total, w2), fs(), fg(data$df_total, w3), fs(), fg("", w4), fs(), fg("", w5), fs(), fg("", w6), "\n")
  cat(rep("-", w), sep = "", "\n\n")

  cat(fg("Report", wr), "\n")
  cat(rep("-", wr), sep = "", "\n")
  cat(fg("Category", w8), fs(), fg("N", w9), fs(), fg("Mean", w10), fs(), fg("Std. Dev.", w11), "\n")
  cat(rep("-", wr), sep = "", "\n")
  for (i in seq_len(q)) {
    cat(
      fc(data$group_stats[[i, 1]], w8), fs(), fg(data$group_stats[[i, 2]], w9), fs(), fk(format(round(data$group_stats[[i, 3]], 3), nsmall = 3), w10),
      fs(), fk(format(round(data$group_stats[[i, 4]], 3), nsmall = 3), w11), "\n"
    )
  }
  cat(rep("-", wr), sep = "", "\n\n")

  cat(fl("Number of obs", 13), "=", fl(data$obs, w7), fs(), fl("R-squared", 13), "=", data$r2, "\n")
  cat(fl("Root MSE", 13), "=", data$rmse, fs(), fl("Adj R-squared", 13), "=", data$adjusted_r2, "\n\n")
}


print_binom <- function(data) {

  # widths
  w1 <- nchar("Group")
  w2 <- max(nchar("N"), nchar(data$n))
  w3 <- max(nchar("Obs. Prop"), nchar(data$obs_p))
  w4 <- max(nchar("Exp. Prop"), nchar(data$exp_p))
  w <- sum(w1, w2, w3, w4, 13)

  k0 <- data$n - data$k
  p0 <- 1 - data$obs_p
  e0 <- 1 - data$exp_p

  cat(format("Binomial Test", width = w, justify = "centre"), "\n")
  cat(" ", rep("-", w), sep = "", "\n")
  cat(
    " ", format("Group", width = w1, justify = "left"), fs(),
    format("N", width = w2, justify = "centre"), fs(),
    format("Obs. Prop", width = w3, justify = "centre"), fs(),
    format("Exp. Prop", width = w4, justify = "centre"), "\n"
  )
  cat(" ", rep("-", w), sep = "", "\n")
  cat(
    " ", format("0", width = w1, justify = "centre"), fs(),
    format(k0, width = w2, justify = "right"), fs(),
    format(p0, width = w3, justify = "centre"), fs(),
    format(e0, width = w4, justify = "centre", nsmall = 3), "\n"
  )
  cat(
    " ", format("1", width = w1, justify = "centre"), fs(),
    format(data$k, width = w2, justify = "right"), fs(),
    format(data$obs_p, width = w3, justify = "centre"), fs(),
    format(data$exp_p, width = w4, justify = "centre", nsmall = 3), "\n"
  )
  cat(" ", rep("-", w), sep = "", "\n")

  # test summary widths
  w6 <- nchar("Lower")
  w7 <- nchar(paste0("Pr(k <= ", data$k, " or k >= ", data$k, ")"))
  w8 <- nchar(paste0("Pr(k <= ", data$k, " or k >= ", data$k, ")"))
  w9 <- 8
  w10 <- sum(w6, w7, w9, 9)
  w11 <- sum(w6, w8, w9, 9)




  if (data$k < data$exp_k) {
    cat("\n\n", format("Test Summary", width = w11, justify = "centre"), "\n")
    cat(" ", rep("-", w11), sep = "", "\n")
    cat(
      " ", format("Tail", width = w6, justify = "left"), fs(), format("Prob", width = w8, justify = "centre"), fs(),
      format("p-value", width = w9, justify = "centre"), "\n"
    )
    cat(" ", rep("-", w11), sep = "", "\n")
    cat(
      " ", format("Lower", width = w6, justify = "left"), fs(), format(paste0("Pr(k <= ", data$k, ")"), width = w8, justify = "centre"), fs(),
      format(as.character(data$pval_lower), width = w9, justify = "centre"), "\n"
    )
    cat(
      " ", format("Upper", width = w6, justify = "left"), fs(), format(paste0("Pr(k >= ", data$k, ")"), width = w8, justify = "centre"), fs(),
      format(as.character(data$pval_upper), width = w9, justify = "centre"), "\n"
    )
    
    cat(" ", rep("-", w11), sep = "", "\n")
  } else {
    cat("\n\n", format("Test Summary", width = w10, justify = "centre"), "\n")
    cat(" ", rep("-", w10), sep = "", "\n")
    cat(
      " ", format("Tail", width = w6, justify = "left"), fs(), format("Prob", width = w7, justify = "centre"), fs(),
      format("p-value", width = w9, justify = "centre"), "\n"
    )
    cat(" ", rep("-", w10), sep = "", "\n")
    cat(
      " ", format("Lower", width = w6, justify = "left"), fs(), format(paste0("Pr(k <= ", data$k, ")"), width = w7, justify = "centre"), fs(),
      format(as.character(data$pval_lower), width = w9, justify = "centre"), "\n"
    )
    cat(
      " ", format("Upper", width = w6, justify = "left"), fs(), format(paste0("Pr(k >= ", data$k, ")"), width = w7, justify = "centre"), fs(),
      format(as.character(data$pval_upper), width = w9, justify = "centre"), "\n"
    )
   
    cat(" ", rep("-", w10), sep = "", "\n")
  }
}


print_ttest <- function(data) {
  null_l <- paste0("Ho: mean(", data$var_name, ") >=", as.character(data$mu))
  alt_l <- paste0(" Ha: mean(", data$var_name, ") <", as.character(data$mu))
  null_u <- paste0("Ho: mean(", data$var_name, ") <=", as.character(data$mu))
  alt_u <- paste0("Ha: mean(", data$var_name, ") >", as.character(data$mu))
  null_t <- paste0("Ho: mean(", data$var_name, ") ~=", as.character(data$mu))
  alt_t <- paste0("Ha: mean(", data$var_name, ") !=", as.character(data$mu))
  all_l <- paste("Ha: mean <", as.character(data$mu))
  all_u <- paste("Ha: mean >", as.character(data$mu))
  all_t <- paste("Ha: mean ~=", as.character(data$mu))
  char_p_l <- format(round(data$p_l, 5), nsmall = 5, scientific = FALSE)
  char_p_u <- format(round(data$p_u, 5), nsmall = 5, scientific = FALSE)
  char_p   <- format(round(data$p, 5), nsmall = 5, scientific = FALSE)
  all_p_l <- paste("P < t =", char_p_l)
  all_p_t <- paste("P > |t| =", char_p)
  all_p_u <- paste("P > t =", char_p_u)
  all_tval <- paste0(" t = ", as.character(data$test_stat))


  # formatting output
  # compute the characters of each output and decide the overall width
  var_width <- max(nchar("Variable"), nchar(data$var_name))
  obs_width <- max(nchar("Obs"), nchar(data$n))
  mean_width <- max(nchar("Mean"), nchar(data$Mean))
  se_width <- max(nchar("Std. Err."), nchar(data$std_err))
  sd_width <- max(nchar("Std. Dev."), nchar(data$stddev))
  conf_length <- nchar(data$confint[1]) + nchar(data$confint[2])
  conf_str <- paste0("[", data$conf * 100, "% Conf. Interval]")
  confint_length <- nchar(conf_str)
  if (conf_length > confint_length) {
    conf_width <- round(conf_length / 2)
  } else {
    conf_width <- round(confint_length / 2)
  }
  t_width <- nchar(data$test_stat)
  df_width <- max(nchar("DF"), nchar(data$df))
  p_width <- max(nchar("2 Tailed"), nchar(round(data$p, 5)))
  md_width <- max(nchar("Difference"), nchar(data$mean_diff))
  md_length <- nchar(data$mean_diff_l) + nchar(data$mean_diff_u)
  if (md_length > confint_length) {
    md_conf_width <- floor(md_length / 2)
  } else {
    md_conf_width <- floor(confint_length / 2)
  }

  width_1 <- sum(var_width, obs_width, mean_width, se_width, sd_width, ceiling(conf_width * 2), 26)
  width_2 <- sum(var_width, t_width, df_width, p_width, md_width, ceiling(md_conf_width * 2), 26)
  all_width <- round(width_1 / 3)

  cat(
    format("One-Sample Statistics", width = width_1, justify = "centre"),
    "\n"
  )
  cat(rep("-", width_1), sep = "")
  cat(
    "\n", formatter_t("Variable", var_width), formats_t(),
    formatter_t("Obs", obs_width), formats_t(),
    formatter_t("Mean", mean_width),
    formats_t(), formatter_t("Std. Err.", se_width), formats_t(),
    formatter_t("Std. Dev.", sd_width), formats_t(),
    formatter_t(conf_str, conf_width), "\n"
  )
  cat(rep("-", width_1), sep = "")
  cat(
    "\n", formatter_t(data$var_name, var_width), formats_t(),
    formatter_t(data$n, obs_width), formats_t(),
    formatter_t(data$Mean, mean_width),
    formats_t(), formatter_t(data$std_err, sd_width), formats_t(),
    formatter_t(data$stddev, se_width), formats_t(),
    format_cil(data$confint[1], conf_width),
    format_ciu(data$confint[2], conf_width), "\n"
  )
  cat(rep("-", width_1), sep = "")

  # print result
  if (data$type == "less") {
    cat("\n\n", format("Lower Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"), "\n")
    cat("\n", format(null_l, width = width_2, justify = "centre"))
    cat("\n", format(alt_l, width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t("Variable", var_width), formats_t(), formatter_t("t", t_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), formats_t(), formatter_t("Mean Diff.", md_width), formats_t(), formatter_t(conf_str, md_conf_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$test_stat, 3), t_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(round(data$p_l, 5), p_width),
      formats_t(), formatter_t(data$mean_diff, md_width), formats_t(),
      format_cil(round(data$mean_diff_l, 4), md_conf_width),
      format_ciu(round(data$mean_diff_u, 4), md_conf_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
  } else if (data$type == "greater") {
    cat("\n\n", format("Upper Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"), "\n")
    cat("\n", format(null_u, width = width_2, justify = "centre"))
    cat("\n", format(alt_u, width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t("Variable", var_width), formats_t(), formatter_t("t", t_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), formats_t(), formatter_t("Mean Diff.", md_width), formats_t(), formatter_t(conf_str, md_conf_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$test_stat, 3), t_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(round(data$p_l, 5), p_width),
      formats_t(), formatter_t(data$mean_diff, md_width), formats_t(),
      format_cil(round(data$mean_diff_l, 4), md_conf_width),
      format_ciu(round(data$mean_diff_u, 4), md_conf_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
  } else if (data$type == "both") {
    cat("\n\n", format("Two Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"), "\n")
    cat("\n", format(null_t, width = width_2, justify = "centre"))
    cat("\n", format(alt_t, width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t("Variable", var_width), formats_t(), formatter_t("t", t_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), formats_t(), formatter_t("Mean Diff.", md_width), formats_t(), formatter_t(conf_str, md_conf_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$test_stat, 3), t_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(round(data$p_l, 5), p_width),
      formats_t(), formatter_t(data$mean_diff, md_width), formats_t(),
      format_cil(round(data$mean_diff_l, 4), md_conf_width),
      format_ciu(round(data$mean_diff_u, 4), md_conf_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
  } else {
    cat("\n\n", format(null_t, width = width_2, justify = "centre"))
    cat("\n\n", format(all_l, width = all_width, justify = "centre"), format(all_t, width = all_width, justify = "centre"), format(all_u, width = all_width, justify = "centre"), "\n")
    cat(format(all_tval, width = all_width, justify = "centre"), format(all_tval, width = all_width, justify = "centre"), format(all_tval, width = all_width, justify = "centre"))
    cat("\n", format(all_p_l, width = all_width, justify = "centre"), format(all_p_t, width = all_width, justify = "centre"), format(all_p_u, width = all_width, justify = "centre"))
  }
}


print_paired_ttest <- function(data) {
  char_p_u     <- format(round(data$p_upper, 3), nsmall = 3, scientific = FALSE)
  char_p_l     <- format(round(data$p_lower, 3), nsmall = 3, scientific = FALSE)
  char_p       <- format(round(data$p_two_tail, 3), nsmall = 3, scientific = FALSE)
  # hypothesis heading
  hyp_null <- paste0("Ho: mean(", data$var_names[1], " - ", data$var_names[2], ") = ", "0")
  hyp_lt <- paste0("Ha: mean(", data$var_names[1], " - ", data$var_names[2], ") < ", "0")
  hyp_ut <- paste0("Ha: mean(", data$var_names[1], " - ", data$var_names[2], ") > ", "0")
  hyp_2t <- paste0("Ha: mean(", data$var_names[1], " - ", data$var_names[2], ") ~= ", "0")
  conf <- data$confint * 100
  conf_char <- paste0("[", conf, "% Conf. Interval]")

  # all tests combines
  all_null <- paste0("Ho: mean(", data$var_names[1], " - ", data$var_names[2], ") = mean(diff) = ", "0")
  all_p_l <- paste("P < t =", char_p_l)
  all_p_t <- paste("P > |t| =", char_p)
  all_p_u <- paste("P > t =", char_p_u)
  all_tval <- paste0(" t = ", as.character(data$tstat))

  # formatting output
  var_width1 <- max(nchar("Variables"), nchar(data$var_names[1]), nchar(data$var_names[2]), nchar("diff"))
  var_width <- max(nchar("Variables"), nchar(data$xy))
  obs_width <- max(nchar("Obs"), nchar(data$Obs))
  mean_width <- max(nchar("Mean"), nchar(format(max(data$b[["mean"]]), nsmall = 2)))
  se_width <- max(nchar("Std. Err."), nchar(format(max(data$b[["se"]]), nsmall = 2)))
  sd_width <- max(nchar("Std. Dev."), nchar(format(max(data$b[["sd"]]), nsmall = 2)))
  corr_width <- nchar("Correlation")
  corsig_width <- max(nchar("Sig."), nchar(data$corsig))
  t_width <- nchar(data$tstat)
  df_width <- max(nchar("DF"), nchar(data$df))
  p_width <- max(nchar("Sig."), nchar(format(data$corsig, nsmall = 3)))
  conf_length <- max(sum(nchar(data$conf_int1)), sum(nchar(data$conf_int2)))
  if (conf_length > 20) {
    conf_width <- conf_length
    conf_l_width <- ceiling(conf_width / 2)
    conf_u_width <- ceiling(conf_width / 2)
  } else {
    conf_width <- 20
    conf_l_width <- 10
    conf_u_width <- 10
  }
  space1 <- 20
  space2 <- 13
  space3 <- 13
  width_1 <- sum(var_width1, obs_width, mean_width, se_width, sd_width, conf_width, space1)
  width_2 <- sum(var_width, obs_width, corr_width, corsig_width, space2)
  width_3 <- sum(var_width, t_width, df_width, p_width, space3)

  cat(format("Paired Samples Statistics", width = width_1, justify = "centre"), "\n")
  cat(rep("-", width_1), sep = "", "\n")
  cat(
    formatter_pair("Variables", var_width1), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Mean", mean_width),
    formats_t(), formatter_pair("Std. Err.", se_width), formats_t(), formatter_pair("Std. Dev.", sd_width), formats_t(), conf_char, "\n"
  )
  cat(rep("-", width_1), sep = "")
  cat(
    "\n", formatter_pair(data$var_names[1], var_width1), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[[1, 1]], mean_width),
    formats_t(), formatter_pair(data$b[[1, 3]], se_width), formats_t(), formatter_pair(data$b[[1, 2]], sd_width), formats_t(), format_cil(data$conf_int1[[1]], conf_l_width),
    format_ciu(data$conf_int1[[2]], conf_u_width)
  )
  cat(
    "\n", formatter_pair(data$var_names[2], var_width1), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[[2, 1]], mean_width), formats_t(), formatter_pair(data$b[[2, 3]], se_width),
    formats_t(), formatter_pair(data$b[[2, 2]], sd_width), formats_t(), format_cil(data$conf_int2[[1]], conf_l_width),
    format_ciu(data$conf_int2[[2]], conf_u_width), "\n"
  )
  cat(rep("-", width_1), sep = "")
  cat(
    "\n", formatter_pair("diff", var_width1), formats_t(), formatter_pair(data$Obs, obs_width), formats_t(), formatter_pair(data$b[[3, 1]], mean_width), formats_t(), formatter_pair(data$b[[3, 3]], se_width),
    formats_t(), formatter_pair(data$b[[3, 2]], sd_width), formats_t(), format_cil(data$conf_int_diff[[1]], conf_l_width),
    format_ciu(data$conf_int_diff[[2]], conf_u_width), "\n"
  )
  cat(rep("-", width_1), sep = "")
  cat("\n\n", format("Paired Samples Correlations", width = width_2, justify = "centre"), "\n")
  cat(rep("-", width_2), sep = "")
  cat(
    "\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("Obs", obs_width), formats_t(), formatter_pair("Correlation", corr_width),
    formats_t(), formatter_pair("Sig.", corsig_width)
  )
  cat(
    "\n", formatter_pair(paste(data$var_names[1], "&", data$var_names[2]), var_width), formats_t(), formatter_pair(data$Obs, obs_width),
    formats_t(), formatter_pair(data$corr, corr_width), formats_t(), format(data$corsig, corsig_width), "\n"
  )
  cat(rep("-", width_2), sep = "", "\n\n")

  # print output
  if (data$alternative == "less") {
    cat(format("Paired Samples Test", width = width_3, justify = "centre"), "\n")
    cat(format("-------------------", width = width_3, justify = "centre"), "\n")
    cat(format(hyp_null, width = width_3, justify = "centre"), "\n")
    cat(format(hyp_lt, width = width_3, justify = "centre"), "\n\n")
    cat(rep("-", width_3), sep = "")
    cat(
      "\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("t", t_width),
      formats_t(), formatter_pair("df", df_width), formats_t(), formatter_pair("Sig.", p_width), "\n"
    )
    cat(rep("-", width_3), sep = "")
    cat(
      "\n", formatter_pair(paste(data$var_names[1], "-", data$var_names[2]), var_width), formats_t(), formatter_pair(data$tstat, t_width), formats_t(), format(data$df, df_width),
      formats_t(), formatter_pair(char_p_l, p_width), "\n"
    )
    cat(rep("-", width_3), sep = "")
  } else if (data$alternative == "greater") {
    cat(format("Paired Samples Test", width = width_3, justify = "centre"), "\n")
    cat(format("-------------------", width = width_3, justify = "centre"), "\n")
    cat(format(hyp_null, width = width_3, justify = "centre"), "\n")
    cat(format(hyp_ut, width = width_3, justify = "centre"), "\n\n")
    cat(rep("-", width_3), sep = "")
    cat(
      "\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("t", t_width),
      formats_t(), formatter_pair("df", df_width), formats_t(), formatter_pair("Sig.", p_width), "\n"
    )
    cat(rep("-", width_3), sep = "")
    cat(
      "\n", formatter_pair(paste(data$var_names[1], "-", data$var_names[2]), var_width), formats_t(), formatter_pair(data$tstat, t_width), formats_t(), format(data$df, df_width),
      formats_t(), formatter_pair(char_p_u, p_width), "\n"
    )
    cat(rep("-", width_3), sep = "")
  } else if (data$alternative == "both") {
    cat(format("Paired Samples Test", width = width_3, justify = "centre"), "\n")
    cat(format("-------------------", width = width_3, justify = "centre"), "\n")
    cat(format(hyp_null, width = width_3, justify = "centre"), "\n")
    cat(format(hyp_2t, width = width_3, justify = "centre"), "\n\n")
    cat(rep("-", width_3), sep = "")
    cat(
      "\n", formatter_pair("Variables", var_width), formats_t(), formatter_pair("t", t_width),
      formats_t(), formatter_pair("df", df_width), formats_t(), formatter_pair("Sig.", p_width), "\n"
    )
    cat(rep("-", width_3), sep = "")
    cat(
      "\n", formatter_pair(paste(data$var_names[1], "-", data$var_names[2]), var_width), formats_t(), formatter_pair(data$tstat, t_width), formats_t(), format(data$df, df_width),
      formats_t(), formatter_pair(char_p, p_width), "\n"
    )
    cat(rep("-", width_3), sep = "")
  } else {
    cat(format(all_null, width = 72, justify = "centre"), "\n\n")
    cat(
      format("Ha: mean(diff) < 0", width = 24, justify = "centre"), format("Ha: mean(diff) ~= 0", width = 24, justify = "centre"),
      format("Ha: mean(diff) > 0", width = 24, justify = "centre"), "\n"
    )
    cat(format(all_tval, width = 24, justify = "centre"), format(all_tval, width = 24, justify = "centre"), format(all_tval, width = 24, justify = "centre"), "\n")
    cat(format(all_p_l, width = 24, justify = "centre"), format(all_p_t, width = 24, justify = "centre"), format(all_p_u, width = 24, justify = "centre"), "\n")
  }
}


print_two_ttest <- function(data) {

  char_sig          <- format(round(data$sig, 4), nsmall = 4, scientific = FALSE)
  char_sig_l        <- format(round(data$sig_l, 4), nsmall = 4, scientific = FALSE)
  char_sig_u        <- format(round(data$sig_u, 4), nsmall = 4, scientific = FALSE)
  char_sig_pooled   <- format(round(data$sig_pooled, 4), nsmall = 4, scientific = FALSE)
  char_sig_pooled_l <- format(round(data$sig_pooled_l, 4), nsmall = 4, scientific = FALSE)
  char_sig_pooled_u <- format(round(data$sig_pooled_u, 4), nsmall = 4, scientific = FALSE)

  # hypothesis heading
  hyp_null <- paste0("Ho: mean(", data$levels[1], ") - mean(", data$levels[2], ") = diff = ", "0")
  hyp_lt <- paste0("Ha: diff < ", "0")
  hyp_2t <- paste0("Ha: diff ~= ", "0")
  hyp_ut <- paste0("Ha: diff > ", "0")
  conf <- data$confint * 100
  conf_char <- paste0("[", conf, "% Conf. Interval]")

  # all tests combines
  all_p_l <- paste("P < t =", char_sig_pooled_l)
  all_p_t <- paste("P > |t| =", char_sig_pooled)
  all_p_u <- paste("P > t =", char_sig_pooled_u)
  all_s_l <- paste("P < t =", char_sig_l)
  all_s_t <- paste("P > |t| =", char_sig)
  all_s_u <- paste("P > t =", char_sig_u)
  p_tval <- paste0(" t = ", as.character(data$t_pooled))
  s_tval <- paste0(" t = ", as.character(data$t_satterthwaite))

  # format output
  grp_w <- max(nchar(data$levels[1]), nchar(data$levels[2]), nchar("Combined"), 10)
  obs_w <- max(nchar("Obs"), nchar(data$obs[1]), nchar(data$obs[2]), nchar(data$n))
  mean_w <- max(nchar("Mean"), nchar(data$mean[1]), nchar(data$mean[2]), nchar(data$mean_diff), nchar(data$combined[2]))
  se_w <- max(nchar("Std. Err."), nchar(data$se[1]), nchar(data$se[2]), nchar(data$combined[4]), nchar(data$se_dif))
  sd_w <- max(nchar("Std. Dev."), nchar(data$sd[1]), nchar(data$sd[2]), nchar(data$combined[3]), nchar(data$sd_dif))
  df_w <- max(nchar("DF"), nchar(as.vector(data$df_pooled)), nchar(as.vector(data$df_satterthwaite)))
  t_w <- max(nchar("t Value"), nchar(as.vector(data$t_pooled)), nchar(as.vector(data$t_satterthwaite)))
  pt_w <- max(
    nchar("P > |t|"), nchar(as.vector(char_sig)), nchar(as.vector(char_sig_l)), nchar(as.vector(char_sig_u)),
    nchar(as.vector(char_sig_pooled)), nchar(as.vector(char_sig_pooled_l)), nchar(as.vector(char_sig_u))
  )
  numdf_w <- max(nchar("Num DF"), nchar(as.vector(data$num_df)), nchar(as.vector(data$den_df)))
  f_w <- max(nchar("F Value"), nchar(as.vector(data$f)))
  fp_w <- max(nchar("P > F"), nchar(as.vector(data$f_sig)))
  conf_length <- nchar(data$lower[1]) + nchar(data$upper[1])
  if (conf_length > 20) {
    conf_width <- conf_length
    conf_l_width <- ceiling(conf_width / 2)
    conf_u_width <- floor(conf_width / 2)
  } else {
    conf_width <- 20
    conf_l_width <- 10
    conf_u_width <- 10
  }
  w1 <- sum(grp_w, obs_w, mean_w, se_w, sd_w, conf_width, 20)
  w2 <- sum(grp_w, 13, 9, df_w, t_w, pt_w, 20)
  w3 <- sum(grp_w, 8, numdf_w, numdf_w, f_w, fp_w, 20)


  cat(fw("Group Statistics", w = w1), "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(
    fw("Group", w = grp_w), formats_t(), fw("Obs", w = obs_w), formats_t(),
    fw("Mean", w = mean_w), formats_t(), fw("Std. Err.", w = se_w), formats_t(),
    fw("Std. Dev.", w = sd_w), formats_t(), conf_char, "\n"
  )
  cat(rep("-", w1), sep = "", "\n")
  cat(
    fw((data$levels[1]), w = grp_w), formats_t(), fn(data$obs[1], w = obs_w), formats_t(),
    fn(data$mean[1], w = mean_w), formats_t(), fn(data$se[1], w = se_w), formats_t(),
    fn(data$sd[1], w = sd_w), formats_t(), fn(data$lower[1], w = conf_l_width), fn(data$upper[1], w = conf_u_width), "\n"
  )
  cat(
    fw((data$levels[2]), w = grp_w), formats_t(), fn(data$obs[2], w = obs_w), formats_t(),
    fn(data$mean[2], w = mean_w), formats_t(), fn(data$se[2], w = se_w), formats_t(),
    fn(data$sd[2], w = sd_w), formats_t(), fn(data$lower[2], w = conf_l_width), fn(data$upper[2], w = conf_u_width), "\n"
  )
  cat(rep("-", w1), sep = "", "\n")
  cat(
    fw("combined", w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(),
    fn(data$combined[2], w = mean_w), formats_t(), fn(data$combined[4], w = se_w), formats_t(),
    fn(data$combined[3], w = sd_w), formats_t(), fn(data$combined[7], w = conf_l_width), fn(data$combined[8], w = conf_u_width), "\n"
  )
  cat(rep("-", w1), sep = "", "\n")
  cat(
    fw(("diff"), w = grp_w), formats_t(), fn(data$n, w = obs_w), formats_t(),
    fn(data$mean_diff, w = mean_w), formats_t(), fn(as.vector(data$se_dif), w = se_w), formats_t(),
    fn(as.vector(data$sd_dif), w = sd_w), formats_t(), fn(as.vector(data$conf_diff[1]), w = conf_l_width),
    fn(as.vector(data$conf_diff[2]), w = conf_u_width), "\n"
  )
  cat(rep("-", w1), sep = "", "\n\n")

  if (data$alternative == "less") {
    cat(fw("Independent Samples Test", w = w2), "\n")
    cat(fw("------------------------", w = w2), "\n\n")
    cat(fw(hyp_null, w = w2), "\n")
    cat(fw(hyp_lt, w = w2), "\n\n")
    cat(rep("-", w2), sep = "", "\n")
    cat(
      fw("Variable", w = grp_w), formats_t(), fw("Method", w = 13), formats_t(),
      fw("Variances", w = 9), formats_t(), fw("DF", w = df_w), formats_t(),
      fw("t Value", w = t_w), formats_t(), fw("P < t", w = pt_w), "\n"
    )
    cat(rep("-", w2), sep = "", "\n")
    cat(
      fw(data$var_y, w = grp_w), formats_t(), fw("Pooled", w = 13), formats_t(),
      fw("Equal", w = 9), formats_t(), fn(data$df_pooled, w = df_w), formats_t(),
      fw(data$t_pooled, w = t_w), formats_t(), fw(char_sig_pooled_l, w = pt_w), "\n"
    )
    cat(
      fw(data$var_y, w = grp_w), formats_t(), fw("Satterthwaite", w = 13), formats_t(),
      fw("Unequal", w = 9), formats_t(), fn(data$df_satterthwaite, w = df_w), formats_t(),
      fw(data$t_satterthwaite, w = t_w), formats_t(), fw(char_sig_l, w = pt_w), "\n"
    )
    cat(rep("-", w2), sep = "", "\n\n")
  } else if (data$alternative == "greater") {
    cat(fw("Independent Samples Test", w = w2), "\n")
    cat(fw("------------------------", w = w2), "\n\n")
    cat(fw(hyp_null, w = w2), "\n")
    cat(fw(hyp_ut, w = w2), "\n\n")
    cat(rep("-", w2), sep = "", "\n")
    cat(
      fw("Variable", w = grp_w), formats_t(), fw("Method", w = 13), formats_t(),
      fw("Variances", w = 9), formats_t(), fw("DF", w = df_w), formats_t(),
      fw("t Value", w = t_w), formats_t(), fw("P > t", w = pt_w), "\n"
    )
    cat(rep("-", w2), sep = "", "\n")
    cat(
      fw(data$var_y, w = grp_w), formats_t(), fw("Pooled", w = 13), formats_t(),
      fw("Equal", w = 9), formats_t(), fn(data$df_pooled, w = df_w), formats_t(),
      fw(data$t_pooled, w = t_w), formats_t(), fw(char_sig_pooled_u, w = pt_w), "\n"
    )
    cat(
      fw(data$var_y, w = grp_w), formats_t(), fw("Satterthwaite", w = 13), formats_t(),
      fw("Unequal", w = 9), formats_t(), fn(data$df_satterthwaite, w = df_w), formats_t(),
      fw(data$t_satterthwaite, w = t_w), formats_t(), fw(char_sig_u, w = pt_w), "\n"
    )
    cat(rep("-", w2), sep = "", "\n\n")
  } else if (data$alternative == "both") {
    cat(fw("Independent Samples Test", w = w2), "\n")
    cat(fw("------------------------", w = w2), "\n\n")
    cat(fw(hyp_null, w = w2), "\n")
    cat(fw(hyp_2t, w = w2), "\n\n")
    cat(rep("-", w2), sep = "", "\n")
    cat(
      fw("Variable", w = grp_w), formats_t(), fw("Method", w = 13), formats_t(),
      fw("Variances", w = 9), formats_t(), fw("DF", w = df_w), formats_t(),
      fw("t Value", w = t_w), formats_t(), fw("P > |t|", w = pt_w), "\n"
    )
    cat(rep("-", w2), sep = "", "\n")
    cat(
      fw(data$var_y, w = grp_w), formats_t(), fw("Pooled", w = 13), formats_t(),
      fw("Equal", w = 9), formats_t(), fn(data$df_pooled, w = df_w), formats_t(),
      fw(data$t_pooled, w = t_w), formats_t(), fw(char_sig_pooled, w = pt_w), "\n"
    )
    cat(
      fw(data$var_y, w = grp_w), formats_t(), fw("Satterthwaite", w = 13), formats_t(),
      fw("Unequal", w = 9), formats_t(), fn(data$df_satterthwaite, w = df_w), formats_t(),
      fw(data$t_satterthwaite, w = t_w), formats_t(), fw(char_sig, w = pt_w), "\n"
    )
    cat(rep("-", w2), sep = "", "\n\n")
  } else {
    cat(fw("Independent Samples Test", w = 72), "\n")
    cat(fw("------------------------", w = w2), "\n\n")
    cat(format(hyp_null, width = 72, justify = "centre"), "\n\n")
    cat(
      format("Ha: diff < 0", width = 24, justify = "centre"), format("Ha: diff ~= 0", width = 24, justify = "centre"),
      format("Ha: diff > 0", width = 24, justify = "centre"), "\n\n"
    )
    cat(
      format("", width = 24, justify = "centre"), format("Pooled", width = 24, justify = "centre"),
      format("", width = 24, justify = "centre"), "\n"
    )
    cat(rep("-", 72), sep = "", "\n")
    cat(format(p_tval, width = 24, justify = "centre"), format(p_tval, width = 24, justify = "centre"), format(p_tval, width = 24, justify = "centre"), "\n")
    cat(format(all_p_l, width = 24, justify = "centre"), format(all_p_t, width = 24, justify = "centre"), format(all_p_u, width = 24, justify = "centre"), "\n\n")
    cat(
      format("", width = 24, justify = "centre"), format("Satterthwaite", width = 24, justify = "centre"),
      format("", width = 24, justify = "centre"), "\n"
    )
    cat(rep("-", 72), sep = "", "\n")
    cat(format(s_tval, width = 24, justify = "centre"), format(s_tval, width = 24, justify = "centre"), format(s_tval, width = 24, justify = "centre"), "\n")
    cat(format(all_s_l, width = 24, justify = "centre"), format(all_s_t, width = 24, justify = "centre"), format(all_s_u, width = 24, justify = "centre"), "\n\n\n")
  }

  cat(fw("Test for Equality of Variances", w = w3), "\n")
  cat(rep("-", w3), sep = "", "\n")
  cat(
    fw("Variable", w = grp_w), formats_t(), fw("Method", w = 8), formats_t(),
    fw("Num DF", w = numdf_w), formats_t(), fw("Den DF", w = numdf_w), formats_t(),
    fw("F Value", w = f_w), formats_t(), fw("P > F", w = fp_w), "\n"
  )
  cat(rep("-", w3), sep = "", "\n")
  cat(
    fw(data$var_y, w = grp_w), formats_t(), fw("Folded F", w = 8), formats_t(),
    fn(data$num_df, w = numdf_w), formats_t(), fn(data$den_df, w = numdf_w), formats_t(),
    fn(data$f, w = f_w), formats_t(), fn(data$f_sig, w = fp_w), "\n"
  )
  cat(rep("-", w3), sep = "")
}


print_prop_test <- function(data) {
  cwidth <- max(nchar("z"), nchar("DF"), nchar("Pr(|Z| > |z|)"), nchar("Sample Size"), nchar("phat"))
  nwidth <- max(nchar(data$z), nchar(data$p0), nchar(data$sig[1]), nchar(data$n), nchar(data$phat))
  w1 <- sum(cwidth, nwidth, 6)
  lw <- max(nchar("Variable"), nchar(data$varname))
  ow <- max(nchar("Observed"), nchar(data$n))
  ew <- max(nchar("Expected"), nchar(data$exp))
  dw <- max(nchar("% Deviation"), nchar(data$deviation))
  rw <- max(nchar("Std. Residuals"), nchar(data$std))
  w <- sum(lw, ow, ew, dw, rw, 16)
  names <- c(0, 1)

  if (data$alt == "less") {
    cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(format("Sample Size", width = cwidth, justify = "left"), formats(), format(data$n, width = nwidth, justify = "right"), "\n")
    cat(format("Exp Prop", width = cwidth, justify = "left"), formats(), format(data$p, width = nwidth, justify = "right"), "\n")
    cat(format("Obs Prop", width = cwidth, justify = "left"), formats(), format(data$phat, width = nwidth, justify = "right"), "\n")
    cat(format("z", width = cwidth, justify = "left"), formats(), format(data$z, width = nwidth, justify = "right"), "\n")
    cat(format("Pr(Z < z)", width = cwidth, justify = "left"), formats(), format(data$sig, width = nwidth, justify = "right"), "\n\n")
  } else if (data$alt == "greater") {
    cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(format("Sample Size", width = cwidth, justify = "left"), formats(), format(data$n, width = nwidth, justify = "right"), "\n")
    cat(format("Exp Prop", width = cwidth, justify = "left"), formats(), format(data$p, width = nwidth, justify = "right"), "\n")
    cat(format("Obs Prop", width = cwidth, justify = "left"), formats(), format(data$phat, width = nwidth, justify = "right"), "\n")
    cat(format("z", width = cwidth, justify = "left"), formats(), format(data$z, width = nwidth, justify = "right"), "\n")
    cat(format("Pr(Z > z)", width = cwidth, justify = "left"), formats(), format(data$sig, width = nwidth, justify = "right"), "\n\n")
  } else if (data$alt == "both") {
    cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(format("Sample Size", width = cwidth, justify = "left"), formats(), format(data$n, width = nwidth, justify = "right"), "\n")
    cat(format("Exp Prop", width = cwidth, justify = "left"), formats(), format(data$p, width = nwidth, justify = "right"), "\n")
    cat(format("Obs Prop", width = cwidth, justify = "left"), formats(), format(data$phat, width = nwidth, justify = "right"), "\n")
    cat(format("z", width = cwidth, justify = "left"), formats(), format(data$z, width = nwidth, justify = "right"), "\n")
    cat(format("Pr(|Z| > |z|)", width = cwidth, justify = "left"), formats(), format(data$sig, width = nwidth, justify = "right"), "\n\n")
  } else {
    cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(format("Sample Size", width = cwidth, justify = "left"), formats(), format(data$n, width = nwidth, justify = "right"), "\n")
    cat(format("Exp Prop", width = cwidth, justify = "left"), formats(), format(data$p, width = nwidth, justify = "right"), "\n")
    cat(format("Obs Prop", width = cwidth, justify = "left"), formats(), format(data$phat, width = nwidth, justify = "right"), "\n")
    cat(format("z", width = cwidth, justify = "left"), formats(), format(data$z, width = nwidth, justify = "right"), "\n")
    cat(format("Pr(|Z| > |z|)", width = cwidth, justify = "left"), formats(), format(unname(data$sig[1]), width = nwidth, justify = "right"), "\n")
    cat(format("Pr(Z < z)", width = cwidth, justify = "left"), formats(), format(unname(data$sig[2]), width = nwidth, justify = "right"), "\n")
    cat(format("Pr(Z > z)", width = cwidth, justify = "left"), formats(), format(unname(data$sig[3]), width = nwidth, justify = "right"), "\n\n")
  }

  cat(rep("-", w), sep = "", "\n")
  cat(fg("Category", lw), fs(), fg("Observed", ow), fs(), fg("Expected", ew), fs(), fg("% Deviation", dw), fs(), fg("Std. Residuals", rw), "\n")
  cat(rep("-", w), sep = "", "\n")
  for (i in seq_len(length(data$obs))) {
    cat(
      fg(names[i], lw), fs(), fg(data$obs[i], ow), fs(), fg(data$exp[i], ew), fs(),
      fg(data$deviation[i], dw), fs(), fg(data$std[i], rw), "\n"
    )
  }
  cat(rep("-", w), sep = "", "\n")
}

print_ts_prop_test <- function(data) {
  cwidth <- max(nchar("z"), nchar("Pr(|Z| > |z|)"), nchar("Total Observations"))
  nwidth <- max(nchar(data$z), nchar(data$sig[1]), nchar(data$n1), nchar(data$n2))
  w1 <- sum(cwidth, nwidth, 6)

  totobs <- sum(data$n1, data$n2)

  if (data$alt == "less") {
    cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(format("Total Observations", width = cwidth, justify = "left"), formats(), format(totobs, width = nwidth, justify = "right"), "\n")
    cat(format("z", width = cwidth, justify = "left"), formats(), format(data$z, width = nwidth, justify = "right"), "\n")
    cat(format("Pr(Z < z)", width = cwidth, justify = "left"), formats(), format(data$sig, width = nwidth, justify = "right"), "\n\n")
  } else if (data$alt == "greater") {
    cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(format("Total Observations", width = cwidth, justify = "left"), formats(), format(totobs, width = nwidth, justify = "right"), "\n")
    cat(format("z", width = cwidth, justify = "left"), formats(), format(data$z, width = nwidth, justify = "right"), "\n")
    cat(format("Pr(Z > z)", width = cwidth, justify = "left"), formats(), format(data$sig, width = nwidth, justify = "right"), "\n\n")
  } else if (data$alt == "both") {
    cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(format("Total Observations", width = cwidth, justify = "left"), formats(), format(totobs, width = nwidth, justify = "right"), "\n")
    cat(format("z", width = cwidth, justify = "left"), formats(), format(data$z, width = nwidth, justify = "right"), "\n")
    cat(format("Pr(|Z| < |z|)", width = cwidth, justify = "left"), formats(), format(data$sig, width = nwidth, justify = "right"), "\n\n")
  } else {
    cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
    cat(rep("-", w1), sep = "", "\n")
    cat(format("Total Observations", width = cwidth, justify = "left"), formats(), format(totobs, width = nwidth, justify = "right"), "\n")
    cat(format("z", width = cwidth, justify = "left"), formats(), format(data$z, width = nwidth, justify = "right"), "\n")
    cat(format("Pr(|Z| < |z|)", width = cwidth, justify = "left"), formats(), format(unname(data$sig[1]), width = nwidth, justify = "right"), "\n")
    cat(format("Pr(Z < z)", width = cwidth, justify = "left"), formats(), format(unname(data$sig[2]), width = nwidth, justify = "right"), "\n")
    cat(format("Pr(Z > z)", width = cwidth, justify = "left"), formats(), format(unname(data$sig[3]), width = nwidth, justify = "right"), "\n\n")
  }
}


print_os_vartest <- function(data) {
  null_l <- paste0("Ho: sd(", data$var_name, ") >= ", as.character(data$sd))
  alt_l <- paste0(" Ha: sd(", data$var_name, ") < ", as.character(data$sd))
  null_u <- paste0("Ho: sd(", data$var_name, ") <= ", as.character(data$sd))
  alt_u <- paste0("Ha: sd(", data$var_name, ") > ", as.character(data$sd))
  null_t <- paste0("Ho: sd(", data$var_name, ") = ", as.character(data$sd))
  alt_t <- paste0("Ha: sd(", data$var_name, ") != ", as.character(data$sd))
  all_l <- paste("Ha: sd <", as.character(data$sd))
  all_u <- paste("Ha: sd >", as.character(data$sd))
  all_t <- paste("Ha: sd !=", as.character(data$sd))
  char_p_l <- format(round(data$p_lower, 4), nsmall = 4, scientific = FALSE)
  char_p_u <- format(round(data$p_upper, 4), nsmall = 4, scientific = FALSE)
  char_p   <- format(round(data$p_two, 4), nsmall = 4, scientific = FALSE)
  all_p_l <- paste("Pr(C < c) =", char_p_l)
  if (data$p_lower < 0.5) {
    all_p_t <- paste("2 * Pr(C < c) =", char_p)
  } else {
    all_p_t <- paste("2 * Pr(C > c) =", char_p)
  }
  all_p_u <- paste("Pr(C > c) =", char_p_u)
  all_tval <- paste0(" c = ", as.character(data$chi))


  # formatting output
  # compute the characters of each output and decide the overall width
  var_width <- max(nchar("Variable"), nchar(data$var_name))
  obs_width <- max(nchar("Obs"), nchar(data$n))
  mean_width <- max(nchar("Mean"), nchar(data$xbar))
  se_width <- max(nchar("Std. Err."), nchar(data$se))
  sd_width <- max(nchar("Std. Dev."), nchar(data$sigma))
  conf_length <- nchar(data$c_lwr) + nchar(data$c_upr)
  conf_str <- paste0("[", data$conf * 100, "% Conf. Interval]")
  confint_length <- nchar(conf_str)
  if (conf_length > confint_length) {
    conf_width <- round(conf_length / 2)
  } else {
    conf_width <- round(confint_length / 2)
  }
  c_width <- nchar(data$chi)
  df_width <- max(nchar("DF"), nchar(data$df))
  p_width <- max(nchar("2 Tailed"), nchar(round(data$p_two, 5)))
  md_width <- max(nchar("Difference"), nchar(data$mean_diff))
  md_length <- nchar(data$mean_diff_l) + nchar(data$mean_diff_u)

  width_1 <- sum(var_width, obs_width, mean_width, se_width, sd_width, ceiling(conf_width * 2), 21)
  width_2 <- sum(var_width, c_width, df_width, p_width, 12)
  all_width <- round(width_1 / 3)
  width_3 <- all_width * 3

  cat(
    format("One-Sample Statistics", width = width_1, justify = "centre"),
    "\n"
  )
  cat(rep("-", width_1), sep = "")
  cat(
    "\n", formatter_t("Variable", var_width), formats_t(),
    formatter_t("Obs", obs_width), formats_t(),
    formatter_t("Mean", mean_width),
    formats_t(), formatter_t("Std. Err.", se_width), formats_t(),
    formatter_t("Std. Dev.", sd_width), formats_t(),
    formatter_t(conf_str, conf_width), "\n"
  )
  cat(rep("-", width_1), sep = "")
  cat(
    "\n", formatter_t(data$var_name, var_width), formats_t(),
    formatter_t(data$n, obs_width), formats_t(),
    formatter_t(data$xbar, mean_width),
    formats_t(), formatter_t(data$se, se_width), formats_t(),
    formatter_t(data$sigma, sd_width), formats_t(),
    format_cil(data$c_lwr, conf_width),
    format_ciu(data$c_upr, conf_width), "\n"
  )
  cat(rep("-", width_1), sep = "")

  # print result
  if (data$type == "less") {
    cat("\n\n", format("Lower Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"))
    cat("\n", format(null_l, width = width_2, justify = "centre"))
    cat("\n", format(alt_l, width = width_2, justify = "centre"), "\n\n")
    cat(format("Chi-Square Test for Variance", width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t("Variable", var_width), formats_t(), formatter_t("c", c_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), formats_t(), "\n"
    )
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$chi, 3), c_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(char_p_l, p_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
  } else if (data$type == "greater") {
    cat("\n\n", format("Upper Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"))
    cat("\n", format(null_u, width = width_2, justify = "centre"))
    cat("\n", format(alt_u, width = width_2, justify = "centre"), "\n\n")
    cat(format("Chi-Square Test for Variance", width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t("Variable", var_width), formats_t(), formatter_t("c", c_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$chi, 3), c_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(char_p_u, p_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
  } else if (data$type == "both") {
    cat("\n\n", format("Two Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"))
    cat("\n", format(null_t, width = width_2, justify = "centre"))
    cat("\n", format(alt_t, width = width_2, justify = "centre"), "\n\n")
    cat(format("Chi-Square Test for Variance", width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t("Variable", var_width), formats_t(), formatter_t("c", c_width), formats_t(), formatter_t("DF", df_width), formats_t(),
      formatter_t("Sig", p_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t(data$var_name, var_width), formats_t(),
      formatter_t(round(data$chi, 3), c_width), formats_t(),
      formatter_t(data$df, df_width), formats_t(),
      formatter_t(char_p, p_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
  } else {
    cat("\n\n", format(null_t, width = width_3, justify = "centre"))
    cat("\n\n", format(all_l, width = all_width, justify = "centre"), format(all_t, width = all_width, justify = "centre"), format(all_u, width = all_width, justify = "centre"), "\n")
    cat(format(all_tval, width = all_width, justify = "centre"), format(all_tval, width = all_width, justify = "centre"), format(all_tval, width = all_width, justify = "centre"))
    cat("\n", format(all_p_l, width = all_width, justify = "centre"), format(all_p_t, width = all_width, justify = "centre"), format(all_p_u, width = all_width, justify = "centre"))
  }
}


print_chisq_test <- function(x) {
  width1 <- nchar("Likelihood Ratio Chi-Square")
  width2 <- max(nchar(x$df))
  width3 <- max(
    nchar(x$chisquare), nchar(x$chisquare_lr), nchar(x$chisquare_mantel_haenszel), nchar(x$chisquare_adjusted), nchar(x$phi_coefficient),
    nchar(x$contingency_coefficient), nchar(x$cramers_v)
  )
  width4 <- 6
  widthn <- sum(width1, width2, width3, width4, 12)

  if (x$ds == 4) {
    cat(format("Chi Square Statistics", width = widthn, justify = "centre"), "\n\n")
    cat(
      format("Statistics", width = width1, justify = "left"), formats(), format("DF", width = width2, justify = "centre"), formats(),
      format("Value", width = width3, justify = "centre"), formats(), format("Prob", width = width4, justify = "centre"), "\n", sep = ""
    )
    cat(rep("-", widthn), sep = "", "\n")
    cat(
      format("Chi-Square", width = width1, justify = "left"), formats(), format(x$df, width = width2, justify = "centre"), formats(),
      format(x$chisquare, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(x$pval_chisquare, width = width4, justify = "right", nsmall = 4, scientific = F), "\n", sep = ""
    )
    cat(
      format("Likelihood Ratio Chi-Square", width = width1, justify = "left"), formats(), format(x$df, width = width2, justify = "centre"), formats(),
      format(x$chisquare_lr, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(x$pval_chisquare_lr, width = width4, justify = "right", nsmall = 4, scientific = F), "\n", sep = ""
    )
    cat(
      format("Continuity Adj. Chi-Square", width = width1, justify = "left"), formats(), format(x$df, width = width2, justify = "right"), formats(),
      format(x$chisquare_adjusted, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(x$pval_chisquare_adjusted, width = width4, justify = "right", nsmall = 4, scientific = F), "\n", sep = ""
    )
    cat(
      format("Mantel-Haenszel Chi-Square", width = width1, justify = "left"), formats(), format(x$df, width = width2, justify = "right"), formats(),
      format(x$chisquare_mantel_haenszel, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(x$pval_chisquare_mantel_haenszel, width = width4, justify = "right", nsmall = 4, scientific = F), "\n", sep = ""
    )
    cat(
      format("Phi Coefficient", width = width1, justify = "left"), formats(), format(" ", width = width2, justify = "right"), formats(),
      format(x$phi_coefficient, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(" ", width = width4, justify = "right"), "\n", sep = ""
    )
    cat(
      format("Contingency Coefficient", width = width1, justify = "left"), formats(), format(" ", width = width2, justify = "right"), formats(),
      format(x$contingency_coefficient, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(" ", width = width4, justify = "right"), "\n", sep = ""
    )
    cat(
      format("Cramer's V", width = width1, justify = "left"), formats(), format(" ", width = width2, justify = "right"), formats(),
      format(x$cramers_v, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(" ", width = width4, justify = "right"), "\n", sep = ""
    )
    cat(rep("-", widthn), sep = "", "\n")
  } else {
    cat(format("Chi Square Statistics", width = widthn, justify = "centre"), "\n\n")
    cat(
      format("Statistics", width = width1, justify = "left"), formats(), format("DF", width = width2, justify = "centre"), formats(),
      format("Value", width = width3, justify = "centre"), formats(), format("Prob", width = width4, justify = "centre"), "\n", sep = ""
    )
    cat(rep("-", widthn), sep = "", "\n")
    cat(
      format("Chi-Square", width = width1, justify = "left"), formats(), format(x$df, width = width2, justify = "centre"), formats(),
      format(x$chisquare, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(x$pval_chisquare, width = width4, justify = "right", nsmall = 4, scientific = F), "\n", sep = ""
    )
    cat(
      format("Likelihood Ratio Chi-Square", width = width1, justify = "left"), formats(), format(x$df, width = width2, justify = "centre"), formats(),
      format(x$chisquare_lr, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(x$pval_chisquare_lr, width = width4, justify = "right", nsmall = 4, scientific = F), "\n", sep = ""
    )
    cat(
      format("Phi Coefficient", width = width1, justify = "left"), formats(), format(" ", width = width2, justify = "right"), formats(),
      format(x$phi_coefficient, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(" ", width = width4, justify = "right"), "\n", sep = ""
    )
    cat(
      format("Contingency Coefficient", width = width1, justify = "left"), formats(), format(" ", width = width2, justify = "right"), formats(),
      format(x$contingency_coefficient, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(" ", width = width4, justify = "right"), "\n", sep = ""
    )
    cat(
      format("Cramer's V", width = width1, justify = "left"), formats(), format(" ", width = width2, justify = "right"), formats(),
      format(x$cramers_v, width = width3, justify = "centre", nsmall = 4, scientific = F), formats(), format(" ", width = width4, justify = "right"), "\n", sep = ""
    )
    cat(rep("-", widthn), sep = "", "\n")
  }
}


print_chisq_gof <- function(data) {
  cwidth <- max(nchar("Chi-Square"), nchar("DF"), nchar("Pr > Chi Sq"), nchar("Sample Size"))
  nwidth <- max(nchar(data$chisquare), nchar(data$degrees_of_freedom), nchar(data$pvalue), 
    nchar(data$sample_size))
  w1 <- sum(cwidth, nwidth, 6)
  lw <- max(nchar("Variable"), nchar(data$categories))
  ow <- max(nchar("Observed"), nchar(data$observed_frequency))
  ew <- max(nchar("Expected"), nchar(data$expected_frequency))
  dw <- max(nchar("% Deviation"), nchar(data$deviation))
  rw <- max(nchar("Std. Residuals"), nchar(data$std_residuals))
  w <- sum(lw, ow, ew, dw, rw, 16)


  cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(format("Chi-Square", width = cwidth, justify = "left"), formats(), format(data$chisquare, width = nwidth, justify = "right"), "\n")
  cat(format("DF", width = cwidth, justify = "left"), formats(), format(data$degrees_of_freedom, width = nwidth, justify = "right"), "\n")
  cat(format("Pr > Chi Sq", width = cwidth, justify = "left"), formats(), format(data$pvalue, width = nwidth, justify = "right"), "\n")
  cat(format("Sample Size", width = cwidth, justify = "left"), formats(), format(data$sample_size, width = nwidth, justify = "right"), "\n\n")
  cat(format(paste("Variable:", data$varname), width = w, justify = "centre"), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(fg("Category", lw), fs(), fg("Observed", ow), fs(), fg("Expected", ew), fs(), fg("% Deviation", dw), fs(), fg("Std. Residuals", rw), "\n")
  cat(rep("-", w), sep = "", "\n")
  for (i in seq_len(data$n_levels)) {
    cat(
      fg(data$categories[i], lw), fs(), fg(data$observed_frequency[i], ow), fs(), 
      fg(data$expected_frequency[i], ew), fs(), fg(data$deviation[i], dw), fs(), 
      fg(data$std_residuals[i], rw), "\n"
    )
  }
  cat(rep("-", w), sep = "", "\n")
}

print_runs_test <- function(x) {
  cat(
    "Runs Test\n",
    "Total Cases: ", x$n, "\n",
    "Test Value : ", x$threshold, "\n",
    "Cases < Test Value: ", x$n_below, "\n",
    "Cases > Test Value: ", x$n_above, "\n",
    "Number of Runs: ", x$n_runs, "\n",
    "Expected Runs: ", x$mean, "\n",
    "Variance (Runs): ", x$var, "\n",
    "z Statistic: ", x$z, "\n",
    "p-value: ", x$p, "\n"
  )
}


print_cochran_test <- function(data) {
  cwidth <- max(nchar("N"), nchar("Cochran's Q"), nchar("df"), nchar("p value"))
  nwidth <- max(nchar(data$n), nchar(data$q), nchar(data$df), nchar(data$pvalue))
  w1 <- sum(cwidth, nwidth, 6)

  cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(format("N", width = cwidth, justify = "left"), formats(), format(data$n, width = nwidth, justify = "right"), "\n")
  cat(format("Cochran's Q", width = cwidth, justify = "left"), formats(), format(data$q, width = nwidth, justify = "right"), "\n")
  cat(format("df", width = cwidth, justify = "left"), formats(), format(data$df, width = nwidth, justify = "right"), "\n")
  cat(format("p value", width = cwidth, justify = "left"), formats(), format(data$pvalue, width = nwidth, justify = "right"), "\n")
  cat(rep("-", w1), sep = "", "\n")
}


print_mcnemar_test <- function(data) {
  cwidth1 <- max(
    nchar("McNemar's chi2"), nchar("DF"), nchar("Pr > chi2"),
    nchar("Exact Pr >= chi2")
  )
  nwidth1 <- max(
    nchar(data$tatistic), nchar(data$df), nchar(data$pvalue),
    nchar(data$exactp)
  )
  w1 <- sum(cwidth1, nwidth1, 6)

  cwidth2 <- max(
    nchar("Kappa"), nchar("ASE"), nchar("95% Lower Conf Limit"),
    nchar("95% Upper Conf Limit")
  )
  nwidth2 <- max(
    nchar(data$kappa), nchar(data$std_err), nchar(data$kappa_cil),
    nchar(data$kappa_ciu)
  )
  w2 <- sum(cwidth2, nwidth2, 6)

  cwidth3 <- max(
    nchar("Cases"), nchar("Controls"), nchar("Ratio"),
    nchar("Odds Ratio")
  )
  nwidth3 <- max(
    nchar(data$cases), nchar(data$controls), nchar(data$ratio),
    nchar(data$odratio)
  )
  w3 <- sum(cwidth3, nwidth3, 6)

  tcs <- colSums(data$tbl)
  trs <- rowSums(data$tbl)
  twidth1 <- 5
  twidth2 <- max(nchar(data$tbl[, 1]), nchar(tcs[1]))
  twidth3 <- max(nchar(data$tbl[, 2]), nchar(tcs[2]))
  twidth4 <- max(5, nchar(sum(trs)))
  w4 <- sum(twidth1, twidth2, twidth3, twidth4, 18)
  twidth5 <- sum(twidth2, twidth3)

  cat(
    format("     ", width = twidth1, justify = "centre"), formats(),
    format("Controls", width = twidth5, justify = "left"), "\n"
  )
  cat(rep("-", w4), sep = "", "\n")
  cat(
    format("Cases", width = twidth1, justify = "centre"), formats(),
    format("0", width = twidth2, justify = "centre"), formats(),
    format("1", width = twidth3, justify = "centre"), formats(),
    format("Total", width = twidth4, justify = "centre"), "\n"
  )
  cat(rep("-", w4), sep = "", "\n")
  cat(
    format("0", width = twidth1, justify = "centre"), formats(),
    format(data$tbl[1, 1], width = twidth2, justify = "centre"), formats(),
    format(data$tbl[1, 2], width = twidth3, justify = "centre"), formats(),
    format(trs[1], width = twidth4, justify = "centre"), "\n"
  )
  cat(
    format("1", width = twidth1, justify = "centre"), formats(),
    format(data$tbl[2, 1], width = twidth2, justify = "centre"), formats(),
    format(data$tbl[2, 2], width = twidth3, justify = "centre"), formats(),
    format(trs[2], width = twidth4, justify = "centre"), "\n"
  )
  cat(rep("-", w4), sep = "", "\n")
  cat(
    format("Total", width = twidth1, justify = "centre"), formats(),
    format(tcs[1], width = twidth2, justify = "centre"), formats(),
    format(tcs[2], width = twidth3, justify = "centre"), formats(),
    format(sum(trs), width = twidth4, justify = "centre"), "\n"
  )
  cat(rep("-", w4), sep = "", "\n\n")


  cat(format("McNemar's Test", width = w1, justify = "centre"), "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(format("McNemar's chi2", width = cwidth1, justify = "left"), formats(), format(data$statistic, width = nwidth1, justify = "right"), "\n")
  cat(format("DF", width = cwidth1, justify = "left"), formats(), format(data$df, width = nwidth1, justify = "right"), "\n")
  cat(format("Pr > chi2", width = cwidth1, justify = "left"), formats(), format(data$pvalue, width = nwidth1, justify = "right"), "\n")
  cat(format("Exact Pr >= chi2", width = cwidth1, justify = "left"), formats(), format(data$exactp, width = nwidth1, justify = "right"), "\n")
  cat(rep("-", w1), sep = "", "\n\n")

  cat(format("Kappa Coefficient", width = w2, justify = "centre"), "\n")
  cat(rep("-", w2), sep = "", "\n")
  cat(format("Kappa", width = cwidth2, justify = "left"), formats(), format(data$kappa, width = nwidth2, justify = "right"), "\n")
  cat(format("ASE", width = cwidth2, justify = "left"), formats(), format(data$std_err, width = nwidth2, justify = "right"), "\n")
  cat(format("95% Lower Conf Limit", width = cwidth2, justify = "left"), formats(), format(data$kappa_cil, width = nwidth2, justify = "right"), "\n")
  cat(format("95% Upper Conf Limit", width = cwidth2, justify = "left"), formats(), format(data$kappa_ciu, width = nwidth2, justify = "right"), "\n")
  cat(rep("-", w2), sep = "", "\n\n")

  cat(format("Proportion With Factor", width = w3, justify = "centre"), "\n")
  cat(rep("-", w3), sep = "", "\n")
  cat(format("cases", width = cwidth3, justify = "left"), formats(), format(data$cases, width = nwidth3, justify = "right"), "\n")
  cat(format("controls", width = cwidth3, justify = "left"), formats(), format(data$controls, width = nwidth3, justify = "right"), "\n")
  cat(format("ratio", width = cwidth3, justify = "left"), formats(), format(data$ratio, width = nwidth3, justify = "right"), "\n")
  cat(format("odds ratio", width = cwidth3, justify = "left"), formats(), format(data$odratio, width = nwidth3, justify = "right"), "\n")
  cat(rep("-", w3), sep = "", "\n")
}


print_levene_test <- function(data) {
  lw <- max(nchar("Levels"), nchar(data$levs), nchar("Total"))
  ow <- max(nchar("Frequency"), nchar(data$lens), nchar(data$n))
  ew <- max(nchar("Mean"), nchar(data$avgs), nchar(data$avg))
  dw <- max(nchar("Std. Dev."), nchar(data$sds), nchar(data$sd))
  w <- sum(lw, ow, ew, dw, 12)

  cwidth <- max(
    nchar("Statistic"), nchar("Brown and Forsythe"), nchar("Levene"),
    nchar("Brown and Forsythe (Trimmed Mean)")
  )
  nwidth <- max(nchar("Num DF"), nchar(data$n_df))
  dwidth <- max(nchar("Den DF"), nchar(data$d_df))
  ewidth <- max(nchar("F"), nchar(data$bf), nchar(data$lev), nchar(data$bft))
  fwidth <- max(nchar("Pr > F"), nchar(data$p_bf), nchar(data$p_lev), nchar(data$p_bft))
  w1 <- sum(cwidth, nwidth, dwidth, ewidth, fwidth, 16)

  cat(format("Summary Statistics", width = w, justify = "centre"), "\n")
  cat(
    fg("Levels", lw), fs(), fg("Frequency", ow), fs(), fg("Mean", ew), fs(),
    fg("Std. Dev", dw), "\n"
  )
  cat(rep("-", w), sep = "", "\n")
  for (i in seq_len(length(data$levs))) {
    cat(
      fg(data$levs[i], lw), fs(), fg(data$lens[i], ow), fs(), fg(data$avgs[i], ew), fs(),
      fg(data$sds[i], dw), "\n"
    )
  }
  cat(rep("-", w), sep = "", "\n")
  cat(
    fg("Total", lw), fs(), fg(data$n, ow), fs(), fg(data$avg, ew), fs(),
    fg(data$sd, dw), "\n"
  )
  cat(rep("-", w), sep = "", "\n\n")

  cat(format("Test Statistics", width = w1, justify = "centre"), "\n")
  cat(rep("-", w1), sep = "", "\n")
  cat(
    format("Statistic", width = cwidth, justify = "left"), fs(),
    format("Num DF", width = nwidth, justify = "right"), fs(), format("Den DF", width = dwidth, justify = "right"),
    fs(), format("F", width = ewidth, justify = "right"), fs(), format("Pr > F", width = fwidth, justify = "right"), "\n"
  )
  cat(rep("-", w1), sep = "", "\n")
  cat(
    format("Brown and Forsythe", width = cwidth, justify = "left"), fs(),
    format(data$n_df, width = nwidth, justify = "right"), fs(), format(data$d_df, width = dwidth, justify = "right"),
    fs(), format(data$bf, width = ewidth, justify = "right"), fs(), format(data$p_bf, width = fwidth, justify = "right"), "\n"
  )
  cat(
    format("Levene", width = cwidth, justify = "left"), fs(),
    format(data$n_df, width = nwidth, justify = "right"), fs(), format(data$d_df, width = dwidth, justify = "right"),
    fs(), format(data$lev, width = ewidth, justify = "right"), fs(), format(data$p_lev, width = fwidth, justify = "right"), "\n"
  )
  cat(
    format("Brown and Forsythe (Trimmed Mean)", width = cwidth, justify = "left"), fs(),
    format(data$n_df, width = nwidth, justify = "right"), fs(), format(data$d_df, width = dwidth, justify = "right"),
    fs(), format(data$bft, width = ewidth, justify = "right"), fs(), format(data$p_bft, width = fwidth, justify = "right"), "\n"
  )
  cat(rep("-", w1), sep = "", "\n")
}


print_var_test <- function(data) {
  var_width <- max(nchar("combined"), nchar(data$lev))
  obs_width <- max(nchar("Obs"), nchar(data$lens), nchar(data$len))
  mean_width <- max(nchar("Mean"), nchar(data$avgs), nchar(data$avg))
  se_width <- max(nchar("Std. Err."), nchar(data$ses), nchar(data$se))
  sd_width <- max(nchar("Std. Dev."), nchar(data$sds), nchar(data$sd))
  width_1 <- sum(var_width, obs_width, mean_width, se_width, sd_width, 16)

  rto <- paste0("ratio = sd(", data$lev[1], ") / (", data$lev[2], ")")
  nhyp <- "Ho: ratio = 1"
  lhyp <- "Ha: ratio < 1"
  uhyp <- "Ha: ratio > 1"
  char_p_l   <- format(round(data$lower, 4), nsmall = 4, scientific = FALSE)
  char_p_u   <- format(round(data$upper, 4), nsmall = 4, scientific = FALSE)
  all_p_l <- paste("Pr(F < f) =", char_p_l)
  all_p_u <- paste("Pr(F > f) =", char_p_u)


  f_width <- nchar(data$f)
  df1_width <- max(nchar("Num DF"), nchar(data$n1))
  df2_width <- max(nchar("Den DF"), nchar(data$n2))
  p_width <- max(nchar("p"), nchar(char_p_l))
  width_2 <- sum(f_width, df1_width, df2_width, p_width, 12)
  width_3 <- sum(f_width, df1_width, df2_width, 8)
  all_width <- sum(nchar(all_p_l), nchar(all_p_u), 4)

  cat(
    format("Variance Ratio Test", width = width_1, justify = "centre"),
    "\n"
  )
  cat(rep("-", width_1), sep = "")
  cat(
    "\n", formatter_t("Group", var_width), formats_t(),
    formatter_t("Obs", obs_width), formats_t(),
    formatter_t("Mean", mean_width),
    formats_t(), formatter_t("Std. Err.", se_width), formats_t(),
    formatter_t("Std. Dev.", sd_width), "\n"
  )
  cat(rep("-", width_1), sep = "", "\n")
  for (i in seq_len(length(data$avgs))) {
    cat(
      formatter_t(data$lev[i], var_width), formats_t(),
      formatter_t(data$lens[i], obs_width), formats_t(),
      formatter_t(data$avgs[i], mean_width),
      formats_t(), formatter_t(data$ses[i], se_width), formats_t(),
      formatter_t(data$sds[i], sd_width), "\n"
    )
  }
  cat(rep("-", width_1), sep = "")
  cat(
    "\n", formatter_t("combined", var_width), formats_t(),
    formatter_t(data$len, obs_width), formats_t(),
    formatter_t(data$avg, mean_width),
    formats_t(), formatter_t(data$se, se_width), formats_t(),
    formatter_t(data$sd, sd_width), "\n"
  )
  cat(rep("-", width_1), sep = "")

  if (data$type == "less") {
    cat("\n\n", format("Lower Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"))
    cat("\n", format(rto, width = width_2, justify = "centre"))
    cat("\n", format(nhyp, width = width_2, justify = "centre"))
    cat("\n", format(lhyp, width = width_2, justify = "centre"), "\n\n")
    cat(format("Variance Ratio Test", width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t("F", f_width), formats_t(), formatter_t("Num DF", df1_width),
      formats_t(), formatter_t("Den DF", df2_width), formats_t(),
      formatter_t("p", p_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t(data$f, f_width), formats_t(),
      formatter_t(data$n1, df1_width), formats_t(),
      formatter_t(data$n2, df2_width), formats_t(),
      formatter_t(char_p_l, p_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
  } else if (data$type == "greater") {
    cat("\n\n", format("Upper Tail Test", width = width_2, justify = "centre"))
    cat("\n", format("---------------", width = width_2, justify = "centre"))
    cat("\n", format(nhyp, width = width_2, justify = "centre"))
    cat("\n", format(uhyp, width = width_2, justify = "centre"), "\n\n")
    cat(format("Variance Ratio Test", width = width_2, justify = "centre"), "\n")
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t("F", f_width), formats_t(), formatter_t("Num DF", df1_width),
      formats_t(), formatter_t("Den DF", df2_width), formats_t(),
      formatter_t("p", p_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
    cat(
      "\n", formatter_t(data$f, f_width), formats_t(),
      formatter_t(data$n1, df1_width), formats_t(),
      formatter_t(data$n2, df2_width), formats_t(),
      formatter_t(char_p_u, p_width), "\n"
    )
    cat(rep("-", width_2), sep = "")
  } else {
    cat("\n\n", format("Variance Ratio Test", width = width_1, justify = "centre"), "\n")
    cat(rep("-", width_1), sep = "")
    cat(
      "\n", formatter_t("F", (width_1 / 3)), formatter_t("Num DF", (width_1 / 3)),
      formatter_t("Den DF", (width_1 / 3)), "\n"
    )
    cat(rep("-", width_1), sep = "")
    cat(
      "\n", formatter_t(data$f, (width_1 / 3)),
      formatter_t(data$n1, (width_1 / 3)),
      formatter_t(data$n2, (width_1 / 3)), "\n"
    )
    cat(rep("-", width_1), sep = "")

    cat("\n\n", format("Null & Alternate Hypothesis", width = all_width, justify = "centre"), "\n")
    cat(rep("-", all_width), sep = "", "\n")
    cat(format(rto, width = all_width, justify = "centre"))
    cat("\n", format(nhyp, width = all_width, justify = "centre"))
    cat(
      "\n\n", format(lhyp, width = (all_width / 2), justify = "centre"),
      format(uhyp, width = (all_width / 2), justify = "centre")
    )
    cat(
      "\n", format(all_p_l, width = (all_width / 2), justify = "centre"),
      format(all_p_u, width = (all_width / 2), justify = "centre")
    )
    cat("\n", rep("-", all_width), sep = "")
  }
}
