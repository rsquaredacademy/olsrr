print_reg <- function(data) {

		# width
    # prints
    a <- c("R", "R-Squared", "Adj. R-Squared", "Pred R-Squared")
    b <- c(data$r, data$rsq, data$adjr, data$prsq)
    d <- c( "RMSE", "Coef. Var", "MSE", "MAE")
    e <- c(data$sigma, data$cv, data$mse, data$mae)

    w1 <- max(nchar(a))
    w2 <- max(nchar(format(b, nsmall = 3)))
    w3 <- max(nchar(d))
    w4 <- max(nchar(format(e, nsmall = 3)))
    w5 <- sum(w1, w2, w3, w4, 28)
    nw <- length(b)

    # model summary
    cat(fc('Model Summary', w5), '\n')
    cat(rep("-", w5), sep = "", '\n')
    for (i in seq_len(nw)) {
        cat(fl(a[i], w1), fs(), fs(), fs(), fg(b[i], w2), fs(), fs(), fl(d[i], w3), fs(), fs(), fs(), fg(e[i], w4), "\n")
    }
    cat(rep("-", w5), sep = "", '\n')
    cat(" RMSE: Root Mean Square Error", "\n", "MSE: Mean Square Error", "\n", "MAE: Mean Absolute Error", "\n\n")

    # anova
    w7 <- nchar('Regression')
    w8 <- max(nchar('Squares'), nchar(format(data$rss, nsmall = 3)), nchar(data$ess), nchar(data$tss))
    w9 <- max(nchar('DF'), nchar(data$model_df), nchar(data$error_df), nchar(data$total_df))
    w10 <- max(nchar('Mean Square'), nchar(data$rms), nchar(data$ems))
    w11 <- max(nchar('F'), nchar(data$f))
    w12 <- max(nchar('Sig.'), nchar(format(data$p, nsmall = 4)))
    w <- sum(w7, w8, w9, w10, w11, w12, 21)

    p <- format(data$p, nsmall = 4)

    # ANOVA
    cat(fc('ANOVA', w), '\n')
    cat(rep("-", w), sep = "", '\n')
    cat(fg('', w7), fs(), fg('Sum of', w8), fs(), fg('', w9), fs(), fg('', w10), fs(), fg('', w11), fs(), fg('', w12), '\n')
    cat(fg('', w7), fs(), fg('Squares', w8), fs(), fg('DF', w9), fs(), fg('Mean Square', w10), fs(), fc('F', w11), fs(), fg('Sig.', w12), '\n')
    cat(rep("-", w), sep = "", '\n')
    cat(fl('Regression', w7), fs(), fg(format(data$rss, nsmall = 3), w8), fs(), fg(data$model_df, w9), fs(), fg(format(data$rms, nsmall = 3), w10), fs(), fg(data$f, w11), fs(), fg(p, w12), '\n')
    cat(fl('Residual', w7), fs(), fg(format(data$ess, nsmall = 3), w8), fs(), fg(data$error_df, w9), fs(), fg(format(data$ems, nsmall = 3), w10), fs(), fg('', w11), fs(), fg('', w12), '\n')
    cat(fl('Total', w7), fs(), fg(format(data$tss, nsmall = 3), w8), fs(), fg(data$total_df, w9), fs(), fg('', w10), fs(), fg('', w11), fs(), fg('', w12), '\n')
    cat(rep("-", w), sep = "", '\n\n')

    # coefficients
    w13 <- max(nchar(data$title), nchar(data$mvars))
    w14 <- max(nchar('Beta'), nchar(format(data$betas, nsmall = 3)))
    w15 <- max(nchar('Std. Error'), nchar(format(data$std_errors, nsmall = 3)))
    w16 <- max(nchar('Std. Beta'), nchar(format(data$sbetas, nsmall = 3)))
    w17 <- max(nchar('t'), nchar(format(round(data$tvalues, 3), nsmall = 3)))
    w18 <- max(nchar('Sig.'), nchar(format(round(data$pvalues, 3), nsmall = 3)))
    w19 <- max(nchar('lower'), nchar(format(data$conf_lm[, 1], nsmall = 3)))
    w20 <- max(nchar('upper'), nchar(format(data$conf_lm[, 2], nsmall = 3)))
    w21 <- sum(w13, w14, w15, w16, w17, w18, w19, w20, 29)

    k <- length(data$mvars)

    sb <- c('', data$sbetas)

    cat(fc('Parameter Estimates', w21), '\n')
    cat(rep("-", w21), sep = "", '\n')
    cat(fg(data$title, w13), fs(), fg('Beta', w14), fs(), fg('Std. Error', w15), fs(), fg('Std. Beta', w16), fs(),
        fc('t', w17), fs(), fc('Sig', w18), fs(), fg('lower', w19), fs(), fg('upper', w20), '\n')
    cat(rep("-", w21), sep = "", '\n')
    for (i in seq_len(k)) {
        cat(fg(data$mvars[i], w13), fs(), fg(format(data$betas[i], nsmall = 3), w14),
         fs(), fg(format(data$std_errors[i], nsmall = 3), w15), fs(), fg(sb[i], w16),
         fs(), fg(format(data$tvalues[i], nsmall = 3), w17), fs(),
         fg(format(data$pvalues[i], nsmall = 3), w18), fs(),
         fg(as.vector(format(data$conf_lm[, 1], nsmall = 3))[i], w19), fs(),
         fg(as.vector(format(data$conf_lm[, 2], nsmall = 3))[i], w20), '\n')
    }
    cat(rep("-", w21), sep = "", '\n')

}


print_correlations <- function(data) {

		# number of rows
		nr <- nrow(data)
    vars <- rownames(data)
    cols <- colnames(data)

    # widths
    w1 <- max(nchar('Variable'), nchar(vars))
    w2 <- nchar(cols[1])
    w3 <- nchar(cols[2])
    w4 <- max(nchar(cols[3]), nchar(data[, 3]))
    w <- sum(w1, w2, w3, w4, 12)

    # print
    cat(fc('Correlations', w), '\n')
    cat(rep("-", w), sep = "", '\n')
    cat(fl('Variable', w1), fs(), fc('Zero Order', w2), fs(), fc('Partial', w3), fs(), fc('Part', w4), "\n")
    cat(rep("-", w), sep = "", '\n')
    for (i in seq_len(nr)) {
        cat(fl(vars[i], w1), fs(), fg(data[i, 1], w2), fs(), fg(data[i, 2], w3), fs(), fg(data[i, 3], w4), "\n")
    }
    cat(rep("-", w), sep = "", '\n')

}


print_ftest <- function(data) {

	# width
	w1 <- max(nchar(data$numdf), nchar(data$dendf),
		nchar(format(data$f, nsmall = 3)),
		nchar(format(data$p, nsmall = 3)))

	w <- w1 + 16

	cat('\n F Test for Heteroskedasticity\n')
			cat(" ", rep("-", 29), sep = "", '\n')
			cat(' Ho: Variance is homogenous\n', "Ha: Variance is not homogenous\n\n")


	if (data$fv == TRUE) {

		cat(' Variables: fitted values of', data$resp, '\n\n')

	} else if (data$rhs == TRUE) {

		cat(' Variables:', data$preds, '\n\n')

	} else {

		cat(' Variables:', data$vars, '\n\n')

	}

	cat(format('Test Summary', width = w, justify = 'centre'), '\n')
	cat(" ", rep("-", w), sep = "", '\n')
	cat(' Num DF     =   ', data$numdf, '\n',
			'Den DF     =   ', data$dendf, '\n',
			'F          =   ', format(data$f, nsmall = 3), '\n',
			'Prob > F   =   ', format(data$p, nsmall = 3), '\n')

}


print_bp_test <- function(data) {

	cat('\n', 'Breusch Pagan Test for Heteroskedasticity\n')
	cat(" ", rep("-", 41), sep = "", '\n',
		format(' Ho: the variance is constant', width = 41, justify = 'left'), '\n',
		format(" Ha: the variance is not constant", width = 41, justify = 'left'), "\n\n")


	if (data$fv) {

		if (data$rhs) {

			if (data$multiple) {

				w1 <- max(nchar(data$preds), 13)
				w2 <- max(nchar('chi2'), nchar(format(data$bp, nsmall = 4)))
				w3 <- max(nchar(length(data$preds)), nchar('df'))
				w4 <- max(nchar(format(data$p, nsmall = 4)))
				w <- sum(w1, w2, w3, w4, 13)

				ldp <- length(data$preds)
				ldp2 <- ldp + 1

				w5 <- 11 + sum(nchar(data$preds)) + ldp - 1

				cat(format('Data', width = w5, justify = 'centre'), '\n')
	            cat(" ", rep("-", w5), sep = "", '\n')
	            cat(' Response :', data$resp, '\n',
	                'Variables:', data$preds, '\n\n')


				if (data$padj == 'bonferroni') {

					cat(" ", format('Test Summary (Bonferroni p values)', width = w, justify = 'centre'), '\n')
	            	cat(" ", rep("-", w), sep = "", '\n')

				} else if (data$padj == 'sidak') {

					cat(" ", format('Test Summary (Sidak p values)', width = w, justify = 'centre'), '\n')
	            	cat(" ", rep("-", w), sep = "", '\n')

				} else if (data$padj == 'holm') {

					cat(" ", format('Test Summary (Holm\'s p values)', width = w, justify = 'centre'), '\n')
	            	cat(" ", rep("-", w), sep = "", '\n')

				} else {

					cat(" ", format('Test Summary (Unadjusted p values)', width = w, justify = 'centre'), '\n')
	            	cat(" ", rep("-", w), sep = "", '\n')

				}

				bp <- format(data$bp, nsmall = 4)
				p <- format(data$p, nsmall = 4)

				cat(' ', format('Variable', width = w1, justify = 'left'), fs(),
					format('chi2', width = w2, justify = 'centre'), fs(),
					format('df', width = w3, justify = 'centre'), fs(),
					format('p', width = w4, justify = 'centre'), '\n')
				cat(" ", rep("-", w), sep = "", '\n')
				for (i in seq_len(ldp)) {
					cat(' ', format(data$preds[i], width = w1, justify = 'left'), fs(),
					format(bp[i], width = w2, justify = 'centre'), fs(),
					format('1', width = w3, justify = 'right'), fs(),
					format(p[i], width = w4, justify = 'centre'), '\n')
				}
				cat(" ", rep("-", w), sep = "", '\n')
				cat(' ', format('simultaneous', width = w1, justify = 'left'), fs(),
					format(bp[ldp2], width = w2, justify = 'centre'), fs(),
					format(ldp, width = w3, justify = 'right'), fs(),
					format(p[ldp2], width = w4, justify = 'centre'), '\n')
				cat(" ", rep("-", w), sep = "", '\n')


			} else {

				ldp <- length(data$preds)

				w1 <- 11 + sum(nchar(data$preds)) + ldp - 1

	            w3 <- max(nchar(ldp),
	                nchar(format(data$bp, nsmall = 4)),
	                nchar(format(data$p, nsmall = 4)))

	            w <- w3 + 19


	            cat(format('Data', width = w1, justify = 'centre'), '\n')
	            cat(" ", rep("-", w1), sep = "", '\n')
	            cat(' Response :', data$resp, '\n',
	                'Variables:', data$preds, '\n\n')

	            cat(format('Test Summary', width = w, justify = 'centre'), '\n')
	            cat(" ", rep("-", w), sep = "", '\n')
	            cat(' DF            =   ', ldp, '\n',
	                    'Chi2          =   ', format(data$bp, nsmall = 4), '\n',
	                    'Prob > Chi2   =   ', format(data$p, nsmall = 4), '\n')


			}

		} else {


			w1 <- nchar(data$resp)
			w2 <- w1 + 28

            w3 <- max(nchar('1'),
                nchar(format(data$bp, nsmall = 4)),
                nchar(format(data$p, nsmall = 4)))

            w <- w3 + 19


            cat(format('Data', width = w2, justify = 'centre'), '\n')
            cat(" ", rep("-", w2), sep = "", '\n')
            cat(' Response :', data$resp, '\n',
                'Variables: fitted values of', data$resp, '\n\n')

            cat(format('Test Summary', width = w, justify = 'centre'), '\n')
            cat(" ", rep("-", w), sep = "", '\n')
            cat(' DF            =   ', 1, '\n',
                    'Chi2          =   ', format(data$bp, nsmall = 4), '\n',
                    'Prob > Chi2   =   ', format(data$p, nsmall = 4), '\n')


		}


	} else {

		if (data$multiple) {

			if (data$rhs) {


				w1 <- max(nchar(data$preds), 13)
				w2 <- max(nchar('chi2'), nchar(format(data$bp, nsmall = 4)))
				w3 <- max(nchar(length(data$preds)), nchar('df'))
				w4 <- max(nchar(format(data$p, nsmall = 4)))
				w <- sum(w1, w2, w3, w4, 13)

				ldp <- length(data$preds)
				ldp2 <- ldp + 1

				w5 <- 11 + sum(nchar(data$preds)) + ldp - 1

				cat(format('Data', width = w5, justify = 'centre'), '\n')
	            cat(" ", rep("-", w5), sep = "", '\n')
	            cat(' Response :', data$resp, '\n',
	                'Variables:', data$preds, '\n\n')


				if (data$padj == 'bonferroni') {

					cat(" ", format('Test Summary (Bonferroni p values)', width = w, justify = 'centre'), '\n')
	            	cat(" ", rep("-", w), sep = "", '\n')

				} else if (data$padj == 'sidak') {

					cat(" ", format('Test Summary (Sidak p values)', width = w, justify = 'centre'), '\n')
	            	cat(" ", rep("-", w), sep = "", '\n')

				} else if (data$padj == 'holm') {

					cat(" ", format('Test Summary (Holm\'s p values)', width = w, justify = 'centre'), '\n')
	            	cat(" ", rep("-", w), sep = "", '\n')

				} else {

					cat(" ", format('Test Summary (Unadjusted p values)', width = w, justify = 'centre'), '\n')
	            	cat(" ", rep("-", w), sep = "", '\n')

				}

				bp <- format(data$bp, nsmall = 4)
				p <- format(data$p, nsmall = 4)

				cat(' ', format('Variable', width = w1, justify = 'left'), fs(),
					format('chi2', width = w2, justify = 'centre'), fs(),
					format('df', width = w3, justify = 'centre'), fs(),
					format('p', width = w4, justify = 'centre'), '\n')
				cat(" ", rep("-", w), sep = "", '\n')
				for (i in seq_len(ldp)) {
					cat(' ', format(data$preds[i], width = w1, justify = 'left'), fs(),
					format(bp[i], width = w2, justify = 'centre'), fs(),
					format('1', width = w3, justify = 'right'), fs(),
					format(p[i], width = w4, justify = 'centre'), '\n')
				}
				cat(" ", rep("-", w), sep = "", '\n')
				cat(' ', format('simultaneous', width = w1, justify = 'left'), fs(),
					format(bp[ldp2], width = w2, justify = 'centre'), fs(),
					format(ldp, width = w3, justify = 'right'), fs(),
					format(p[ldp2], width = w4, justify = 'centre'), '\n')
				cat(" ", rep("-", w), sep = "", '\n')




			} else {

				if (length(data$vars) > 1) {


					w1 <- max(nchar(data$vars), 13)
					w2 <- max(nchar('chi2'), nchar(format(data$bp, nsmall = 4)))
					w3 <- max(nchar(length(data$vars)), nchar('df'))
					w4 <- max(nchar(format(data$p, nsmall = 4)))
					w <- sum(w1, w2, w3, w4, 13)

					ldp <- length(data$vars)
					ldp2 <- ldp + 1

					w5 <- 11 + sum(nchar(data$vars)) + ldp - 1

					cat(format('Data', width = w5, justify = 'centre'), '\n')
		            cat(" ", rep("-", w5), sep = "", '\n')
		            cat(' Response :', data$resp, '\n',
		                'Variables:', data$vars, '\n\n')


					if (data$padj == 'bonferroni') {

						cat(" ", format('Test Summary (Bonferroni p values)', width = w, justify = 'centre'), '\n')
		            	cat(" ", rep("-", w), sep = "", '\n')

					} else if (data$padj == 'sidak') {

						cat(" ", format('Test Summary (Sidak p values)', width = w, justify = 'centre'), '\n')
		            	cat(" ", rep("-", w), sep = "", '\n')

					} else if (data$padj == 'holm') {

						cat(" ", format('Test Summary (Holm\'s p values)', width = w, justify = 'centre'), '\n')
		            	cat(" ", rep("-", w), sep = "", '\n')

					} else {

						cat(" ", format('Test Summary (Unadjusted p values)', width = w, justify = 'centre'), '\n')
		            	cat(" ", rep("-", w), sep = "", '\n')

					}

					bp <- format(data$bp, nsmall = 4)
					p <- format(data$p, nsmall = 4)

					cat(' ', format('Variable', width = w1, justify = 'left'), fs(),
						format('chi2', width = w2, justify = 'centre'), fs(),
						format('df', width = w3, justify = 'centre'), fs(),
						format('p', width = w4, justify = 'centre'), '\n')
					cat(" ", rep("-", w), sep = "", '\n')
					for (i in seq_len(ldp)) {
						cat(' ', format(data$vars[i], width = w1, justify = 'left'), fs(),
						format(bp[i], width = w2, justify = 'centre'), fs(),
						format('1', width = w3, justify = 'right'), fs(),
						format(p[i], width = w4, justify = 'centre'), '\n')
					}
					cat(" ", rep("-", w), sep = "", '\n')
					cat(' ', format('simultaneous', width = w1, justify = 'left'), fs(),
						format(bp[ldp2], width = w2, justify = 'centre'), fs(),
						format(ldp, width = w3, justify = 'right'), fs(),
						format(p[ldp2], width = w4, justify = 'centre'), '\n')
					cat(" ", rep("-", w), sep = "", '\n')



				} else {

					ldp <- length(data$vars)

					w1 <- 11 + sum(nchar(data$vars)) + ldp - 1

		            w3 <- max(nchar(ldp),
		                nchar(format(data$bp, nsmall = 4)),
		                nchar(format(data$p, nsmall = 4)))

		            w <- w3 + 19


		            cat(format('Data', width = w1, justify = 'centre'), '\n')
		            cat(" ", rep("-", w1), sep = "", '\n')
		            cat(' Response :', data$resp, '\n',
		                'Variables:', data$vars, '\n\n')

		            cat(format('Test Summary', width = w, justify = 'centre'), '\n')
		            cat(" ", rep("-", w), sep = "", '\n')
		            cat(' DF            =   ', ldp, '\n',
		                    'Chi2          =   ', format(data$bp, nsmall = 4), '\n',
		                    'Prob > Chi2   =   ', format(data$p, nsmall = 4), '\n')

					}

			}

		} else {

			if (data$rhs) {

				ldp <- length(data$preds)

				w1 <- 11 + sum(nchar(data$preds)) + ldp - 1

	            w3 <- max(nchar(ldp),
	                nchar(format(data$bp, nsmall = 4)),
	                nchar(format(data$p, nsmall = 4)))

	            w <- w3 + 19


	            cat(format('Data', width = w1, justify = 'centre'), '\n')
	            cat(" ", rep("-", w1), sep = "", '\n')
	            cat(' Response :', data$resp, '\n',
	                'Variables:', data$preds, '\n\n')

	            cat(format('Test Summary', width = w, justify = 'centre'), '\n')
	            cat(" ", rep("-", w), sep = "", '\n')
	            cat(' DF            =   ', ldp, '\n',
	                    'Chi2          =   ', format(data$bp, nsmall = 4), '\n',
	                    'Prob > Chi2   =   ', format(data$p, nsmall = 4), '\n')


			} else {

				lvars <- length(data$vars)

				w1 <- 11 + sum(nchar(data$vars)) + lvars - 1

	            w3 <- max(nchar(lvars),
	                nchar(format(data$bp, nsmall = 4)),
	                nchar(format(data$p, nsmall = 4)))

	            w <- w3 + 19


	            cat(format('Data', width = w1, justify = 'centre'), '\n')
	            cat(" ", rep("-", w1), sep = "", '\n')
	            cat(' Response :', data$resp, '\n',
	                'Variables:', data$vars, '\n\n')

	            cat(format('Test Summary', width = w, justify = 'centre'), '\n')
	            cat(" ", rep("-", w), sep = "", '\n')
	            cat(' DF            =   ', lvars, '\n',
	                    'Chi2          =   ', format(data$bp, nsmall = 4), '\n',
	                    'Prob > Chi2   =   ', format(data$p, nsmall = 4), '\n')

			}

		}

	}


}


print_levene_test <- function(data) {

	# format output
  bf <- format(data$bf, nsmall = 4)
  p_bf <- format(data$p_bf, nsmall = 4)
  lev <- format(data$lev, nsmall = 4)
  p_lev <- format(data$p_lev, nsmall = 4)
  bft <- format(data$bft, nsmall = 4)
  p_bft <- format(data$p_bft, nsmall = 4)
  avgs <- format(data$avgs, nsmall = 2)
  sds <- format(data$sds, nsmall = 2)
  avg <- format(data$avg, nsmall = 2)
  sd <- format(data$sd, nsmall = 2)

  # width
  w1 <- max(nchar(data$levs), nchar('combined'))
  w2 <- max(nchar('Obs'), nchar(data$lens), nchar(data$len))
  w3 <- max(nchar('Mean'), nchar(avgs), nchar(avg))
  w4 <- max(nchar('Std. Dev.'), nchar(sds), nchar(sd))


  w <- sum(w1, w2, w3, w4, 16)


  cat(fw('Summary Statistics', w = w), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(fw('Group', w = w1), fs(), fw('Obs', w = w2), fs(), fw('Mean', w = w3),
  	fs(), fw('Std. Dev.', w = w4), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(fw(data$levs[1], w = w1), fs(), fw(data$lens[1], w = w2), fs(), fw(avgs[1], w = w3),
  	fs(), fw(sds[1], w = w4), "\n")
  cat(fw(data$levs[2], w = w1), fs(), fw(data$lens[2], w = w2), fs(), fw(avgs[2], w = w3),
  	fs(), fw(sds[2], w = w4), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(fw('combined', w = w1), fs(), fw(data$len, w = w2), fs(), fw(avg, w = w3),
  	fs(), fw(sd, w = w4), "\n")
  cat(rep("-", w), sep = "", "\n\n")


  	wbf <- nchar(bf)
  	wpbf <- nchar(p_bf)
  	wlev <- nchar(lev)
  	wplev <- nchar(p_lev)
  	wbft <- nchar(bft)
  	wpbft <- nchar(p_bft)
    wn1 <- nchar(data$n_df)
    wn2 <- nchar(data$d_df)

    w1 <- max(wbf, wn1, wn2, wpbf) + 17
    w2 <- max(wlev, wn1, wn2, wplev) + 17
    w3 <- max(wbft, wn1, wn2, wpbft) + 17
    w4 <- max(wbf, wpbf, wlev, wplev, wbft, wpbft, wn1, wn2) + 26

    k1 <- rep("-", w1)
    k2 <- rep("-", w2)
    k3 <- rep("-", w3)
    k4 <- rep("-", w4)

    if (data$type == "mean") {

    	cat(format('Test Summary', width = w1, justify = 'centre'),'\n')
    	cat('', k1, sep = "", '\n')
    	cat(' W         =    ', bf, '\n',
    		'Num DF    =    ', data$n_df, '\n',
    		'Den DF    =    ', data$d_df, '\n',
    		'Pr > F    =    ', p_bf, '\n')
    	cat('', k1, sep = "", '\n')

    } else if (data$type == "median") {

    	cat(format('Test Summary', width = w2, justify = 'centre'),'\n')
    	cat('', k2, sep = "", '\n')
    	cat(' W         =    ', lev, '\n',
    		'Num DF    =    ', data$n_df, '\n',
    		'Den DF    =    ', data$d_df, '\n',
    		'Pr > F    =    ', p_lev, '\n')
    	cat('', k2, sep = "", '\n')

    } else if (data$type == "trimmed-mean") {

    	cat(format('Test Summary', width = w3, justify = 'centre'),'\n')
    	cat('', k3, sep = "", '\n')
    	cat(' W         =    ', bft, '\n',
    		'Num DF    =    ', data$n_df, '\n',
    		'Den DF    =    ', data$d_df, '\n',
    		'Pr > F    =    ', p_bft, '\n')
    	cat('', k3, sep = "", '\n')

    } else {

		cat(format('Test Summary', width = w4, justify = 'centre'),'\n')
    	cat('', k4, sep = "", '\n')
    	cat(' Num DF             =    ', data$n_df, '\n',
    		'Den DF             =    ', data$d_df, '\n',
    		'W: Mean            =    ', bf, '\n',
    		'Pr > F             =    ', p_bft, '\n',
    		'W: Median          =    ', lev, '\n',
    		'Pr > F             =    ', p_lev, '\n',
    		'W: Trimmed Mean    =    ', bft, '\n',
    		'Pr > F             =    ', p_bft, '\n')
    	cat('', k4, sep = "", '\n')

    }

}


print_bartlett_test <- function(data) {

	# width
	w1 <- max(nchar(data$df),
		nchar(format(data$fstat, nsmall = 3)),
		nchar(format(data$pval, nsmall = 3)))

	w <- w1 + 19

	# variable names
	if (is.na(data$g_var)) {
		ln <- length(data$var_c)
		w2 <- sum(nchar(unlist(lapply(data$var_c, l))))
		w3 <- w2 + ln + 10
	} else {
		w2 <- max(nchar(l(data$var_c)), nchar(l(data$g_var)))
		w3 <- w2 + 19
	}



	cat("\n", format(" Bartlett's Test of Homogenity of Variances",
		width = 48, justify = "centre"), "\n")
	cat(rep("-", 48), sep = "", '\n',
		"Ho: Variances are equal across groups\n",
		"Ha: Variances are unequal for atleast two groups\n\n")

	if (is.na(data$g_var)) {
		cat(format('Data', width = w3, justify = 'centre'), '\n')
		cat(" ", rep("-", w3), sep = "", '\n')
		cat(' Variables:', unlist(lapply(data$var_c, l)), '\n\n')
	} 

	# else {
	# 	if (data$var_c != 'var') {
	# 		cat(format('Data', width = w3, justify = 'centre'), '\n')
	# 		cat(" ", rep("-", w3), sep = "", '\n')
	# 		cat(' Variable         :', l(data$var_c), '\n',
	# 			'Grouping Variable:', l(data$g_var), '\n\n')
	# 	}
	# }

	cat(format('Test Summary', width = w, justify = 'centre'), '\n')
	cat(" ", rep("-", w), sep = "", '\n')
	cat(' DF            =   ', data$df, '\n',
			'Chi2          =   ', format(data$fstat, nsmall = 3), '\n',
			'Prob > Chi2   =   ', format(data$pval, nsmall = 3), '\n')

}



print_var_test <- function(data) {

  # format output
  f <- format(data$f, nsmall = 4)
  lower <- format(data$lower, nsmall = 4)
  upper <- format(data$upper, nsmall = 4)
  twotail <- format(data$two_tail, nsmall = 4)
  avgs <- format(data$avgs, nsmall = 2)
  sds <- format(data$sds, nsmall = 2)
  ses <- format(data$ses, nsmall = 2)
  avg <- format(data$avg, nsmall = 2)
  sd <- format(data$sd, nsmall = 2)
  se <- format(data$se, nsmall = 2)

  # width
  w1 <- max(nchar(data$lev), nchar('combined'))
  w2 <- max(nchar('Obs'), nchar(data$lens), nchar(data$len))
  w3 <- max(nchar('Mean'), nchar(avgs), nchar(avg))
  w4 <- max(nchar('Std. Dev.'), nchar(sds), nchar(sd))
  w5 <- max(nchar('Std. Err.'), nchar(ses), nchar(se))


  w <- sum(w1, w2, w3, w4, w5, 16)


  cat(fw('Group Statistics', w = w), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(fw('Group', w = w1), fs(), fw('Obs', w = w2), fs(), format('Mean', w = w3, justify = 'centre'),
  	fs(), fw('Std. Err.', w = w5), fs(), fw('Std. Dev.', w = w4), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(fw(data$lev[1], w = w1), fs(), fw(data$lens[1], w = w2), fs(), fw(avgs[1], w = w3),
  	fs(), fw(ses[1], w = w5), fs(), fw(sds[1], w = w4), "\n")
  cat(fw(data$lev[2], w = w1), fs(), fw(data$lens[2], w = w2), fs(), fw(avgs[2], w = w3),
  	fs(), fw(ses[2], w = w5), fs(), fw(sds[2], w = w4), "\n")
  cat(rep("-", w), sep = "", "\n")
  cat(fw('combined', w = w1), fs(), fw(data$len, w = w2), fs(), fw(avg, w = w3),
  	fs(), fw(se, w = w5), fs(), fw(sd, w = w4), "\n")
  cat(rep("-", w), sep = "", "\n")


  	wt <- nchar(f)
    wn1 <- nchar(data$n1)
    wn2 <- nchar(data$n2)
    wl <- nchar(lower)
    wu <- nchar(upper)
    wto <- nchar(twotail)

    w1 <- max(wt, wn1, wn2, wl) + 23
    w2 <- max(wt, wn1, wn2, wu) + 23
    w3 <- max(wt, wn1, wn2, wto) + 27
    w4 <- max(wt, wn1, wn2, wl, wu, wto) + 27

    k1 <- rep("-", w1)
    k2 <- rep("-", w2)
    k3 <- rep("-", w3)
    k4 <- rep("-", w4)

    if (data$type == "less") {

    	cat('\n\n', 'Null & Alternative Hypotheses', '\n')
    	cat(" ------------------------------",'\n')
    	cat(' Ho: ratio = 1', '\n', 'Ha: ratio < 1', '\n\n')
    	cat(format('Test Summary', width = w1, justify = 'centre'),'\n')
    	cat('', k1, sep = "", '\n')
    	cat(' f               =    ', f, '\n',
    		'Num DF          =    ', data$n1, '\n',
    		'Den DF          =    ', data$n2, '\n',
    		'Prob (F < f)    =    ', lower, '\n')
    	cat('', k1, sep = "", '\n')

    } else if (data$type == "greater") {

		cat('\n\n', 'Null & Alternative Hypotheses', '\n')
    	cat(" ------------------------------",'\n')
    	cat(' Ho: ratio = 1', '\n', 'Ha: ratio > 1', '\n\n')
    	cat(format('Test Summary', width = w2, justify = 'centre'),'\n')
    	cat('', k2, sep = "", '\n')
    	cat(' f               =    ', f, '\n',
    		'Num DF          =    ', data$n1, '\n',
    		'Den DF          =    ', data$n2, '\n',
    		'Prob (F > f)    =    ', upper, '\n')
    	cat('', k2, sep = "", '\n')

    } else if (data$type == "two.sided") {

		cat('\n\n', 'Null & Alternative Hypotheses', '\n')
    	cat(" ------------------------------	",'\n')
    	cat(' Ho: ratio = 1', '\n', 'Ha: ratio != 1', '\n\n')
    	cat(format('Test Summary', width = w3, justify = 'centre'),'\n')
    	cat('', k3, sep = "", '\n')
    	cat(' f                   =    ', f, '\n',
    		'Num DF              =    ', data$n1, '\n',
    		'Den DF              =    ', data$n2, '\n',
    		'2 * Prob (F > f)    =    ', twotail, '\n')
    	cat('', k3, sep = "", '\n')

    } else {

		cat('\n\n', format('Test Summary', width = w4, justify = 'centre'),'\n')
    	cat('', k4, sep = "", '\n')
    	cat(' f                   =    ', f, '\n',
    		'Num DF              =    ', data$n1, '\n',
    		'Den DF              =    ', data$n2, '\n',
    		'Prob (F < f)        =    ', lower, '\n',
    		'Prob (F > f)        =    ', upper, '\n',
    		'2 * Prob (F > f)    =    ', twotail, '\n')
    	cat('', k4, sep = "", '\n')

    }


}



print_osvar_test <- function(data) {

	var_width <- max(nchar('Variable'), nchar(data$varname))
	obs_width <- max(nchar('Obs'), nchar(data$n))
	mean_width <- max(nchar('Mean'), nchar(format(data$avg, nsmall = 3)))
	se_width <- max(nchar('Std. Err.'), nchar(format(data$se, nsmall = 3)))
	sd_width <- max(nchar('Std. Dev.'), nchar(format(data$sd, nsmall = 3)))
	conf_length <- nchar(format(data$confint[1], nsmall = 3)) + nchar(format(data$confint[2], nsmall = 3))
	confint_length <- nchar('[95% Conf. Interval]')
	if (conf_length > confint_length) {
	  conf_width <- round(conf_length / 2)
	} else {
	  conf_width <- round(confint_length / 2)
	}

	width_1 <- sum(var_width, obs_width, mean_width, se_width, sd_width, ceiling(conf_width * 2), 21)

	cat(format("One-Sample Statistics", width = width_1, justify = "centre"), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t("Variable", var_width), formats_t(), formatter_t("Obs", obs_width), formats_t(), formatter_t("Mean", mean_width), formats_t(),
      formatter_t("Std. Err.", se_width), formats_t(), formatter_t("Std. Dev.", sd_width), formats_t(), formatter_t("[95% Conf. Interval]", conf_width), "\n")
    cat(rep("-", width_1), sep = "")
    cat("\n", formatter_t(data$varname, var_width), formats_t(), formatter_n(data$n, obs_width), formats_t(),
    	formatter_n(data$avg, mean_width), formats_t(), formatter_n(data$se, se_width),
      formats_t(), formatter_n(data$sd, se_width), formats_t(), format_cil(data$confint[1], conf_width), format_ciu(data$confint[2], conf_width), "\n")
    cat(rep("-", width_1), sep = "")


    wt <- nchar(format(data$t, nsmall = 2))
    wdf <- nchar(data$df)
    wl <- nchar(format(data$lchi, nsmall = 3))
    wu <- nchar(format(data$uchi, nsmall = 3))
    wto <- nchar(format(data$tchi, nsmall = 3))

    w1 <- max(wt, wdf, wl) + 22
    w2 <- max(wt, wdf, wu) + 22
    w3 <- max(wt, wdf, wto) + 26
    w4 <- max(wt, wdf, wl, wu, wto) + 26

    k1 <- rep("-", w1)
    k2 <- rep("-", w2)
    k3 <- rep("-", w3)
    k4 <- rep("-", w4)

    if (data$type == "less") {

    	cat('\n\n\n', format('Null & Alternative Hypotheses', width = width_1, justify = 'left'), '\n')
    	cat(format(" ------------------------------", width = width_1, justify = 'left'),'\n')
    	cat(format(paste(' Ho: sd =', data$hyp_sd), width = 29, justify = 'centre'), '\n',
    		format(paste('Ha: sd <', data$hyp_sd), width = 29, justify = 'centre'), '\n\n\n')
    	cat(format('Test Summary', width = w1, justify = 'centre'),'\n')
    	cat(' ', k1, sep = "", '\n')
    	cat(' c (chi2)        =    ', data$t, '\n',
    		'DF              =    ', data$df, '\n',
    		'Prob (C < c)    =    ', data$lchi, '\n')
    	cat(' ', k1, sep = "", '\n')

    } else if (data$type == "greater") {

		cat('\n\n\n', format('Null & Alternative Hypotheses', width = width_1, justify = 'left'), '\n')
    	cat(format(" ------------------------------", width = width_1, justify = 'left'),'\n')
    	cat(format(paste(' Ho: sd =', data$hyp_sd), width = 29, justify = 'centre'), '\n',
    		format(paste('Ha: sd >', data$hyp_sd), width = 29, justify = 'centre'), '\n\n\n')
    	cat(format('Test Summary', width = w2, justify = 'centre'),'\n')
    	cat(' ', k2, sep = "", '\n')
    	cat(' c (chi2)        =    ', data$t, '\n',
    		'DF              =    ', data$df, '\n',
    		'Prob (C > c)    =    ', data$uchi, '\n')
    	cat(' ', k2, sep = "", '\n')

    } else if (data$type == "two.sided") {

		cat('\n\n\n', format('Null & Alternative Hypotheses', width = width_1, justify = 'left'), '\n')
    	cat(format(" ------------------------------", width = width_1, justify = 'left'),'\n')
    	cat(format(paste(' Ho: sd =', data$hyp_sd), width = 29, justify = 'centre'), '\n',
    		format(paste('Ha: sd !=', data$hyp_sd), width = 29, justify = 'centre'), '\n\n\n')
    	cat(format('Test Summary', width = w3, justify = 'centre'),'\n')
    	cat(' ', k3, sep = "", '\n')
    	cat(' c (chi2)            =    ', data$t, '\n',
    		'DF                  =    ', data$df, '\n',
    		'2 * Prob (C > c)    =    ', data$tchi, '\n')
    	cat(' ', k3, sep = "", '\n')

    } else {

		cat('\n\n', format('Test Summary', width = w4, justify = 'centre'),'\n')
    	cat(' ', k4, sep = "", '\n')
    	cat(' c (chi2)            =    ', data$t, '\n',
    		'DF                  =    ', data$df, '\n',
    		'Prob (C < c)        =    ', format(data$lchi, nsmall = 3), '\n',
    		'Prob (C > c)        =    ', format(data$uchi, nsmall = 3), '\n',
    		'2 * Prob (C > c)    =    ', format(data$tchi, nsmall = 3), '\n')
    	cat(' ', k4, sep = "", '\n')


    }

}



print_score_test <- function(data) {

	# width
	w1 <- max(nchar(data$df),
		nchar(format(data$score, nsmall = 3)),
		nchar(format(data$p, nsmall = 3)))

	w <- w1 + 19

	cat('\n Score Test for Heteroskedasticity\n')
			cat(" ", rep("-", 33), sep = "", '\n')
			cat(' Ho: Variance is homogenous\n', "Ha: Variance is not homogenous\n\n")


	if (data$rhs == TRUE) {

		cat(' Variables:', data$preds, '\n\n')

	} else if (data$fv == TRUE) {

		cat(' Variables: fitted values of', data$resp, '\n\n')

	} else {

		cat(' Variables:', data$preds, '\n\n')

	}

	cat(format('Test Summary', width = w, justify = 'centre'), '\n')
	cat(" ", rep("-", w), sep = "", '\n')
	cat(' DF            =   ', data$df, '\n',
			'Chi2          =   ', format(data$score, nsmall = 3), '\n',
			'Prob > Chi2   =   ', format(data$p, nsmall = 3), '\n')

}



print_step_backward <- function(data) {


    n <- data$steps

    if (n < 1) {
    	stop('No variables have been removed from the model based on p-values.')
    }

    # width
    w1 <- nchar('Step')
    w2 <- max(nchar('Variable'), nchar(data$removed))
    w3 <- max(nchar('R-Square'), nchar(round(data$rsquare, 3)))
    w4 <- max(nchar('R-Square'), nchar(round(data$adjr, 3)))
    w5 <- max(nchar('C(p)'), nchar(round(data$mallows_cp, 4)))
    w6 <- max(nchar('AIC'), nchar(round(data$aic, 4)))
    w7 <- max(nchar('RMSE'), nchar(round(data$rmse, 4)))
    w <- sum(w1, w2, w3, w4, w5, w6, w7, 24)

    cat(format("Backward Elimination Method", justify = "left", width = w), "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = w), "\n\n")
    for (i in seq_len(length(data$indvar))) {
        cat(format(paste(i, ".", data$indvar[i]), justify = "left", width = w), "\n")
    }
    cat("\n", rep("-", w), sep = "", '\n')
    cat(format("Elimination Summary", justify = "centre", width = w), "\n")
    cat(rep("-", w), sep = "", '\n')
    cat(format('', width = w1), fs(), format('Variable', width = w2), fs(),
        format('', width = w3), fs(), format('Adj.', width = w4, justify = "centre"), fs(),
        format('', width = w5), fs(), format('', width = w6), fs(),
        format('', width = w7), fs(), "\n")
    cat(format('Step', width = w1, justify = "centre"), fs(), format('Removed', width = w2, justify = "centre"), fs(),
        format('R-Square', width = w3, justify = "centre"), fs(), format('R-Square', width = w4, justify = "centre"), fs(),
        format('C(p)', width = w5, justify = "centre"), fs(), format('AIC', width = w6, justify = "centre"), fs(),
        format('RMSE', width = w7, justify = "centre"), fs(), "\n")
    cat(rep("-", w), sep = "", '\n')

    for (i in seq_len(n)) {
        cat(format(i, width = w1), fs(), format(data$removed[i], width = w2), fs(),
        format(data$rsquare[i], width = w3, nsmall = 3), fs(), format(data$adjr[i], width = w4, nsmall = 3), fs(),
        format(data$mallows_cp[i], width = w5, justify = "centre", nsmall = 4), fs(),
        format(round(data$aic[i], 4), width = w6, nsmall = 4), fs(), format(round(data$rmse[i], 4), width = w7, nsmall = 4), fs(), "\n")
    }
    cat(rep("-", w), sep = "", '\n')

}



print_best_subset <- function(data) {

    w1 <- 11
    w2 <- max(nchar(data$predictors))
    w <- sum(w1, w2, 4)

    w3 <- nchar('Model')
    w4 <- nchar('R-Square')
    w5 <- max(nchar('Pred'), nchar(round(data$predrsq, 4)))
    w6 <- max(nchar('C(p)'), nchar(round(data$cp, 4)))
    w7 <- max(nchar('AIC'), nchar(round(data$aic, 4)))
    w8 <- max(nchar('SBIC'), nchar(round(data$sbic, 4)))
    w9 <- max(nchar('SBC'), nchar(round(data$sbc, 4)))
    w10 <- max(nchar('MSEP'), nchar(round(data$gmsep, 4)))
    w11 <- max(nchar('FPE'), nchar(round(data$jp, 4)))
    w12 <- max(nchar('HSP'), nchar(round(data$sp, 4)))
    w13 <- max(nchar('APC'), nchar(round(data$pc, 4)))

    v <- sum(w3, w4, w4, w4, w6, w7, w8, w9, w10, w11, w12, w13, 44)

    cat(format("Best Subsets Regression", width = w, justify = "centre"))
    cat("\n", rep("-", w), sep = "", '\n')
    cat("Model Index    Predictors")
    cat("\n", rep("-", w), sep = "", '\n')
    for(i in data$mindex) {
        cat(format(as.character(data$mindex[i]), width = w1, justify = "centre"), fs(),
            format(data$predictors[i], width = w2), "\n")
    }
    cat(rep("-", w), sep = "", '\n\n')

    cat(format("Subsets Regression Summary", width = v, justify = "centre"))
    cat("\n", rep("-", v), sep = "", '\n')
    cat(format('', width = w3, justify = 'centre'), fs(), format('', width = w4, justify = 'centre'), fs(),
        format('Adj.', width = w4, justify = 'centre'), fs(), format('Pred', width = w4, justify = 'centre'), fs(),
        format('', width = w6, justify = 'centre'), fs(), format('', width = w7, justify = 'centre'), fs(),
        format('', width = w8, justify = 'centre'), fs(), format('', width = w9, justify = 'centre'), fs(),
        format('', width = w10, justify = 'centre'), fs(), format('', width = w11, justify = 'centre'), fs(),
        format('', width = w12, justify = 'centre'), fs(), format('', width = w13, justify = 'centre'), "\n")
    cat(format('Model', width = w3, justify = 'centre'), fs(), format('R-Square', width = w4, justify = 'centre'), fs(),
        format('R-Square', width = w4, justify = 'centre'), fs(), format('R-Square', width = w4, justify = 'centre'), fs(),
        format('C(p)', width = w6, justify = 'centre'), fs(), format('AIC', width = w7, justify = 'centre'), fs(),
        format('SBIC', width = w8, justify = 'centre'), fs(), format('SBC', width = w9, justify = 'centre'), fs(),
        format('MSEP', width = w10, justify = 'centre'), fs(), format('FPE', width = w11, justify = 'centre'), fs(),
        format('HSP', width = w12, justify = 'centre'), fs(), format('APC', width = w13, justify = 'centre'))
    cat("\n", rep("-", v), sep = "", '\n')
    for (i in data$mindex) {
        cat(format(as.character(data$mindex[i]), width = w3, justify = 'centre'), fs(), format(data$rsquare[i], nsmall = 4, width = w4, justify = 'centre'), fs(),
        format(data$adjr[i], nsmall = 4, width = w4, justify = 'centre'), fs(), format(data$predrsq[i], width = w4, justify = 'centre'), fs(),
        format(data$cp[i], nsmall = 4, width = w6, justify = 'centre'), fs(), format(data$aic[i], nsmall = 4, width = w7, justify = 'centre'), fs(),
        format(round(data$sbic[i], 4), nsmall = 4, width = w8, justify = 'centre'), fs(), format(data$sbc[i], nsmall = 4, width = w9, justify = 'centre'), fs(),
        format(round(data$gmsep[i], 4), nsmall = 4, width = w10, justify = 'centre'), fs(), format(round(data$jp[i], 4), nsmall = 4, width = w11, justify = 'centre'), fs(),
        format(round(data$sp[i], 4), nsmall = 4, width = w12, justify = 'centre'), fs(), format(round(data$pc[i], 4), nsmall = 4, width = w13, justify = 'centre'), "\n")
    }
    cat(rep("-", v), sep = "", '\n')
    cat("AIC: Akaike Information Criteria", "\n", "SBIC: Sawa's Bayesian Information Criteria", "\n", 
    	"SBC: Schwarz Bayesian Criteria", "\n",  "MSEP: Estimated error of prediction, assuming multivariate normality", 
    	"\n", "FPE: Final Prediction Error", "\n", "HSP: Hocking's Sp", "\n", "APC: Amemiya Prediction Criteria", 
    	"\n\n")

}



print_step_forward <- function(data) {


    n <- length(data$predictors)

    if (n < 1) {
    	stop('No variables have been added to the model based on p-values.')
    }

    # width
    w1 <- nchar('Step')
    w2 <- max(nchar('Variable'), nchar(data$predictors))
    w3 <- max(nchar('R-Square'), nchar(round(data$rsquare, 3)))
    w4 <- max(nchar('R-Square'), nchar(round(data$adjr, 3)))
    w5 <- max(nchar('C(p)'), nchar(round(data$mallows_cp, 4)))
    w6 <- max(nchar('AIC'), nchar(round(data$aic, 4)))
    w7 <- max(nchar('RMSE'), nchar(round(data$rmse, 4)))
    w <- sum(w1, w2, w3, w4, w5, w6, w7, 24)

    cat(format("Forward Selection Method", justify = "left", width = w), "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = w), "\n\n")
    for (i in seq_len(length(data$indvar))) {
        cat(format(paste(i, ".", data$indvar[i]), justify = "left", width = w), "\n")
    }
    cat("\n", rep("-", w), sep = "", '\n')
    cat(format("Selection Summary", justify = "centre", width = w), "\n")
    cat(rep("-", w), sep = "", '\n')
    cat(format('', width = w1), fs(), format('Variable', width = w2), fs(),
        format('', width = w3), fs(), format('Adj.', width = w4, justify = "centre"), fs(),
        format('', width = w5), fs(), format('', width = w6), fs(),
        format('', width = w7), fs(), "\n")
    cat(format('Step', width = w1, justify = "centre"), fs(), format('Entered', width = w2, justify = "centre"), fs(),
        format('R-Square', width = w3, justify = "centre"), fs(), format('R-Square', width = w4, justify = "centre"), fs(),
        format('C(p)', width = w5, justify = "centre"), fs(), format('AIC', width = w6, justify = "centre"), fs(),
        format('RMSE', width = w7, justify = "centre"), fs(), "\n")
    cat(rep("-", w), sep = "", '\n')

    for (i in seq_len(n)) {
        cat(format(i, width = w1), fs(), format(data$predictors[i], width = w2), fs(),
        format(data$rsquare[i], width = w3, nsmall = 3), fs(), format(data$adjr[i], width = w4, nsmall = 3), fs(),
        format(data$mallows_cp[i], width = w5, justify = "centre", nsmall = 4), fs(),
        format(round(data$aic[i], 4), width = w6, nsmall = 4), fs(), format(round(data$rmse[i], 4), width = w7, nsmall = 4), fs(), "\n")
    }
    cat(rep("-", w), sep = "", '\n')

}



print_stepwise <- function(data) {

    n <- data$steps

    if (n < 1) {
    	stop('No variables have been added to or removed from the model based on p-values.')
    }

    # width
    w1 <- nchar('Step')
    w2 <- max(nchar('Variable'), nchar(data$orders))
    w8 <- max(nchar('Removed'), nchar(data$method))
    w3 <- max(nchar('R-Square'), nchar(round(data$rsquare, 3)))
    w4 <- max(nchar('R-Square'), nchar(round(data$adjr, 3)))
    w5 <- max(nchar('C(p)'), nchar(round(data$mallows_cp, 4)))
    w6 <- max(nchar('AIC'), nchar(round(data$aic, 4)))
    w7 <- max(nchar('RMSE'), nchar(round(data$rmse, 4)))
    w <- sum(w1, w2, w3, w4, w5, w6, w7, w8, 28)

    cat(format("Stepwise Selection Method", justify = "left", width = w), "\n\n")
    cat(format("Candidate Terms:", justify = "left", width = w), "\n\n")
    for (i in seq_len(length(data$indvar))) {
        cat(format(paste(i, ".", data$indvar[i]), justify = "left", width = w), "\n")
    }
    cat("\n", rep("-", w), sep = "", '\n')
    cat(format("Stepwise Selection Summary", justify = "centre", width = w), "\n")
    cat(rep("-", w), sep = "", '\n')
    cat(format('', width = w1), fs(), format('', width = w2), fs(), format('Added/', width = w8, justify = 'centre'), fs(),
        format('', width = w3), fs(), format('Adj.', width = w4, justify = "centre"), fs(),
        format('', width = w5), fs(), format('', width = w6), fs(),
        format('', width = w7), fs(), "\n")
    cat(format('Step', width = w1, justify = "centre"), fs(), format('Variable', width = w2, justify = "centre"), fs(),
        format('Removed', width = w8, justify = "centre"), fs(),
        format('R-Square', width = w3, justify = "centre"), fs(), format('R-Square', width = w4, justify = "centre"), fs(),
        format('C(p)', width = w5, justify = "centre"), fs(), format('AIC', width = w6, justify = "centre"), fs(),
        format('RMSE', width = w7, justify = "centre"), fs(), "\n")
    cat(rep("-", w), sep = "", '\n')

    for (i in seq_len(n)) {
        cat(format(i, width = w1, justify = 'centre'), fs(), format(data$orders[i], width = w2, justify = 'centre'), fs(),
            format(data$method[i], width = w8), fs(), format(data$rsquare[i], width = w3, nsmall = 3), fs(),
            format(data$adjr[i], width = w4, nsmall = 3), fs(), format(data$mallows_cp[i], width = w5, justify = "centre", nsmall = 4), fs(),
            format(round(data$aic[i], 4), width = w6, nsmall = 4), fs(), format(round(data$rmse[i], 4), width = w7, nsmall = 4), fs(), "\n")
    }
    cat(rep("-", w), sep = "", '\n')

}



print_stepaic_forward <- function(data) {

	if (data$steps < 1) {
		stop('No variables have been added to the model.')
	}

        # width
        w1 <- max(nchar('Predictor'), nchar(data$predictors))
        w2 <- max(nchar('AIC'), nchar(data$aics))
        w3 <- max(nchar('Sum Sq'), nchar(data$rss))
        w4 <- max(nchar('RSS'), nchar(data$ess))
        w5 <- max(nchar('R-Sq'), nchar(data$rsq))
        w6 <- max(nchar('Adj. R-Sq'), nchar(data$arsq))
        w <- sum(w1, w2, w3, w4, w5, w6, 20)

        ln <- length(data$aics)

        cat(rep("-", w), sep = "", '\n')
        cat(fl('Variable', w1), fs(), fc('AIC', w2), fs(),
            fc('Sum Sq', w3), fs(), fc('RSS', w4), fs(), fc('R-Sq', w5), fs(),
            fc('Adj. R-Sq', w6), '\n')
        cat(rep("-", w), sep = "", '\n')
        for (i in seq_len(ln)) {
            cat(fl(data$predictors[i], w1), fs(), fg(data$aics[i], w2), fs(),
            fg(data$rss[i], w3), fs(), fg(data$ess[i], w4), fs(),
            fg(data$rsq[i], w5), fs(), fg(data$arsq[i], w6), '\n')
        }
        cat(rep("-", w), sep = "", '\n')

}



print_stepaic_backward <- function(data) {

	if (data$steps < 1) {
		stop('No variables have been removed from the model.')
	}

    # width
    w1 <- max(nchar('Full Model'), nchar(data$predictors))
    w2 <- max(nchar('AIC'), nchar(format(data$aics, nsmall = 3)))
    w3 <- max(nchar('RSS'), nchar(format(data$ess, nsmall = 3)))
    w4 <- max(nchar('Sum Sq'), nchar(format(data$rss, nsmall = 3)))
    w5 <- max(nchar('R-Sq'), nchar(format(data$rsq, nsmall = 3)))
    w6 <- max(nchar('Adj. R-Sq'), nchar(format(data$arsq, nsmall = 3)))
    w <- sum(w1, w2, w3, w4, w5, w6, 20)

    predictors <- c('Full Model', data$predictors)

    ln <- length(data$aics)

    cat('\n\n', format('Backward Elimination Summary', width = w, justify = 'centre'), '\n')
    cat(rep("-", w), sep = "", '\n')
    cat(fl('Variable', w1), fs(), fc('AIC', w2), fs(),
        fc('RSS', w3), fs(), fc('Sum Sq', w4), fs(), fc('R-Sq', w5), fs(),
        fc('Adj. R-Sq', w6), '\n')
    cat(rep("-", w), sep = "", '\n')
    for (i in seq_len(ln)) {
        cat(fl(predictors[i], w1), fs(), fg(data$aics[i], w2), fs(),
            fg(data$ess[i], w3), fs(), fg(data$rss[i], w4), fs(),
            fg(data$rsq[i], w5), fs(), fg(data$arsq[i], w6), '\n')
    }
    cat(rep("-", w), sep = "", '\n\n')

}



print_stepaic_both <- function(data) {

	if (data$steps < 1) {
		stop('No variables have been added to or removed from the model.')
	}

    # width
    w1 <- max(nchar('Variable'), nchar(data$predictors))
    w2 <- max(nchar('AIC'), nchar(format(data$aic, nsmall = 3)))
    w3 <- max(nchar('RSS'), nchar(format(data$ess, nsmall = 3)))
    w4 <- max(nchar('Sum Sq'), nchar(format(data$rss, nsmall = 3)))
    w5 <- max(nchar('R-Sq'), nchar(format(data$rsq, nsmall = 3)))
    w6 <- max(nchar('Adj. R-Sq'), nchar(format(data$arsq, nsmall = 3)))
    w7 <- nchar('Addition')
    w <- sum(w1, w2, w3, w4, w5, w6, w7, 24)

    ln <- length(data$aic)

    cat('\n\n', format('Stepwise Summary', width = w, justify = 'centre'), '\n')
    cat(rep("-", w), sep = "", '\n')
    cat(fl('Variable', w1), fs(), fc('Method', w7), fs(), fc('AIC', w2), fs(),
        fc('RSS', w3), fs(), fc('Sum Sq', w4), fs(), fc('R-Sq', w5), fs(),
        fc('Adj. R-Sq', w6), '\n')
    cat(rep("-", w), sep = "", '\n')
    for (i in seq_len(ln)) {
        cat(fl(data$predictors[i], w1), fs(), fl(data$method[i], w7), fs(),
            fg(data$aic[i], w2), fs(),
            fg(data$ess[i], w3), fs(), fg(data$rss[i], w4), fs(),
            fg(data$rsq[i], w5), fs(), fg(data$arsq[i], w6), '\n')
    }
    cat(rep("-", w), sep = "", '\n\n')

}



print_norm_test <- function(data) {

	# width
	w1 <- 18
	w2 <- 14
	w3 <- 7
	w <- sum(w1, w2, w3, 8)

	# vectors
	tests <- c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Cramer-von Mises",
		"Anderson-Darling")
	stats <- c(data$shapiro$statistic, data$kolmogorv$statistic,
		data$cramer$statistic, data$anderson$statistic)
	pvals <- c(data$shapiro$p.value, data$kolmogorv$p.value,
		data$cramer$p.value, data$anderson$p.value)
	n <- length(stats)

	# print
	cat(rep("-", w), sep = "", '\n')
	cat(format("Test", width = w1, justify = "centre"), fs(), format("Statistic", width = w2, justify = 'centre'),
		fs(), format("pvalue", width = 7, justify = "centre"), "\n")
	cat(rep("-", w), sep = "", '\n')
	for (i in seq_len(n)) {
		cat(format(tests[i], width = w1), fs(), format(as.character(round(stats[i], 4)), width = w2, justify = "centre"),
		fs(), format(round(pvals[i], 4), nsmall = 4, width = 7, justify = "centre"), "\n")
	}
	cat(rep("-", w), sep = "", '\n')

}



print_pure_error_anova <- function(data) {

	wt <- max(nchar(data$resp), nchar(data$preds)) + 13

	cat(format('Lack of Fit F Test', width = wt, justify = 'centre'), '\n')
	cat(rep("-", wt), sep = "", '\n')
	cat('Response :  ', data$resp, '\n')
	cat('Predictor:  ', data$preds, '\n\n')

	# widths
	w1 <- max(nchar(data$resp), 12)
	w2 <- max(nchar('DF'), nchar(data$df_rss), nchar(data$df_ess), nchar(data$df_lof), nchar(data$df_error))
	w3 <- max(nchar('Sum Sq'), nchar(format(data$rss, nsmall = 2)), nchar(format(data$ess, nsmall = 2)),
		nchar(format(data$lackoffit, nsmall = 2)), nchar(format(data$pure_error, nsmall = 2)))
	w4 <- max(nchar('Mean Sq'), nchar(format(data$rms, nsmall = 2)), nchar(format(data$ems, nsmall = 2)),
		nchar(format(data$lms, nsmall = 2)), nchar(format(data$pms, nsmall = 2)))
	w5 <- max(nchar('F Value'), nchar(format(data$rf, nsmall = 2)), nchar(format(data$lf, nsmall = 2)))
	w6 <- max(nchar('Pr(>F)'), nchar(format(data$pr, nsmall = 2)), nchar(format(data$pl, nsmall = 2)))
	w <- sum(w1, w2, w3, w4, w5, w6, 20)

	cat(format('Analysis of Variance Table', width = w, justify = 'centre'), '\n')
	cat(rep("-", w), sep = "", '\n')
	cat(fc('', w = w1), fs(), fc('DF', w = w2), fs(), fc('Sum Sq', w = w3), fs(), fc('Mean Sq', w = w4),
		 fs(), fc('F Value', w = w5), fs(), fc('Pr(>F)', w = w6), '\n')
	cat(rep("-", w), sep = "", '\n')
	cat(fl(data$preds, w = w1), fs(), fg(data$df_rss, w = w2), fs(), format(data$rss, nsmall = 2, width = w3), fs(),
		format(data$rms, nsmall = 2, width = w4), fs(), format(data$rf, nsmall = 2, width = w5), fs(),
		format(data$pr, nsmall = 2, width = w6), '\n')
	cat(fl('Residual', w = w1), fs(), fg(data$df_ess, w = w2), fs(), format(data$ess, nsmall = 2, width = w3), fs(),
		format(data$ems, nsmall = 2, width = w4), fs(), format('', nsmall = 2, width = w5), fs(),
		format('', nsmall = 2, width = w6), '\n')
	cat(fl(' Lack of fit', w = w1), fs(), fg(data$df_lof, w = w2), fs(), format(data$lackoffit, nsmall = 2, width = w3), fs(),
		format(data$lms, nsmall = 2, width = w4), fs(), format(data$lf, nsmall = 2, width = w5), fs(),
		format(data$pl, nsmall = 2, width = w6), '\n')
	cat(fl(' Pure Error', w = w1), fs(), fg(data$df_error, w = w2), fs(), format(data$pure_error, nsmall = 2, width = w3), fs(),
		format(data$pms, nsmall = 2, width = w4), fs(), format('', nsmall = 2, width = w5), fs(),
		format('', nsmall = 2, width = w6), '\n')
	cat(rep("-", w), sep = "", '\n')

}
