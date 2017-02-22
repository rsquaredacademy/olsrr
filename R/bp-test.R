#' @importFrom stats anova
#' @title Breusch Pagan Test
#' @description Test for constant variance. It assumes that the error terms are normally distributed.
#' @param model an object of class \code{lm}
#' @param fitted.values logical; if TRUE, use fitted values of regression model
#' @param rhs logical; if TRUE, specifies that tests for heteroskedasticity be
#' performed for the right-hand-side (explanatory) variables of the fitted
#' regression model
#' @param multiple logical; if TRUE, specifies that multiple testing be performed
#' @param p.adj p value adjustment, following options are available: bonferroni,
#' holm, sidak and none
#' @param vars variables to be used for for heteroskedasticity test
#' @return \code{bp_test} returns an object of class \code{"bp_test"}.
#' An object of class \code{"bp_test"} is a list containing the
#' following components:
#'
#' \item{bp}{reusch pagan statistic}
#' \item{p}{p-value of \code{bp}}
#' \item{fv}{fitted values of the regression model}
#' \item{rhs}{name of explanatory variables of fitted regression model}
#' \item{multiple}{logical value indicating if multiple tests should be performed}
#' \item{padj}{adjusted p values}
#' \item{vars}{variables to be used for heteroskedasticity test}
#' \item{resp}{response variable}
#' \item{preds}{predictors}
#' @references T.S. Breusch & A.R. Pagan (1979), A Simple Test for Heteroscedasticity and 
#' Random Coefficient Variation. Econometrica 47, 1287–1294
#'
#' Cook, R. D.; Weisberg, S. (1983). "Diagnostics for Heteroskedasticity in Regression". Biometrika. 70 (1): 1–10.
#' @examples
#' # Use fitted values of the model
#' model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#' bp_test(model)
#' 
#' # Use independent variables of the model
#' model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#' bp_test(model, rhs = TRUE)
#' 
#' # Use independent variables of the model and perform multiple tests
#' model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#' bp_test(model, rhs = TRUE, multiple = TRUE)
#' 
#' # Bonferroni p value Adjustment
#' model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#' bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = 'bonferroni')
#' 
#' # Sidak p value Adjustment
#' model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#' bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = 'sidak')
#' 
#' # Holm's p value Adjustment
#' model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#' bp_test(model, rhs = TRUE, multiple = TRUE, p.adj = 'holm')
#'
#' @export
#'
bp_test <- function(model, fitted.values = TRUE, rhs = FALSE, multiple = FALSE,
	p.adj = c("none", "bonferroni", "sidak", "holm"), vars = NA) UseMethod('bp_test')

#' @export
#'
bp_test.default <- function(model, fitted.values = TRUE, rhs = FALSE, multiple = FALSE,
	p.adj = c("none", "bonferroni", "sidak", "holm"), vars = NA) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    if (!is.logical(fitted.values)) {
    	stop('fitted.values must be either TRUE or FALSE')
    }

    if (!is.logical(rhs)) {
    	stop('rhs must be either TRUE or FALSE')
    }

    if (!is.logical(multiple)) {
    	stop('multiple must be either TRUE or FALSE')
    }

    suppressWarnings(
    	if (!is.na(vars)) {

    		if (!all(vars %in% names(model$coefficients))) {
    			stop('vars must be a subset of the predictors in the model')
    		}

	    	fitted.values <- FALSE
	    }
    )

        method <- match.arg(p.adj)
    # l          <- model.frame(model)
            m1 <- tibble::as_data_frame(model.frame(model))
	          m2 <- tibble::as_data_frame(model.matrix(model)[, c(-1)])
	           l <- tibble::as_data_frame(cbind(m1[, c(1)], m2))
             n <- nrow(l)
      response <- names(l)[1]
    predictors <- names(l)[-1]

	if (fitted.values) {

		vars <- NA

		if (rhs) {

			if (multiple) {

					n         <- nrow(l)
					nam       <- names(l)[-1]
					np        <- length(nam)
					var_resid <- sum(residuals(model) ^ 2) / n
					ind       <- residuals(model) ^ 2 / var_resid - 1
					l         <- cbind(l, ind)
					tstat     <- c()
					pvals     <- c()

					for (i in seq_len(np)) {

					    form     <- as.formula(paste("ind ~", nam[i]))
					    model1   <- lm(form, data = l)
					    tstat[i] <- sum(model1$fitted ^ 2) / 2
					    pvals[i] <- pchisq(tstat[i], df = 1, lower.tail = F)

					}

					mdata  <- l[-1]
					models <- lm(ind ~ ., data = mdata)
					bp     <- sum(models$fitted ^ 2) / 2
					ps     <- pchisq(bp, df = np, lower.tail = F)
					bp     <- c(tstat, bp)


				if (method == "bonferroni") {

					bpvals <- pmin(1, pvals * np)
					p      <- c(bpvals, ps)


				} else if (method == "sidak") {

					spvals <- pmin(1, 1 - (1 - pvals) ^ np)
					p      <- c(spvals, ps)

				} else if (method == "holm") {

					j      <- rev(seq_len(length(pvals)))
					k      <- order(pvals)
					h      <- order(k)
					pholms <- pmin(1, sort(pvals) * j)[h]
					p      <- c(pholms, ps)

				} else {

					p      <- c(pvals, ps)

				}

			} else {

				n         <- nrow(model.frame(model))
				var_resid <- sum(residuals(model) ^ 2) / n
				ind       <- residuals(model) ^ 2 / var_resid - 1
				mdata     <- l[-1]
				d_f       <- ncol(mdata)
				model1    <- lm(ind ~ ., data = mdata)
				bp        <- sum(model1$fitted ^ 2) / 2
				p         <- pchisq(bp, df = d_f, lower.tail = F)

			}


		} else {

			pred         <- model$fitted.values
			resid        <- model$residuals ^ 2
			avg_resid    <- sum(resid) / n
			scaled_resid <- resid / avg_resid
			model1       <- lm(scaled_resid ~ pred)
			bp           <- anova(model1)$`Sum Sq`[1] / 2
			p            <- pchisq(bp, df = 1, lower.tail = F)

		}

	} else {

		if (multiple) {

			if (rhs) {

					n         <- nrow(l)
					nam       <- names(l)[-1]
					np        <- length(nam)
					var_resid <- sum(residuals(model) ^ 2) / n
					ind       <- residuals(model) ^ 2 / var_resid - 1
					l         <- cbind(l, ind)
					tstat     <- c()
					pvals     <- c()

					for (i in seq_len(np)) {

					    form     <- as.formula(paste("ind ~", nam[i]))
					    model1   <- lm(form, data = l)
					    tstat[i] <- sum(model1$fitted ^ 2) / 2
					    pvals[i] <- pchisq(tstat[i], df = 1, lower.tail = F)

					}

					mdata  <- l[-1]
					models <- lm(ind ~ ., data = mdata)
					bp     <- sum(models$fitted ^ 2) / 2
					ps     <- pchisq(bp, df = np, lower.tail = F)
					bp     <- c(tstat, bp)

				if (method == "bonferroni") {

					bpvals <- pmin(1, pvals * np)
					p      <- c(bpvals, ps)


				} else if (method == "sidak") {

					spvals <- pmin(1, 1 - (1 - pvals) ^ np)
					p      <- c(spvals, ps)

				} else if (method == "holm") {

					j      <- rev(seq_len(length(pvals)))
					k      <- order(pvals)
					h      <- order(k)
					pholms <- pmin(1, sort(pvals) * j)[h]
					p      <- c(pholms, ps)

				} else {

					p      <- c(pvals, ps)

				}


			} else {

				len_vars <- length(vars)

				if (len_vars > 1) {

					n         <- nrow(l)
					nam       <- names(l)[-1]
					np        <- length(nam)
					var_resid <- sum(residuals(model) ^ 2) / n
					ind       <- residuals(model) ^ 2 / var_resid - 1
					l         <- cbind(l, ind)
					len_var   <- length(vars)
					tstat     <- c()
					pvals     <- c()

					for (i in seq_len(len_var)) {

					    form     <- as.formula(paste("ind ~", vars[i]))
					    model1   <- lm(form, data = l)
					    tstat[i] <- sum(model1$fitted ^ 2) / 2
					    pvals[i] <- pchisq(tstat[i], df = 1, lower.tail = F)

					}

					# simultaneous
					mdata  <- l[-1]
					dl     <- mdata[, vars]
					dk     <- as.data.frame(cbind(ind, dl))
					np     <- ncol(dk) - 1
					models <- lm(ind ~ ., data = dk)
					bp     <- sum(models$fitted ^ 2) / 2
					ps     <- pchisq(bp, df = np, lower.tail = F)
					bp     <- c(tstat, bp)

					if (method == "bonferroni") {

						bpvals <- pmin(1, pvals * np)
						p      <- c(bpvals, ps)


					} else if (method == "sidak") {

						spvals <- pmin(1, 1 - (1 - pvals) ^ np)
						p      <- c(spvals, ps)

					} else if (method == "holm") {

						j      <- rev(seq_len(length(pvals)))
						k      <- order(pvals)
						h      <- order(k)
						pholms <- pmin(1, sort(pvals) * j)[h]
						p      <- c(pholms, ps)

					} else {

						p      <- c(pvals, ps)

					}

				} else {

					n         <- nrow(l)
					nam       <- names(l)[-1]
					np        <- length(nam)
					var_resid <- sum(residuals(model) ^ 2) / n
					ind       <- residuals(model) ^ 2 / var_resid - 1
					l         <- cbind(l, ind)
					mdata     <- l[-1]
					dl        <- mdata[, vars]
					dk        <- as.data.frame(cbind(ind, dl))
					nd        <- ncol(dk) - 1
					models    <- lm(ind ~ ., data = dk)
					bp        <- sum(models$fitted ^ 2) / 2
					p         <- pchisq(bp, df = nd, lower.tail = F)

				}

			}

		} else {

			if (rhs) {

				n         <- nrow(l)
				nam       <- names(l)[-1]
				np        <- length(nam)
				var_resid <- sum(residuals(model) ^ 2) / n
				ind       <- residuals(model) ^ 2 / var_resid - 1
				l         <- cbind(l, ind)
				mdata     <- l[-1]
				models    <- lm(ind ~ ., data = mdata)
				bp        <- sum(models$fitted ^ 2) / 2
				p         <- pchisq(bp, df = np, lower.tail = F)


			} else {

				n         <- nrow(l)
				nam       <- names(l)[-1]
				np        <- length(nam)
				var_resid <- sum(residuals(model) ^ 2) / n
				ind       <- residuals(model) ^ 2 / var_resid - 1
				l         <- cbind(l, ind)
				mdata     <- l[-1]
				dl        <- mdata[, vars]
				dk        <- as.data.frame(cbind(ind, dl))
				nd        <- ncol(dk) - 1
				models    <- lm(ind ~ ., data = dk)
				bp        <- sum(models$fitted ^ 2) / 2
				p         <- pchisq(bp, df = nd, lower.tail = F)


			}

		}


	}

	# output
	out <- list(bp       = round(bp, 4),
		          p        = round(p, 4),
		          fv       = fitted.values,
		          rhs      = rhs,
		          multiple = multiple,
							padj     = method,
							vars     = vars,
							resp     = response,
							preds    = predictors)

	class(out) <- 'bp_test'

	return(out)

}

#' @export
#'
print.bp_test <- function(x, ...) {
	print_bp_test(x)
}
