#' @importFrom stats coefficients
#' @importFrom dplyr group_by_ select_ summarise_each funs
#' @title Lack  of Fit F Test
#' @description Lack of Fit F Test
#' @param model an object of class \code{lm}
#' @param ... other parameters
#' @details Some statistical tests, for example the analysis of variance, assume
#' that variances are equal across groups or samples. The Bartlett test can be
#' used to verify that assumption. Bartlett's test is sensitive to departures
#' from normality. That is, if your samples come from non-normal distributions,
#' then Bartlett's test may simply be testing for non-normality. The Levene test
#' is an alternative to the Bartlett test that is less sensitive to departures
#' from normality.
#' @return \code{pure_error_anova} returns an object of class
#' \code{"pure_error_anova"}. An object of class \code{"pure_error_anova"} is a
#' list containing the following components:
#'
#' \item{lackoffit}{f statistic}
#' \item{pure_error}{pure error}
#' \item{rss}{regression sum of squares}
#' \item{ess}{error sum of squares}
#' \item{total}{total sum of squares}
#' \item{rms}{}
#' \item{ems}{p-value of \code{fstat}}
#' \item{lms}{degrees of freedom}
#' \item{pms}{name(s) of \code{variable}}
#' \item{rf}{name of \code{group_var}}
#' \item{lf}{f statistic}
#' \item{pr}{p-value of \code{fstat}}
#' \item{pl}{degrees of freedom}
#' \item{mpred}{name(s) of \code{variable}}
#' \item{df_rss}{name of \code{group_var}}
#' \item{df_ess}{f statistic}
#' \item{df_lof}{p-value of \code{fstat}}
#' \item{df_error}{degrees of freedom}
#' \item{final}{name(s) of \code{variable}}
#' \item{resp}{name of \code{group_var}}
#' \item{preds}{name of \code{group_var}}
#' @export
#'
pure_error_anova <- function(model, ...) UseMethod('pure_error_anova')

#' @export
#'
pure_error_anova.default <- function(model, ...) {

	if (!all(class(model) == 'lm')) {
    stop('Please specify a OLS linear regression model.', call. = FALSE)
  }

	ln <- length(coefficients(model))
	if (ln > 2) {
		stop("Lack of fit F test is available only for simple linear regression.", call. = FALSE)
	}

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

	class(result) <- 'pure_error_anova'

	return(result)

}

print.pure_error_anova <- function(x, ...) {
	print_pure_error_anova(x)
}
