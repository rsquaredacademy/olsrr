#' @rdname summary.linear_fe
#'
#' @examples
#' data(ExampleDataLinear)
#' outcome <- ExampleDataLinear$Y
#' covar <- ExampleDataLinear$Z
#' ProvID <- ExampleDataLinear$ProvID
#' fit_re <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
#' summary(fit_re)
#'
#' @importFrom stats pnorm qnorm
#'
#' @exportS3Method summary linear_re

summary.linear_re <- function(object, parm, level = 0.95, null = 0, ...) {
  alpha <- 1 - level

  if (missing(object)) stop ("Argument 'object' is required!",call.=F)
  if (!class(object) %in% c("linear_re")) stop("Object `object` is not of the classes 'linear_re'!",call.=F)

  model <- attributes(object)$model

  covar_char <- c("(intercept)", object$char_list$Z.char)

  FE_est <- object$coefficient$FE
  se.FE <- sqrt(diag(object$variance$FE))
  stat <- (FE_est - null) / se.FE

  n <- nrow(object$data_includ)
  df <- n - length(object$coefficient$FE) - length(object$coefficient$RE) + 1

  p_value <- 2 * (1 - pt(abs(stat), df = df))
  p_value <- format.pval(p_value, digits = 7, eps = 1e-10)

  CI <- confint(model, parm = "beta_", method = "Wald", level = level)

  # crit_value <- qnorm(1-alpha/2)
  # lower_bound <- FE_est - crit_value * se.FE
  # upper_bound <- FE_est + crit_value * se.FE

  result <- data.frame(FE = FE_est, se.FE = se.FE, stat = stat, p_value = p_value,
                       lower_bound = CI[,1], upper_bound = CI[,2])
  colnames(result) <- c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper")

  if (missing(parm)) {
    ind <- 1:length(covar_char)
  } else if (is.character(parm)) {
    ind <- which(covar_char %in% parm)
  } else if (is.numeric(parm) & max(abs(as.integer(parm) - parm)) == 0 & !(0 %in% parm)) {
    ind <- parm
  } else {
    stop("Argument 'parm' includes invalid elements!")
  }

  result <- result[ind, ]
  return(result)
}


# summary_linearRE_covar <- function(object, parm, level = 0.95, null = 0, ref.dis = "normal") {
#   alpha <- 1 - level
#
#   if (missing(object)) stop ("Argument 'object' is required!",call.=F)
#   if (!class(object) %in% c("linear_re")) stop("Object object is not of the classes 'linear_RE'!",call.=F)
#
#   covar_char <- c("(intercept)", object$char_list$Z.char)
#
#   if (ref.dis == "normal") {
#     covar <- rbind(object$coefficient$mu, object$coefficient$beta)
#     colnames(covar) <- "Estimate"
#     rownames(covar) <- covar_char
#
#     se.covar <- rbind(sqrt(object$variance$mu), sqrt(object$variance$beta))
#     colnames(covar) <- "Std.Error"
#     rownames(covar) <- covar_char
#
#     stat <- (covar - null) / se.covar
#
#     p_value <- 2 * (1 - pnorm(abs(stat)))
#
#     crit_value <- qnorm(1 - alpha / 2)
#
#     lower_bound <- covar - crit_value * se.covar
#     upper_bound <- covar + crit_value * se.covar
#
#     result <- data.frame(Estimate = covar, Std.Error = se.covar,
#                          stat = stat, p_value = p_value,
#                          CI.lower = lower_bound, CI.upper = upper_bound)
#     colnames(result) <- c("Estimate", "Std.Error", "Stat", "p", "CI.Lower", "CI.Upper")
#
#   }
#   else if (ref.dis == "t") {
#     object_converted <- as_lmerModLmerTest(object$model)
#     sum <- summary(object_converted)
#     beta_sum <- sum$coefficients
#
#     stat <- (beta_sum[,1] - null) / beta_sum[,2]
#     p_value <- 2 * (1 - pt(abs(stat), df = beta_sum[,3]))
#     CI <- confint(object_converted, level = level)
#
#     result <- data.frame(Estimate = beta_sum[,1], Std.Error = beta_sum[,2],
#                          df = beta_sum[,3], stat = stat, p_value = p_value,
#                          CI.lower = CI[3:nrow(CI),1], CI.upper = CI[3:nrow(CI),2])
#     colnames(result) <- c("Estimate", "Std.Error", "df", "Stat", "p", "CI.Lower", "CI.Upper")
#   }
#   else {
#     stop("Reference distribution must be either 'normal' or 't'")
#   }
#
#   if (missing(parm)) {
#     ind <- 1:length(covar_char)
#   } else if (is.character(parm)) {
#     ind <- which(covar_char %in% parm)
#   } else if (is.numeric(parm) & max(abs(as.integer(parm) - parm)) == 0 & !(0 %in% parm)) {
#     ind <- parm
#   } else {
#     stop("Argument 'parm' includes invalid elements!")
#   }
#
#   result <- result[ind, ]
#   return(result)
# }
