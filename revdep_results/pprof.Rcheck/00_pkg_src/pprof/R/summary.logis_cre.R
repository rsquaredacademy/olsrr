#' @rdname summary.logis_re
#'
#' @examples
#' data(ExampleDataBinary)
#' outcome <- ExampleDataBinary$Y
#' covar <- ExampleDataBinary$Z
#' ProvID <- ExampleDataBinary$ProvID
#' data <- data.frame(outcome, ProvID, covar)
#' outcome.char <- colnames(data)[1]
#' ProvID.char <- colnames(data)[2]
#' wb.char <- c("z1", "z2")
#' other.char <- c("z3", "z4", "z5")
#' fit_cre <- logis_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
#' wb.char = wb.char, other.char = other.char)
#' summary(fit_cre)
#'
#' @importFrom stats pnorm qnorm confint
#'
#' @exportS3Method summary logis_cre

summary.logis_cre <- function(object, parm, level = 0.95, null = 0, ...) {
  alpha <- 1 - level

  if (missing(object)) stop ("Argument 'object' is required!",call.=F)
  if (!class(object) %in% c("logis_cre")) stop("Object `object` is not of the classes 'logis_cre'!",call.=F)

  model <- attributes(object)$model

  covar_char <- c("(intercept)", object$char_list$within_terms, object$char_list$between_terms, object$char_list$other.vars)

  FE_est <- object$coefficient$FE
  se.FE <- sqrt(diag(object$variance$FE))
  stat <- (FE_est - null) / se.FE
  p_value <- 2 * (1 - pnorm(stat))
  p_value <- format.pval(p_value, digits = 7, eps = 1e-10)
  CI <- confint(model, parm = "beta_", method = "Wald", level = level)

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
