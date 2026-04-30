#' Get confidence intervals for provider effects or standardized measures from a fitted `logis_re` object
#'
#' Provide confidence intervals for provider effects or standardized measures from a random effect logistic model.
#'
#' @param object a model fitted from \code{logis_re}.
#' @param parm specify a subset of providers for which confidence intervals are given.
#' By default, all providers are included. The class of `parm` should match the class of the provider IDs.
#' @param level the confidence level. The default value is 0.95.
#' @param option 	a character string specifying whether the confidence intervals
#' should be provided for provider effects or standardized measures:
#'   \itemize{
#'   \item {\code{"alpha"}} provider effect.
#'   \item {\code{"SM"}} standardized measures.
#'   }
#' @param measure a character string or a vector indicating whether the output measure is "ratio" or "rate" if \code{option = "SM"}.
#' Both "rate" and "ratio" will be provided by default.
#' @param stdz a character string or a vector specifying the standardization method
#' if `option` includes \code{"SM"}. See `stdz` argument in \code{\link{SM_output.linear_re}}.
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#' \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' Note that \code{"alpha"} for argument `option` only supports \code{"two.sided"}.
#' @param \dots additional arguments that can be passed to the function.
#'
#' @return A list of data frames containing the confidence intervals based on the values of `option` and `stdz`.
#' \item{CI.alpha}{Confidence intervals for provider effects if `option` includes \code{"alpha"}.}
#' \item{CI.indirect}{Confidence intervals for indirect standardized differences if `option` includes \code{"SM"} and `stdz` includes \code{"indirect"}.}
#' \item{CI.direct}{Confidence intervals for direct standardized differences if `option` includes \code{"SM"} and `stdz` includes \code{"direct"}.}
#'
#' @examples
#' data(ExampleDataBinary)
#' outcome <- ExampleDataBinary$Y
#' ProvID <- ExampleDataBinary$ProvID
#' covar <- ExampleDataBinary$Z
#' fit_re <- logis_re(Y = outcome, Z = covar, ProvID = ProvID)
#' confint(fit_re)
#'
#' @importFrom stats pnorm qnorm pt qt
#'
#' @exportS3Method confint logis_re

confint.logis_re <- function(object, parm, level = 0.95, option = "SM", measure = c("rate", "ratio"),
                             stdz = "indirect", alternative = "two.sided", ...) {
  return_ls <- list()

  alpha <- 1 - level

  if (missing(object)) stop ("Argument 'object' is required!",call.=F)
  if (!class(object) %in% c("logis_re")) stop("Object 'object' is not of the classes 'logis_re'!",call.=F)
  if (option != "alpha" & option != "SM") stop("Argument 'option' should be 'alpha' or 'SM'", call.=F)
  if (!"indirect" %in% stdz & !"direct" %in% stdz) stop("Argument 'stdz' NOT as required!", call.=F)
  if ("alpha" %in% option && alternative != "two.sided")
    stop("Provider effect (option = 'alpha') only supports two-sided confidence intervals.", call. = FALSE)

  data <- object$data_include
  prov <- data[ ,object$char_list$ProvID.char]
  n <- nrow(data)
  Y.char <- object$char_list$Y.char
  ProvID.char <- object$char_list$ProvID.char
  Z.char <- object$char_list$Z.char
  prov.name <- rownames(object$coefficient$RE)
  n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)
  Z_beta <- object$linear_pred

  REcoef <- object$coefficient$RE
  model <- attributes(object)$model
  postVar <- matrix(attr(ranef(model, condVar = TRUE)[[ProvID.char]], "postVar")[1,1,], ncol = 1)
  rownames(postVar) <- rownames(REcoef)
  colnames(postVar) <- "PostVar"
  se.alpha <- sqrt(postVar)

  if (alternative == "two.sided") {
    crit_value <- qnorm(1 - alpha / 2)

    U_alpha <- REcoef + crit_value * se.alpha
    L_alpha <- REcoef - crit_value * se.alpha
  }
  else if (alternative == "greater") {
    crit_value <- qnorm(1 - alpha)

    U_alpha <- rep(Inf, length(n.prov))
    L_alpha <- REcoef - crit_value * se.alpha
  }
  else if (alternative == "less") {
    crit_value <- qnorm(1 - alpha)

    U_alpha <- REcoef + crit_value * se.alpha
    L_alpha <- rep(-Inf, length(n.prov))
  }
  else {
    stop("Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
  }

  CI_alpha <- data.frame(alpha = REcoef, alpha.Lower = L_alpha, alpha.Upper = U_alpha)
  colnames(CI_alpha) <- c("Estimate", "alpha.Lower", "alpha.Upper")

  if (missing(parm)) {
    ind <- 1:length(prov.name)
  } else {
    if (is.numeric(parm)) {  #avoid "integer" class
      parm <- as.numeric(parm)
    }

    if (class(parm) == class(data[, ProvID.char])) {
      ind <- which(prov.name %in% parm)
    } else {
      stop("Argument 'parm' includes invalid elements!")
    }
  }

  if (option == "alpha") {
    attr(CI_alpha, "description") <- "Provider Effects"
    return (CI_alpha[ind, ])
    # return_ls$CI.alpha <- CI_alpha[ind, ]
  }


  # CI of SR
  else if (option == "SM") {
    population_rate <- sum(object$obs)/length(object$linear_pred) * 100  #sum(O_i)/N *100%
    if ("indirect" %in% stdz) {
      SR <- SM_output(object, stdz = "indirect", measure = c("ratio", "rate"))

      L.obs <- as.numeric(plogis(rep(L_alpha, n.prov) + object$linear_pred))
      L.prov <- sapply(split(L.obs, prov), sum)
      L_indirect <- L.prov / SR$OE$OE_indirect$Exp.indirect_provider

      U.obs <- as.numeric(plogis(rep(U_alpha, n.prov) + object$linear_pred))
      U.prov <- sapply(split(U.obs, prov), sum)
      U_indirect <- U.prov / SR$OE$OE_indirect$Exp.indirect_provider

      CI.indirect_ratio <- data.frame(SR = SR$indirect.ratio, indirect.Lower = L_indirect, indirect.Upper = U_indirect)
      colnames(CI.indirect_ratio) <- c("Indirect.Ratio", "Ratio.Lower", "Ratio.Upper")

      if ("ratio" %in% measure) {
        attr(CI.indirect_ratio, "confidence_level") <- paste(level, "%")
        attr(CI.indirect_ratio, "type") <- ifelse(alternative == "greater", "upper one-sided",
                                                  ifelse(alternative == "less", "lower one-sided",
                                                         "two-sided"))
        attr(CI.indirect_ratio, "description") <- "Indirect Standardized Ratio"
        attr(CI.indirect_ratio, "model") <- "RE logis"
        return_ls$CI.indirect_ratio <- CI.indirect_ratio[ind, ]
      }

      if ("rate" %in% measure) {
        L_indirect.rate <- pmax(pmin(CI.indirect_ratio$Ratio.Lower * population_rate, 100), 0)
        U_indirect.rate <- pmax(pmin(CI.indirect_ratio$Ratio.Upper * population_rate, 100), 0)

        CI.indirect_rate <- data.frame(SR = SR$indirect.rate, indirect.Lower = L_indirect.rate, indirect.Upper = U_indirect.rate)
        colnames(CI.indirect_rate) <- c("Indirect.Rate", "Rate.Lower", "Rate.Upper")
        attr(CI.indirect_rate, "confidence_level") <- paste(level, "%")
        attr(CI.indirect_rate, "type") <- ifelse(alternative == "greater", "upper one-sided",
                                                ifelse(alternative == "less", "lower one-sided",
                                                       "two-sided"))
        attr(CI.indirect_rate, "description") <- "Indirect Standardized Rate"
        attr(CI.indirect_rate, "model") <- "RE logis"
        attr(CI.indirect_rate, "population_rate") <- population_rate
        return_ls$CI.indirect_rate <- CI.indirect_rate[ind, ]
      }
    }

    if ("direct" %in% stdz) {
      SR <- SM_output(object, stdz = "direct", measure = c("ratio", "rate"))

      U.Exp <- computeDirectExp(U_alpha, Z_beta, threads = 4)
      U.direct_ratio <- U.Exp / rep(sum(object$obs), length(n.prov))

      L.Exp <- computeDirectExp(L_alpha, Z_beta, threads = 4)
      L.direct_ratio <- L.Exp / rep(sum(object$obs), length(n.prov))

      CI.direct_ratio <- data.frame(SR = SR$direct.ratio, direct.Lower = L.direct_ratio, direct.Upper = U.direct_ratio)
      colnames(CI.direct_ratio) <- c("Direct.Ratio", "Ratio.Lower", "Ratio.Upper")

      if ("ratio" %in% measure) {
        attr(CI.direct_ratio, "confidence_level") <- paste(level, "%")
        attr(CI.direct_ratio, "type") <- ifelse(alternative == "greater", "upper one-sided",
                                                ifelse(alternative == "less", "lower one-sided",
                                                       "two-sided"))
        attr(CI.direct_ratio, "description") <- "Direct Standardized Ratio"
        attr(CI.direct_ratio, "model") <- "RE logis"
        return_ls$CI.direct_ratio <- CI.direct_ratio[ind, ]
      }

      if ("rate" %in% measure) {
        L_direct.rate <- pmax(pmin(CI.direct_ratio$Ratio.Lower * population_rate, 100), 0)
        U_direct.rate <- pmax(pmin(CI.direct_ratio$Ratio.Upper * population_rate, 100), 0)

        CI.direct_rate <- data.frame(SR = SR$direct.rate, direct.Lower = L_direct.rate, direct.Upper = U_direct.rate)
        colnames(CI.direct_rate) <- c("Direct.Rate", "Rate.Lower", "Rate.Upper")
        attr(CI.direct_rate, "confidence_level") <- paste(level, "%")
        attr(CI.direct_rate, "type") <- ifelse(alternative == "greater", "upper one-sided",
                                               ifelse(alternative == "less", "lower one-sided",
                                                      "two-sided"))
        attr(CI.direct_rate, "description") <- "Direct Standardized Rate"
        attr(CI.direct_rate, "model") <- "RE logis"
        attr(CI.direct_rate, "population_rate") <- population_rate
        return_ls$CI.direct_rate <- CI.direct_rate[ind, ]
      }
    }
  }

  return(return_ls)
}
