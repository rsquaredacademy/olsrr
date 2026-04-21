#' Get confidence intervals for provider effects or standardized measures from a fitted `linear_cre` object
#'
#' Provide confidence intervals for provider effects or standardized measures from a correlated random effect linear model.
#'
#' @param object a model fitted from \code{linear_cre}.
#' @param parm specify a subset of providers for which confidence intervals are given.
#' By default, all providers are included. The class of `parm` should match the class of the provider IDs.
#' @param level the confidence level. The default value is 0.95.
#' @param option 	a character string specifying whether the confidence intervals
#' should be provided for provider effects or standardized measures:
#'   \itemize{
#'   \item {\code{"alpha"}} provider effect.
#'   \item {\code{"SM"}} standardized measures.
#'   }
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
#' data(ExampleDataLinear)
#' outcome <- ExampleDataLinear$Y
#' covar <- ExampleDataLinear$Z
#' ProvID <- ExampleDataLinear$ProvID
#' data <- data.frame(outcome, ProvID, covar)
#' outcome.char <- colnames(data)[1]
#' ProvID.char <- colnames(data)[2]
#' wb.char <- c("z1", "z2")
#' other.char <- c("z3", "z4", "z5")
#' fit_cre <- linear_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
#' wb.char = wb.char, other.char = other.char)
#' confint(fit_cre)
#'
#' @importFrom stats pnorm qnorm pt qt confint
#'
#' @exportS3Method confint linear_cre

confint.linear_cre <- function(object, parm, level = 0.95, option = "SM",
                               stdz = "indirect", alternative = "two.sided", ...) {
  return_ls <- list()

  alpha <- 1 - level

  if (missing(object)) stop ("Argument 'object' is required!",call.=F)
  if (!class(object) %in% c("linear_cre")) stop("Object 'object' is not of the classes 'linear_cre'!",call.=F)
  if (option != "alpha" & option != "SM") stop("Argument 'option' should be 'alpha' or 'SM'", call.=F)
  if (!"indirect" %in% stdz & !"direct" %in% stdz) stop("Argument 'stdz' NOT as required!", call.=F)
  if ("alpha" %in% option && alternative != "two.sided")
    stop("Provider effect (option = 'alpha') only supports two-sided confidence intervals.", call. = FALSE)

  data <- object$data_include
  prov <- data[ ,object$char_list$ProvID.char]
  n <- nrow(data)
  Y.char <- object$char_list$Y.char
  ProvID.char <- object$char_list$ProvID.char
  prov.name <- rownames(object$coefficient$RE)
  REcoef <- object$coefficient$RE
  n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)

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

    U_alpha <- Inf
    L_alpha <- REcoef - crit_value * se.alpha
  }
  else if (alternative == "less") {
    crit_value <- qnorm(1 - alpha)

    U_alpha <- REcoef + crit_value * se.alpha
    L_alpha <- -Inf
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
    if ("indirect" %in% stdz) {
      SR <- SM_output(object, stdz = "indirect")

      if (alternative == "two.sided") {
        L.obs <- rep(L_alpha, n.prov) + object$linear_pred
        L.prov <- sapply(split(L.obs, prov), sum)
        L_indirect <- (L.prov - SR$OE$OE_indirect$Exp)/n.prov

        U.obs <- rep(U_alpha, n.prov) + object$linear_pred
        U.prov <- sapply(split(U.obs, prov), sum)
        U_indirect <- (U.prov - SR$OE$OE_indirect$Exp)/n.prov
      }
      else if (alternative == "greater") {
        L.obs <- rep(L_alpha, n.prov) + object$linear_pred
        L.prov <- sapply(split(L.obs, prov), sum)
        L_indirect <- (L.prov - SR$OE$OE_indirect$Exp)/n.prov

        U_indirect <- Inf
      }
      else if (alternative == "less") {
        U.obs <- rep(U_alpha, n.prov) + object$linear_pred
        U.prov <- sapply(split(U.obs, prov), sum)
        U_indirect <- (U.prov - SR$OE$OE_indirect$Exp)/n.prov

        L_indirect <- -Inf
      }
      else {
        stop("Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
      }

      CI_indirect <- data.frame(SR = SR$indirect.difference, indirect.Lower = L_indirect, indirect.Upper = U_indirect)
      colnames(CI_indirect) <- c("Indirect.Difference", "indirect.Lower", "indirect.Upper")
      attr(CI_indirect, "confidence_level") <- paste(level, "%")
      attr(CI_indirect, "type") <- ifelse(alternative == "greater", "upper one-sided",
                                          ifelse(alternative == "less", "lower one-sided",
                                                 "two-sided"))
      attr(CI_indirect, "description") <- "Indirect Standardized Difference"
      attr(CI_indirect, "model") <- "CRE linear"
      return_ls$CI.indirect <- CI_indirect[ind, ]
    }

    if ("direct" %in% stdz) {
      SR <- SM_output(object, stdz = "direct")

      Exp.direct <- function(alpha){
        sum(alpha + object$linear_pred)
      }

      if (alternative == "two.sided") {
        L.prov <- sapply(L_alpha, Exp.direct)
        L_direct <- (L.prov - SR$OE$OE_direct$Obs)/n

        U.prov <- sapply(U_alpha, Exp.direct)
        U_direct <- (U.prov - SR$OE$OE_direct$Obs)/n
      }
      else if (alternative == "greater") {
        L.prov <- sapply(L_alpha, Exp.direct)
        L_direct <- (L.prov - SR$OE$OE_direct$Obs)/n

        U_direct <- Inf
      }
      else if (alternative == "less") {
        U.prov <- sapply(U_alpha, Exp.direct)
        U_direct <- (U.prov - SR$OE$OE_direct$Obs)/n

        L_direct <- -Inf
      }
      else {
        stop("Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
      }


      CI_direct <- data.frame(SR = SR$direct.difference, direct.Lower = L_direct, direct.Upper = U_direct)
      colnames(CI_direct) <- c("Direct.Difference", "direct.Lower", "direct.Upper")
      attr(CI_direct, "confidence_level") <- paste(level, "%")
      attr(CI_direct, "type") <- ifelse(alternative == "greater", "upper one-sided",
                                        ifelse(alternative == "less", "lower one-sided",
                                               "two-sided"))
      attr(CI_direct, "description") <- "Direct Standardized Difference"
      attr(CI_direct, "model") <- "CRE linear"
      return_ls$CI.direct <- CI_direct[ind, ]
    }
  }

  return(return_ls)
}
