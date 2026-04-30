#' Calculate direct/indirect standardized differences from a fitted `linear_cre` object
#'
#' Provide direct/indirect standardized differences for a correlated random effect linear model.
#'
#' @param fit a model fitted from \code{linear_cre}.
#' @param parm specifies a subset of providers for which confidence intervals are to be given.
#' By default, all providers are included. The class of `parm` should match the class of the provider IDs.
#' @param stdz a character string or a vector specifying the standardization method(s).
#' The possible values are:
#' \itemize{
#'   \item{\code{"indirect"}} (default) indirect standardization method.
#'   \item{\code{"direct"}} direct standardization method.
#'   \item{\code{c("indirect", "direct")}} outputs both direct and indirect standardized measures.
#' }
#' @param \dots additional arguments that can be passed to the function.
#'
#' @return A list containing the standardized differences based on the method(s) specified in `stdz`,
#' as well as the observed and expected outcomes used to calculate the standardized measures:
#' \item{indirect.difference}{indirect standardized differences, if `stdz` includes \code{"indirect"}.}
#' \item{direct.difference}{direct standardized differences, if `stdz` includes \code{"direct"}.}
#' \item{OE}{a list of data frames containing the observed and expected outcomes used for calculating standardized measures.}
#'
#' @details
#' This function computes standardized differences for a random effect linear model
#' using either direct or indirect methods, or both when specified.
#' The function returns both the standardized differences and the observed and expected outcomes
#' used for their calculation.
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
#' SM_output(fit_cre)
#'
#' @exportS3Method SM_output linear_cre

SM_output.linear_cre <- function(fit, parm, stdz = "indirect", ...) {
  if (missing(fit)) stop ("Argument 'fit' is required!",call.=F)
  if (!class(fit) %in% c("linear_cre")) stop("Object fit is not of the classes 'linear_cre'!",call.=F)
  if (!"indirect" %in% stdz & !"direct" %in% stdz) stop("Argument 'stdz' NOT as required!", call.=F)

  data <- fit$data_include
  prov <- data[ ,fit$char_list$ProvID.char]
  prov.name <- rownames(fit$coefficient$RE)
  alpha.prov <- fit$coefficient$RE
  Z_beta <- fit$linear_pred
  n <- nrow(fit$data_include)

  return_ls <- list()
  OE_list <- list()

  if (missing(parm)) {
    ind <- 1:length(prov.name)
  } else {
    if (is.numeric(parm)) {  #avoid "integer" class
      parm <- as.numeric(parm)
    }
    if (class(parm) == class(data[, fit$char_list$ProvID.char])) {
      ind <- which(prov.name %in% parm)
    } else {
      stop("Argument 'parm' includes invalid elements.")
    }
  }

  if ("indirect" %in% stdz) {
    n.prov <- sapply(split(data[, fit$char_list$Y.char], data[, fit$char_list$ProvID.char]), length)

    Exp <- Z_beta
    Exp.indirect_provider <- sapply(split(Exp, prov), sum)
    Obs.indirect_provider <- sapply(split(fit$fitted, prov), sum)

    indirect_stdz.diff <- matrix((Obs.indirect_provider - Exp.indirect_provider)/n.prov)
    dimnames(indirect_stdz.diff) <- list(rownames(alpha.prov), "Indirect_standardized.difference")
    return_ls$indirect.difference <- indirect_stdz.diff[ind, ,drop = F]

    OE.df <- data.frame(Obs = Obs.indirect_provider, Exp = Exp.indirect_provider)
    OE_list$OE_indirect <- OE.df[ind, ]
  }

  if ("direct" %in% stdz) {
    Obs.direct_provider <- sum(fit$observation)
    Exp.direct <- function(alpha){
      sum(alpha + Z_beta)
    }
    Exp.direct_provider <- sapply(alpha.prov, Exp.direct)
    direct_stdz.diff <- matrix((Exp.direct_provider - Obs.direct_provider)/n)
    dimnames(direct_stdz.diff) <- list(rownames(alpha.prov), "Direct_standardized.difference")
    return_ls$direct.difference <- direct_stdz.diff[ind, , drop = F]

    OE.df <- data.frame(Obs = Obs.direct_provider, Exp = Exp.direct_provider)
    OE_list$OE_direct <- OE.df[ind, ]
  }

  return_ls$OE <- OE_list
  return(return_ls)
}
