#' Calculate direct/indirect standardized differences from a fitted `linear_fe` object
#'
#' Provide direct/indirect standardized differences for a fixed effect linear model.
#'
#' @param fit a model fitted from \code{linear_fe}.
#' @param parm specifies a subset of providers for which confidence intervals are to be given.
#' By default, all providers are included. The class of `parm` should match the class of the provider IDs.
#' @param stdz a character string or a vector specifying the standardization method(s).
#' The possible values are:
#' \itemize{
#'   \item{\code{"indirect"}} (default) indirect standardization method.
#'   \item{\code{"direct"}} direct standardization method.
#'   \item{\code{c("indirect", "direct")}} outputs both direct and indirect standardized measures.
#' }
#' @param null a character string or a number defining the population norm.
#' The default value is \code{"median"}. The possible values are:
#' \itemize{
#'   \item{\code{"median"}} the median of the provider effect estimates (\eqn{\hat{\gamma}_i}).
#'   \item{\code{"mean"}} the weighted average of the provider effect estimates (\eqn{\hat{\gamma}_i}), where the weights correspond to the sample size of each provider.
#'   \item{numeric} a user-defined numeric value representing the population norm.
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
#' This function computes standardized differences for a fixed effect linear model
#' using either direct or indirect methods, or both when specified.
#' For each method, the population norm is determined by the `null` argument.
#' The population norm can be the median of the estimates, their weighted mean
#' (with weights corresponding to provider sizes), or a user-defined numeric value.
#'
#' @examples
#' data(ExampleDataLinear)
#' outcome <- ExampleDataLinear$Y
#' covar <- ExampleDataLinear$Z
#' ProvID <- ExampleDataLinear$ProvID
#' fit_linear <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
#' SM_output(fit_linear)
#' SM_output(fit_linear, stdz = "direct", null = "mean")
#'
#' @importFrom stats median
#'
#' @exportS3Method SM_output linear_fe

SM_output.linear_fe <- function(fit, parm, stdz = "indirect", null = "median", ...) {
  if (missing(fit)) stop ("Argument 'fit' is required!",call.=F)
  if (!class(fit) %in% c("linear_fe")) stop("Object fit is not of the classes 'linear_fe'!",call.=F)
  if (!"indirect" %in% stdz & !"direct" %in% stdz) stop("Argument 'stdz' NOT as required!", call.=F)

  data <- fit$data_include
  prov <- data[ ,fit$char_list$ProvID.char]
  prov.name <- rownames(fit$coefficient$gamma)
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

  gamma <- fit$coefficient$gamma
  n.prov <- sapply(split(data[, fit$char_list$Y.char], data[, fit$char_list$ProvID.char]), length)
  gamma.null <- ifelse(null=="median", median(gamma, na.rm = TRUE),
                       ifelse(null=="mean", sum(n.prov*gamma, na.rm = TRUE)/n,
                              ifelse(class(null)=="numeric", null[1],
                                     stop("Argument 'null' NOT as required!",call.=F))))
  if ("indirect" %in% stdz) {
    Exp <- gamma.null + Z_beta
    Exp.indirect_provider <- sapply(split(Exp, prov), sum)
    Obs.indirect_provider <- sapply(split(fit$observation, prov), sum)

    indirect_stdz.diff <- matrix((Obs.indirect_provider - Exp.indirect_provider)/n.prov)
    dimnames(indirect_stdz.diff) <- list(rownames(gamma), "Indirect_standardized.difference")
    return_ls$indirect.difference <- indirect_stdz.diff[ind, ,drop = F]

    OE.df <- data.frame(Obs = Obs.indirect_provider, Exp = Exp.indirect_provider)
    OE_list$OE_indirect <- OE.df[ind, ]
  }

  if ("direct" %in% stdz) {
    Obs.direct_provider <- sum(gamma.null + fit$linear_pred)
    Exp.direct <- function(gamma){
      sum(gamma + Z_beta)
    }
    Exp.direct_provider <- sapply(gamma, Exp.direct)
    direct_stdz.diff <- matrix((Exp.direct_provider - Obs.direct_provider)/n)
    dimnames(direct_stdz.diff) <- list(rownames(gamma), "Direct_standardized.difference")
    return_ls$direct.difference <- direct_stdz.diff[ind, ,drop = F]

    OE.df <- data.frame(Obs = Obs.direct_provider, Exp = Exp.direct_provider)
    OE_list$OE_direct <- OE.df[ind, ]
  }

  return_ls$OE <- OE_list
  return(return_ls)
}
