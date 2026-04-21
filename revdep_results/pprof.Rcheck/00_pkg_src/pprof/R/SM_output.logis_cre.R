#' Calculate direct/indirect standardized ratios/rates from a fitted `logis_cre` object
#'
#' Provide direct/indirect standardized ratios/rates for a correlated random effect logistic model.
#'
#' @param fit a model fitted from \code{logis_cre}.
#' @param parm specifies a subset of providers for which confidence intervals are to be given.
#' By default, all providers are included. The class of `parm` should match the class of the provider IDs.
#' @param stdz a character string or a vector specifying the standardization method(s).
#' The possible values are:
#' \itemize{
#'   \item{\code{"indirect"}} (default) indirect standardization method.
#'   \item{\code{"direct"}} direct standardization method.
#'   \item{\code{c("indirect", "direct")}} outputs both direct and indirect standardized measures.
#' }
#' @param measure a character string or a vector indicating whether the output measure is "ratio" or "rate"
#' \itemize{
#'   \item{\code{"rate"}} output the standardized rate. The "rate" has been restricted to 0% - 100%.
#'   \item{\code{"ratio"}} output the standardized ratio.
#'   \item{\code{c("ratio", "rate")}} (default) output both the ratio and rate.
#' }
#' @param threads an integer specifying the number of threads to use. The default value is 2.
#' @param \dots additional arguments that can be passed to the function.
#'
#'
#' @return A list contains standardized measures, as well as the observed and expected outcomes used for calculation,
#' depending on the user's choice of standardization method (`stdz`) and measure type (`measure`).
#' \item{indirect.ratio}{standardization ratio using indirect method if `stdz` includes \code{"indirect"} and `measure` includes \code{"ratio"}.}
#' \item{direct.ratio}{standardization ratio using direct method if `stdz` includes \code{"direct"} and `measure` includes \code{"ratio"}.}
#' \item{indirect.rate}{standardization rate using indirect method if `stdz` includes \code{"indirect"} and `measure` includes \code{"rate"}.}
#' \item{direct.rate}{standardization rate using direct method if `stdz` includes \code{"direct"} and `measure` includes \code{"rate"}.}
#' \item{OE}{a list of data frames containing the observed and expected outcomes used for calculating standardized measures.}
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
#'
#' SR <- SM_output(fit_cre, stdz = "direct", measure = "rate")
#' SR$direct.rate
#'
#' @exportS3Method SM_output logis_cre

SM_output.logis_cre <- function(fit, parm, stdz = "indirect", measure = c("rate", "ratio"), threads = 2, ...){

  if (missing(fit)) stop ("Argument 'fit' is required!",call.=F)
  if (!class(fit) %in% c("logis_cre")) stop("Object fit is not of the classes 'logis_cre'!", call.=F)
  if (!"indirect" %in% stdz & !"direct" %in% stdz) stop("Argument 'stdz' NOT as required!", call.=F)
  if (!"rate" %in% measure & !"ratio" %in% measure)stop("Argument 'measure' NOT as required!", call.=F)

  Y.char <- fit$char_list$Y.char
  ProvID.char <- fit$char_list$ProvID.char
  Z.char <- fit$char_list$Z.char
  alpha <- fit$coefficient$RE
  data <- fit$data_include
  Z_beta <- fit$linear_pred
  prov <- data[ ,ProvID.char]
  prov.name <- rownames(fit$coefficient$RE)

  return_ls <- list()
  OE_list <- list()

  if (missing(parm)) {
    ind <- 1:length(prov.name)
  } else {
    if (is.numeric(parm)) {  #avoid "integer" class
      parm <- as.numeric(parm)
    }
    if (class(parm) == class(data[, ProvID.char])) {
      ind <- which(prov.name %in% parm)
    } else {
      stop("Argument 'parm' includes invalid elements.")
    }
  }

  if ("indirect" %in% stdz) {
    n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)
    Obs <- fit$fitted
    Exp <- as.numeric(plogis(Z_beta)) # expected prob of events under alpha = 0

    df.prov <- data.frame(Obs.indirect_provider = sapply(split(Obs, prov), sum),
                          Exp.indirect_provider = sapply(split(Exp, prov), sum))
    rownames(df.prov) <- rownames(alpha)

    OE_list$OE_indirect <- df.prov[ind, ]
    df.prov$IS_Ratio <- df.prov$Obs.indirect_provider / df.prov$Exp.indirect_provider #indirect standardized ratio: O_i/E_i
    if ("ratio" %in% measure){
      indirect_stdz.ratio <- matrix(df.prov$IS_Ratio)
      dimnames(indirect_stdz.ratio) <- list(rownames(alpha), "Indirect_standardized.ratio")
      return_ls$indirect.ratio <- indirect_stdz.ratio[ind, ,drop = F]
    }
    if ("rate" %in% measure) {
      population_rate <- sum(fit$observation)/nrow(data) * 100  #sum(O_i)/N *100%
      df.prov$IS_Rate <- pmax(pmin(df.prov$IS_Ratio * population_rate, 100), 0)  #restricted to 0%-100%
      indirect_stdz.rate <- matrix(df.prov$IS_Rate)
      dimnames(indirect_stdz.rate) <- list(rownames(alpha), "Indirect_standardized.rate")
      return_ls$indirect.rate <- indirect_stdz.rate[ind, ,drop = F]
    }
  }

  if ("direct" %in% stdz) {
    # if (Rcpp) {
    #   Exp <- computeDirectExp(gamma.prov, Z_beta, threads)
    # } else {
    #   exp_ZB <- exp(Z_beta)
    #   Exp.direct <- function(gamma){
    #     numer <- exp(gamma) * exp_ZB
    #     sum(1/(1 + 1/numer))
    #   }
    #   Exp <- sapply(gamma.prov, Exp.direct)
    # }

    Exp <- computeDirectExp(alpha, Z_beta, threads)
    df.prov <- data.frame(Obs_all = rep(sum(fit$observation), length(alpha)), #denominator
                          Exp.direct_all = Exp)  #numerator
    rownames(df.prov) <- rownames(alpha)
    OE_list$OE_direct <- df.prov[ind, ]
    df.prov$DS_ratio <- df.prov$Exp.direct_all / df.prov$Obs_all #direct standardized ratio: E/sum(O)
    if ("ratio" %in% measure){
      direct_stdz.ratio <- matrix(df.prov$DS_ratio)
      dimnames(direct_stdz.ratio) <- list(rownames(alpha), "Direct_standardized.ratio")
      return_ls$direct.ratio <- direct_stdz.ratio[ind, ,drop = F]
    }
    if ("rate" %in% measure) {
      population_rate <- sum(fit$observation)/length(Z_beta) * 100  #sum(O_i)/N *100%
      df.prov$DS_Rate <- pmax(pmin(df.prov$DS_ratio * population_rate, 100), 0)  #restricted to 0%-100%
      direct_stdz.rate <- matrix(df.prov$DS_Rate)
      dimnames(direct_stdz.rate) <- list(rownames(alpha), "Direct_standardized.rate")
      return_ls$direct.rate <- direct_stdz.rate[ind, ,drop = F]
    }
  }
  return_ls$OE <- OE_list

  return(return_ls)
}
