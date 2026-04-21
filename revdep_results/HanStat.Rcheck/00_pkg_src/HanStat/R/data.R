#' Radomized data for testing models
#'
#' @name HansStat Data
#'
#' @docType data
#'
#' @usage data(data)
#'
#' @format data.frame
#'
#' @keywords datasets
#'
#' @references K.T.Krahl (2023)
#'
#' @source https://www.hanseatic-statistics.de/
#'
#' @description  Contains 5 Variables, one dependent, 4 independent. The fourth independent is correlated with the dependent
#'
#' @examples
#' data(data)
#'
#' \donttest{
#' LinReg('dv',c('iv_1','iv_2','iv_3','iv_4'),data=data, BS = TRUE, NBS=1000, OC = TRUE, plot=TRUE)}
NULL

