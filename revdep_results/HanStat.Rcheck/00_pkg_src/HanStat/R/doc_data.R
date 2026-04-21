#' Randomized data for testing models
#' Contains 5 Variables, one dependent, 4 independent. The fourth independent is correlated with the dependent
#' @docType data
#' @usage data(data)
#' @format data.frame
#' @keywords datasets
#' @references K.T.Krahl (2023)
#' @source https://www.hanseatic-statistics.de
#' @examples
#' data(data)
#' \donttest{LinReg('dv',c('iv_1','iv_2','iv_3'),data=data,BS=FALSE,NBS=1000,OC=FALSE,plot=TRUE)}
"data"
