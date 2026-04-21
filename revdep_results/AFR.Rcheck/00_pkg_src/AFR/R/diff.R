#' @title Transforming time-series data to stationary
#' @description
#' Percent change is a change between two consecutive terms, %
#' @usage pct1(x)
#' @import stats
#' @param x time-series vector(s)
#' @examples
#' data (macroKZ)
#' new<-pct1(macroKZ)
#' @rdname pct1
#' @export

pct1<-function(x)((x/stats::lag(x)-1)*100)

#' @title Transforming time-series data to stationary
#' @description
#' Percent change is a change between a term and its lagged value for prior period, %
#' @usage pct4(x)
#' @import stats
#' @param x time-series vector(s)
#' @examples
#' data (macroKZ)
#' new<-pct4(macroKZ)
#' @rdname pct4
#' @export

pct4<-function(x){
  x4<-stats::lag(x,4)
  p<-(x4/x-1)*100
  return(p)
}

#tr<-function(x){
  #trend<-rollaply(m,width=period,fill=NA, align="center", FUN=mean,na.rm=TRUE)
  #season<-m-trend
  #figure<-numeric(period)
  #l<-length(m)
  #index<-seq.int(1,l,by=period)-1
  #for (i in 1:period) figure[i]<-median(season)
#}

#' @title Transforming time-series data to stationary
#' @description
#' Difference of logarithms is finding the difference between two consecutive logarithm values of a time-series
#' @param x time-series vector
#' @param difference difference between x items
#' @param lag lagged period
#' @import stats
#' @importFrom rlang abort
#' @importFrom xts diff.xts
#' @examples
#' data (macroKZ)
#' new<-pct1(macroKZ)
#' @rdname difflog
#' @export

difflog<-
  function (x, lag = 1, difference = 1)
  {
    if (!is.numeric(x))
      rlang::abort("Non-numeric data detected. 'x' must be numeric.")
    x<-log(x)
    ret_vec <- xts::diff.xts(x = x, lag = lag, differences = difference,
                             arithmetic = TRUE, na.pad = TRUE)
    pad_len <- length(x) - length(ret_vec)
    if (pad_len > 0) {
      ret_vec <- c(rep(NA, pad_len), ret_vec)
    }
    return(ret_vec)
  }



