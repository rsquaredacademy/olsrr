#' @title Necessary size of the time-series dataset
#' @description
#' Estimates number of models generated from given number of regressors X
#' @usage opt_size(model)
#' @param model is a linear regression model a class \code{lm}.
#' @examples
#' data(macroKZ)
#' model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
#' opt_size(model)
#' @importFrom nlme splitFormula
#' @export

opt_size<-function(model){
  model$call$formula<-as.formula(model)
  s<-splitFormula(model$call$formula, sep="+")
  s<-gsub("~",replacement="",x=s,ignore.case = TRUE)
  n<-length(s)
  f<-model$df.residual+1+n
  message<-"There is acceptable number of observations."
  if (f>n)
    message
  k<-as.integer(n*6)
  cat(paste(message),paste("It is necessary to have", k  ,"observations."), paste("Your regression has", f, "observations."), sep="\n")
  warning("If there is equal or close number of observations, please check further.")
}



