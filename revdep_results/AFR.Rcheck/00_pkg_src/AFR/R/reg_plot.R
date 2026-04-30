#' @title Regression forecast plot
#' @description
#' The function depicts forecast and actual data.
#' @usage reg_plot(model, dataset)
#' @param model An object of class \code{lm}.
#' @param dataset A dataset based on which model was built.
#' @examples
#' data(macroKZ)
#' model <- lm(real_gdp ~ usdkzt + eurkzt + imp + exp, data = macroKZ)
#' reg_plot(model, macroKZ)
#' @author The Agency of the Republic of Kazakhstan for Regulation and Development of Financial Market (AFR)
#' @importFrom forecast auto.arima
#' @importFrom forecast forecast
#' @importFrom graphics legend lines
#' @export
#'

reg_plot <- function(model, dataset) {

  model$call$formula<-as.formula(model)
  #dataset<-as.data.frame(dataset)
  s<-splitFormula(model$call$formula, sep="+")
  s<-gsub("~",replacement="",x=s,ignore.case = TRUE)
  r<-model$call$formula[2]
  r
  c<-dataset[,colnames(dataset)[grep(r,colnames(dataset))]]

  ar<-auto.arima(c, stationary=FALSE, seasonal=FALSE)
  f<-forecast(ar, h=12)
  p<-predict(c,h=12)


  s<-setNames(f$model$arma, c("p", "q", "P", "Q", "m", "d", "D"))
  cat(paste("Parameters of the best ARIMA model are:"), sep="\n")
  print(as.table(s))

  plot(f,main="Forecast by Arima for 3 years ahead")
  lines(c,col="black")
  legend("topleft", legend=c("Arima","Actual"), col=c("light blue", "black"),lty=1:2, cex=0.8)

}



