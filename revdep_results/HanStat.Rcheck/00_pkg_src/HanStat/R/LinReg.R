#' @title LinReg
#' @source https://www.hanseatic-statistics.de
#' @description A simple multiple linear regression function (OLS) and it's requirements. The function automatically interprets the results, creates plots and provides an indication of violations of assumptions. It also calculates the effect sizes of the models. The bootstrapping method can also be used.
#' @param dv dependent variable name as a string
#' @param iv a string vector with the names of the independent variables, separated by commas, use c(iv_1,iv_2...iv_n)
#' @param data a data frame containing the variables
#' @param BS Bootstrapping method, set BS to TRUE or FALSE, if FALSE Number of bootstraps are ignored
#' @param NBS number of random samples used for bootstrapping
#' @param OC Outlier controll, set OS to TRUE or FALSE, to use cooks distance to exclude outliers, if BS==TRUE, OS must be FALSE
#' @param plot set plot to TRUE to create simple scatterplots of correlation between variables
#' @return the results of linear regression, plots and all requirements plus an interpretation & conclusion about the violations
#' @keywords Linear Regression
#' @importFrom stats coef cooks.distance lm pf resid shapiro.test
#' @importFrom utils install.packages installed.packages
#' @import devtools
#' @export
#' @examples
#' m<-LinReg('dv',c('iv_1','iv_2','iv_3'),data=data,BS=FALSE,NBS=1000,OC=FALSE,plot=TRUE)
#' print(m$Results)
#' print(m$Require)
#' print(m$Plots)


LinReg<-function(dv,iv,data,BS,NBS,OC,plot){
  ###data
  y<-data[dv]
  x<-data[iv]
  colnames(x)<-iv
  ###as numeric
  yx<-cbind(y,x)
  for (i in c(1:length(yx)))
    {
    yx[,i]<-as.numeric(yx[,i])
  }
  ###var and formula
  ###assign data frame object
  data_regression <- yx
  ###model
  formula_model<-paste(dv, paste(iv, collapse=" + "), sep=" ~ ")
  Results<-c()
  No_Violations<-0
  Require<-c()
  Plots<-c()
  ###Cooks Distance and BS Conditions
  if (OC==TRUE & BS==TRUE){
    warning(crayon::red('->->->Chose outlier controll or bootstrapping! Not both!'))
  }
  if (OC==TRUE & BS==FALSE){
    ###Cooks Distance
    model_raw<-lm(formula_model, data=data_regression)
    CooksD<-cooks.distance(model_raw)
    yx$Valid<-(CooksD<(4/(length(yx[,1]))))
    yx<-subset(yx,yx$Valid==TRUE)
    yx$Valid<-NULL
    data_regression <- yx
  }
  if ((OC==TRUE | OC==FALSE) & BS==FALSE){
  ####model
  model<-lm(formula_model,data=data_regression)
  names(model$coefficients)<-c('Intercept',iv)
  Results[['Linear Model']]<-summary(model)
  Fstat<-round(summary(model)$fstatistic[1],digits=3)
  df1<-round(summary(model)$fstatistic[2],digits=3)
  df2<-round(summary(model)$fstatistic[3],digits=3)
  p<-round(pf(Fstat,df1,df2,lower.tail=F),digits=3)
  r2<-round(summary(model)$adj.r.squared,digits=3)
  effect<-ifelse(r2/(1-r2)<.02,'no effect',
                 ifelse(r2/(1-r2)<.15 & r2/(1-r2)>=.02,'small effect',
                        ifelse(r2/(1-r2)<.35 & r2/(1-r2)>=.15, 'medium effect',
                               'large effect')))
  Results[['Model_interpretation']]<-ifelse(p<.05,(paste('The conducted model can explain a significant proportion of variance of',dv,'by the included independent variables (F(',df1,',',df2,')=',Fstat,', Rsquared=',r2,', p=',p,').')),
         (paste('The conducted model can not explain a significant proportion of variance of',dv,'by the included independent variables (F(',df1,',',df2,')=',Fstat,', Rsquared=',r2,', p=',p,').')))
  Results[['Model_effect_size']]<-(paste('According to Cohen(1992) this result can be described as a',effect,'(fsquared=',round(r2/(1-r2),digits=3),').'))
  Int_Coef<-c()
  for (i in c(1:length(iv)))
  {
    Int_Coef[[paste('Variable:',iv[i])]]<-paste('An increase in',iv[i],'by one unit results in an increase in',dv,'by',round(model$coefficients[i+1],digits=3),
    ifelse(summary(model)$coefficients[i,4]<.05, '. ***The result is significant***.','. The result is not significant.'))
  }
  Results[['Coefficients_interpretation']]<-Int_Coef
  ###requirements
  ##Autocorr
  x<-car::durbinWatsonTest(model, max.lag=1, simulate=TRUE, reps=1000,
                           method=c("normal"),
                           alternative=c("two.sided"))
  if (x[3]>0.05)
  {
    Require[['Autocorrelation']]<-paste('No evidence for autocorrelation.','Durbin-Watson-Statistics: ',round(x[[2]],digits=3),'Autocorrelation: ',round(x[[1]],digits=3),".")
  } else {
    Require[['Autocorrelation']]<-paste('Autocorrelation detected!','Durbin-Watson-Statistics: ',round(x[[2]],digits=3),'Autocorrelation: ',round(x[[1]],digits=3),".")
    No_Violations=No_Violations+1
  }
  ##Linearity
  x<-lmtest::resettest(model, power = 2:3, type = c("fitted", "regressor",
                                                    "princomp"), data = list())
  if (x$p.value>0.05)
  {
    Require[['Linearity']]<-paste('No evidence for non linearity.','RESET Statistics: ', round(x$statistic,digits=3),".")
  } else {
    Require[['Linearity']]<-paste('Evidence for non linearity!','RESET Statistics: ', round(x$statistic,digits=3),".")
    No_Violations=No_Violations+1
  }
  ##Multicoll
  x<-olsrr::ols_vif_tol(model)
  if (any(x$VIF>5)==FALSE)
  {
    Require[['Multicollinearity']]<-paste('No evidence for Multicollinearity.','VIF:', round(x$VIF,digits=3),".")
  } else {
    Require[['Multicollinearity']]<-paste('Evidence for Multicollinearity!','VIF:', round(x$VIF,digits=3),".")
    No_Violations=No_Violations+1
  }
  ##Homoscedasticity
  x<-lmtest::bptest(model, studentize=FALSE)
  if (x$p.value>0.05)
  {
    Require[['Homoscedasticity']]<-paste('No evidence for Heteroscedasticity.','Breusch Pagan Test :', round(x$statistic,digits=3),".")
  } else {
    Require[['Homoscedasticity']]<-paste('Evidence for Heteroscedasticity!','Breusch Pagan Test :', round(x$statistic,digits=3),".")
    No_Violations=No_Violations+1
  }
  #Normal dependent
  Residuen<-resid(model)
  x<-shapiro.test(Residuen)
  if (x$p.value>0.05)
  {
    Require[['Gaussian_assumption']]<-paste('No evidence for violation of Gaussian assumption.','Shapiro-Wilk Test :', round(x$statistic,digits=3),".")
  } else {
    Require[['Gaussian_assumption']]<-paste('Evidence for violation of Gaussian assumption!','Shapiro-Wilk Test :', round(x$statistic,digits=3),".")
    No_Violations=No_Violations+1
  }
  if (No_Violations==0)
  {
    Require[['Violations']]<-paste('->->->','Total number of violated requirements:',No_Violations,'!')
  }else{
    Require[['Violations']]<-paste('->->->','Total number of violated requirements:',No_Violations,'!')
  }
  }
  ###bootstrap
  if (OC==FALSE & BS==TRUE){
  # Bootstrap 95% CI for R-Squared
  # function to obtain R-Squared from the data
  rsq <- function(formula, data, indices) {
    d <- data[indices,] # allows boot to select sample
    fit <- lm(formula, data=d)
    return(summary(fit)$r.square)
  }
  #rename variables for formula
  ind_var=iv[1]
  if (length(iv)>1){
    for (i in c(2:length(iv))){
      ind_var=paste(ind_var,'+',iv[i])
    }
  }
  bs_formula=paste(dv,'~',ind_var)
  #BS
  result_bs<-boot::boot(data=data_regression, statistic=rsq,
             R=NBS, formula=bs_formula)
  Results[[1]]<-(paste('R-Squared: ', result_bs[1]))
  Results[[2]]<-(boot::boot.ci(result_bs, type="bca"))
  # Bootstrap 95% CI for result_bs coefficients
  # function to obtain regression weights
  bs <- function(formula, data, indices) {
    d <- data[indices,] # allows boot to select sample
    fit <- lm(formula, data=d)
    return(coef(fit))
  }
  # bootstrapping with  replications
  result_bs <- boot::boot(data=data_regression, statistic=bs,
                  R=NBS, formula=bs_formula)

  Results[[3]]<-result_bs
  # get 95% confidence intervals
  Results[[4]]<-(boot::boot.ci(result_bs, type="bca", index=1)) # intercept)
  Results[[5]]<-(boot::boot.ci(result_bs, type="bca", index=2)) # wt
  Results[[6]]<-(boot::boot.ci(result_bs, type="bca", index=3)) # disp
  }
  ###plot
  statement_out_boot<-ifelse(BS==TRUE & OC==TRUE,1,0)
  if (plot==TRUE & statement_out_boot==0)
  {
  ###+adjust grid.arrange
  plots<-c()
  names_plot<-c()
  counter=1
  for (i in c(2:(1+length(iv))))
  {
  yy<-colnames(data_regression)[1]
  xx<-colnames(data_regression)[i]
  plots[[counter]]<-eval(substitute(ggplot2::ggplot(data_regression, ggplot2::aes(x=data_regression[,(i)], y=data_regression[,1])) +
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method='lm', linetype="dotted",color="darkred")+
  ggplot2::labs(title=paste('Relationship between ',xx,' & ',yy), x=xx, y=yy),list(i = i)))
  name_plot<-paste('plot_',counter,sep = '')
  assign(name_plot,plots[[counter]])
  names_plot[[counter]]<-name_plot
  counter=counter+1
  }
  name_plot<-c()
  for (i in c(1:length(plots)))
  {
    name_plot[[i]]<-paste('plot_',i,",",sep = '')
  }
  ggpubr::ggarrange(NULL)
  name_plot<-unlist(name_plot)
  name_plot<-paste(name_plot,collapse = '')
  if (length(iv)!=2){
  Plots[[1]]<-eval(parse(text = paste("ggpubr::ggarrange(", name_plot,"ncol = ceiling(length(iv)/2), nrow = ceiling(length(iv)/2))")))
  }else{
  Plots[[1]]<-eval(parse(text = paste("ggpubr::ggarrange(", name_plot,"ncol = 2, nrow = ceiling(length(iv)/2))")))
  }
  }
Final_Results<-c()
if (BS==TRUE){
Final_Results[['Results']]<-Results
Final_Results[['Plots']]<-Plots
}else{
Final_Results[['Results']]<-Results
Final_Results[['Require']]<-Require
Final_Results[['Plots']]<-Plots
}
return(Final_Results)
}


