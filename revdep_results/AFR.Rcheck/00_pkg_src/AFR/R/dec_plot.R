#' @title Decomposition plot
#' @description
#' The function depicts decomposition of regressors as a stacked barplot
#' @param model An object of class \code{lm}.
#' @param dataset A dataset based on which model was built
#' @param print_plot logical
#' @examples
#' data(macroKZ)
#' model <- lm(real_gdp ~ usdkzt + eurkzt + imp+exp, data = macroKZ)
#' dec_plot(model, macroKZ)
#' @references Hebbali, Aravind. Published 2020-02-10. olssr package
#' @author The Agency of the Republic of Kazakhstan for Regulation and Development of Financial Market (AFR)
#' @import ggplot2
#' @importFrom nlme splitFormula
#' @importFrom zoo as.yearqtr
#' @export
#'

dec_plot <- function(model, dataset,print_plot = TRUE) {

  m<-matrix(1)
  model$call$formula<-as.formula(model)
  s<-splitFormula(model$call$formula, sep="+")
  s<-gsub("~",replacement="",x=s,ignore.case = TRUE)


  for (i in 1:length(s)){
    macro_name <- s[i]
    one_macro <- dataset[,macro_name]
    m<-cbind(m,one_macro)
  }

  tmp<-data.frame(t(coef(model)*t(m)))
  tmp<-tmp[,-1]
  tmp<-na.omit(tmp)
  names(tmp)<-s


  new<-data.frame()
  for (c in 1:ncol(tmp)) {
    t<-tmp[[c]]
    df<-data.frame(t)
    variable<-colnames(tmp)[[c]]
    df$variable <- variable
    df$date <- rownames(tmp)
    new<-rbind(new,df)
  }

  new$date<-as.yearqtr(time(dataset))


  d<-aggregate(new$t, by=list(date=new$date), sum)
  names(d)<-c("date", "value")


  p <-
    ggplot(new, aes(fill=variable, y=t,x=date))+
    geom_bar(position="stack", stat="identity")+
    scale_fill_brewer(palette="Paired")+
    guides(x = guide_axis(angle = 90))+
    #geom_line(data=d, aes(x=date,y=value, group=1))+
    xlab("period") + ylab("Value") +
    ggtitle("Decomposition plot")+
    theme(plot.title=element_text(face="bold", size=18,hjust=0.5))+
    theme(axis.title=element_text(face="bold"))


  if (print_plot) {
    print(p)
  } else {
    return(p)
  }

}

