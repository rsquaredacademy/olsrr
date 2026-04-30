#' @title Soil Quality Index Based on Linear Scoring
#' @param DataFrame Data set with first column as factors
#' @param OptimumValue Optimum value of each variable; Minimum and maximum coded as "1111" and "9999" respectively.
#' @import readxl dplyr stats matrixStats olsrr FactoMineR
#' @return
#' \itemize{
#'   \item Raw_mean: Raw score
#'   \item Index: Final index
#' }
#' @export
#'
#' @examples
#' library("SQI")
#' OP<-c(7,1111,9999,9999,9999,9999,9999,9999,9999,9999,1111)
#' ScoreIndex<-ScoingIndex(DataFrame = Data,OptimumValue = OP)
#' @references
#' \itemize{
#'\item Bastida, F., Zsolnay, A., Hernández, T., & García, C. (2008). Past, present and future of soil quality indices: a biological perspective. Geoderma, 147(3-4), 159-171.

#' \item Doran, J. W., & Parkin, T. B. (1994). Defining and assessing soil quality. Defining soil quality for a sustainable environment, 35, 1-21.

#' \item Mukherjee, A., & Lal, R. (2014). Comparison of soil quality index using three methods. PloS one, 9(8), e105981.
#' }

ScoingIndex<-function(DataFrame,OptimumValue){
  optimum_value<-OptimumValue
  data<-DataFrame
  n_opt<-length(optimum_value)
  a<-as.matrix(unique(data[,1]))
  n_group<-length(a)
  data_indv<-NULL
  mean<-NULL
  for (g in c(1:n_group)) {
    name<-paste("data","_",g)
    name1<-paste("mean","_",g)
    name1<-NULL
    name<-subset(data, data[,1]==a[g])
    name<-as.data.frame(name)
    n_var<-ncol(name)
    if(n_var !=(n_opt+1)){
      stop("Number of optimum values is not matching with number of variables ")
    }
    VAR<-NULL
    for (v in c(1:n_opt)){
      var_mean<-paste("var","_",v)
      if(is.numeric(optimum_value[v]==1111)){
        var_mean<-mean(min(name[,(v+1)])/name[,(v+1)])
      }else if(is.numeric(optimum_value[v]==9999)){
        var_mean<-mean(name[,(v+1)]/max(name[,(v+1)]))
      }else{
        var_mean<-mean(name[,(v+1)]/as.vector(optimum_value[v]))
      }
      VAR<-cbind(VAR,var_mean)
    }
    mean<-rbind(mean,VAR)
  }
  rownames(mean)<-a
  colnames(mean)<- colnames(name[,-1])

  min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  index<- min_max_norm(as.matrix(rowMeans(mean)))
  colnames(index)<-"Index"
  return(list(Raw_mean=mean,Index=index))
}

#' @title Soil Quality Index Based on Regression
#' @param DataFrame Data set with first column as factors
#' @param OptimumValue Optimum value of each variable; Minimum and maximum coded as "1111" and "9999" respectively.
#' @param Dep_col Dependent variable column number
#' @import readxl dplyr stats matrixStats olsrr FactoMineR
#' @return
#' \itemize{
#'   \item RegIndex: Final index
#' }
#' @export
#'
#' @examples
#' library("SQI")
#' OP<-c(7,1111,9999,9999,9999,9999,9999,9999,9999,9999,1111)
#' RIndex<-RegIndex(DataFrame = Data,Dep_col=7,OptimumValue = OP)
#' @references
#' \itemize{
#'\item Bastida, F., Zsolnay, A., Hernández, T., & García, C. (2008). Past, present and future of soil quality indices: a biological perspective. Geoderma, 147(3-4), 159-171.

#' \item Doran, J. W., & Parkin, T. B. (1994). Defining and assessing soil quality. Defining soil quality for a sustainable environment, 35, 1-21.

#' \item Mukherjee, A., & Lal, R. (2014). Comparison of soil quality index using three methods. PloS one, 9(8), e105981.
#' }

RegIndex<-function(DataFrame, Dep_col,OptimumValue){
  Data<-DataFrame
  C_Number<-Dep_col
  colnames(Data)[C_Number] <- "Dep"
  model <- lm(Dep~.-1, data=Data[,-1])
  both <- step(model, direction='both', trace = 0)
  coefficient<-t(as.data.frame(both$coefficients))
  sel_var<-colnames(coefficient)
  RawIndex<-ScoingIndex(DataFrame = DataFrame,OptimumValue = OptimumValue)$Raw_mean
  Sel_RawIndex<-as.data.frame(RawIndex[,c(sel_var)])
  RI<-NULL
  for (q in c(1:nrow(Sel_RawIndex))) {
    row_pred<-sum(Sel_RawIndex[q,]*coefficient)/length(sel_var)
    RI<-rbind(RI,row_pred)
  }
  colnames(RI)<-"RI"
  row.names(RI)<-row.names(Sel_RawIndex)
  min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  Index<- apply(RI,2,min_max_norm)
  colnames(Index)<-"Index"
  return(RegIndex=Index)
}

#' @title Soil Quality Index Based on Regression
#' @param DataFrame Data set with first column as factors
#' @param OptimumValue Optimum value of each variable; Minimum and maximum coded as "1111" and "9999" respectively.
#' @import readxl dplyr stats matrixStats olsrr FactoMineR
#' @return
#' \itemize{
#'   \item PCAIndex: Final index
#' }
#' @export
#'
#' @examples
#'library("SQI")
#' OP<-c(7,1111,9999,9999,9999,9999,9999,9999,9999,9999,1111)
#' PIndex<-PCAIndex(DataFrame = Data,OptimumValue = OP)
#' @references
#' \itemize{
#'\item Bastida, F., Zsolnay, A., Hernández, T., & García, C. (2008). Past, present and future of soil quality indices: a biological perspective. Geoderma, 147(3-4), 159-171.

#' \item Doran, J. W., & Parkin, T. B. (1994). Defining and assessing soil quality. Defining soil quality for a sustainable environment, 35, 1-21.

#' \item Mukherjee, A., & Lal, R. (2014). Comparison of soil quality index using three methods. PloS one, 9(8), e105981.
#' }
PCAIndex<-function(DataFrame, OptimumValue){
  Data<-DataFrame
  data <- scale((Data[,-1]))
  pca_res <- PCA(data)
  sel_pca<-names(subset(pca_res$eig[,1],pca_res$eig[,1]>1))
  no_pca<-length(sel_pca)
  variance<-pca_res$eig[,2]
  factor_lod<-pca_res$svd$V[,1:length(sel_pca)]
  row.names(factor_lod)<-colnames(data)
  sel_variance<-variance[1:no_pca]
  weight<-sel_variance/sum(sel_variance)
  Sel_Var<-NULL
  for (p in 1:no_pca) {
    sel_pvar<-names(sort(factor_lod[,1], decreasing = TRUE))[c(1:4)]
    data_cor<-data[,c(sel_pvar)]
    Cor<- apply(cor(data_cor),1,sum)
    sel_var_l<- names(sort(Cor, decreasing = TRUE))[1]
    Sel_Var<-c(Sel_Var,sel_var_l)
  }
  sel_var<-Sel_Var
  RawIndex<-ScoingIndex(DataFrame = DataFrame,OptimumValue = OptimumValue)$Raw_mean
  Sel_RawIndex<-as.data.frame(RawIndex[,c(sel_var)])
  RI<-NULL
  for (q in c(1:nrow(Sel_RawIndex))) {
    row_pred<-sum(Sel_RawIndex[q,]*as.vector(weight))/no_pca
    RI<-rbind(RI,row_pred)
  }
  colnames(RI)<-"RI"
  row.names(RI)<-row.names(Sel_RawIndex)
  min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  Index<- apply(RI,2,min_max_norm)
  colnames(Index)<-"Index"
  return(PCAIndex=Index)
}

#' This is data to be included in my package
#' @name Data
#' @docType data
#' @keywords datasets
#' @usage data(Data)
#' @format A data frame with 60 rows and 12 column
NULL
