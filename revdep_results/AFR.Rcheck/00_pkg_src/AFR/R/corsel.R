#' @title Multicollinearity test
#' @description
#' multicollinearity is the occurence of high interrelations among two or more independent variables in a multiple regression.
#' @param x is a numeric vector or matrix
#' @param thrs threshold set to calculate correlation above
#' @param num logical
#' @import stats
#' @examples
#' data(macroKZ)
#' corsel(macroKZ,num=FALSE,thrs=0.65)
#' @rdname corsel
#' @export


#data must be without period and NAs (d1<-d[,-1], d<-as.ts(macroKZ))

corsel<-
  function (x, thrs,num)

  {
    if (any(thrs > 1 | thrs < 0))
      stop("`thrs` should be on [0,1]", call. = FALSE)

    c_Rank<-ifelse(abs(cor(x)>=thrs),TRUE,FALSE)
    c_Rank<-as.data.frame(c_Rank)
    c<-abs(cor(x))
    if (num==FALSE)
      print(c_Rank)
    else
      print(round(c, digits=3))
}

    #for (c in 1:ncol(c_Rank)) {
      #R<-c()
      #for (r in 1:nrow(c_Rank)) {
        #if (c_Rank[r,c]==FALSE) {
          #R<-c(R,rownames(c_Rank)[r])
        #}
      #}
      #print(paste(c(colnames(c_Rank)[c], "has an appropriate correlation with", R), collapse=" "))
    #}




