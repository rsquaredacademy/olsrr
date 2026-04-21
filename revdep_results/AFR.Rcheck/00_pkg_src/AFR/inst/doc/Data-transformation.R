## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE, message=FALSE-----------------------------------------
library(AFR)
library(stats)
library(tseries)

## ----echo=TRUE----------------------------------------------------------------
data(macroKZ)
checkdata(macroKZ)

## ----echo=TRUE----------------------------------------------------------------
macroKZ<-na.remove(macroKZ)

## ----results="hide"-----------------------------------------------------------
new<-log(macroKZ)

## ----results="hide"-----------------------------------------------------------
corsel(macroKZ,num=FALSE,thrs=0.65)

## ----echo=TRUE----------------------------------------------------------------
model<-lm(real_gdp~imp+exp+usdkzt+eurkzt, macroKZ)

