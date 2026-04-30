## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE, message=FALSE-----------------------------------------
library(AFR)
library(olsrr)
library(stats)

## ----echo=TRUE----------------------------------------------------------------
model<-lm(real_gdp~imp+exp+usdkzt+eurkzt, macroKZ)
opt_size(model)

## ----results="hide"-----------------------------------------------------------
check_betas(model)

## ----results="hide"-----------------------------------------------------------
dec_plot(model, macroKZ)

## ----results="hide"-----------------------------------------------------------
reg_plot(model, macroKZ)

