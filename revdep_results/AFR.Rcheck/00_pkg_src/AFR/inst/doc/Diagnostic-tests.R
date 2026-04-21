## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE, message=FALSE-----------------------------------------
library(AFR)
library(lmtest)
library(stats)
library(olsrr)

## ----echo=TRUE----------------------------------------------------------------
model <- lm(real_gdp ~ imp + exp + poil + eurkzt,macroKZ)
bp(model)

## ----echo=TRUE----------------------------------------------------------------
model <- lm(real_gdp ~ imp + exp+poil+eurkzt, macroKZ)
gq(model)

## ----echo=TRUE----------------------------------------------------------------
model <- lm(real_gdp ~ imp + exp + poil + eurkzt,macroKZ)
vif_reg(model)

## ----echo=TRUE----------------------------------------------------------------
model <- lm(real_gdp ~ imp + exp + poil + eurkzt,macroKZ)
dwtest(model)

## ----echo=TRUE----------------------------------------------------------------
model <- lm(real_gdp ~ imp + exp + poil + eurkzt,macroKZ)
bg(model)

## ----echo=TRUE----------------------------------------------------------------
#model <- lm(real_gdp ~ imp + exp + poil + eurkzt,macroKZ)
#ols_test_normality(model)

