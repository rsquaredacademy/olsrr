
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xplorerr

> Tools for interactive data analysis

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/xplorerr)](https://cran.r-project.org/package=xplorerr)
[![R-CMD-check](https://github.com/rsquaredacademy/xplorerr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rsquaredacademy/xplorerr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

xplorerr provides a set of tools for interactive data analysis:

- Descriptive statistics
- Visualize probability distributions
- Inferential statistics
- Linear regression
- Logistic regression
- RFM Analysis
- Data visualization
  - ggplot2
  - plotly
  - rbokeh
  - highcharter

## Installation

``` r
# Install release version from CRAN
install.packages("xplorerr")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("rsquaredacademy/xplorerr")
```

## Usage

#### Descriptive Statistics

Generate descriptive statistics such as measures of location,
dispersion, frequency tables, cross tables, group summaries and multiple
one/two way tables.

``` r
app_descriptive()
```

#### Visualize Probability Distributions

Visualize and compute percentiles/probabilities of normal, t, f, chi
square and binomial distributions.

``` r
app_vistributions()
```

#### Inferential Statistics

Select set of parametric and non-parametric statistical tests. ‘inferr’
builds upon the solid set of statistical tests provided in ‘stats’
package by including additional data types as inputs, expanding and
restructuring the test results. The tests included are t tests, variance
tests, proportion tests, chi square tests, Levene’s test, McNemar Test,
Cochran’s Q test and Runs test.

``` r
app_inference()
```

#### Linear Regression

Tools designed to make it easier for users, particularly
beginner/intermediate R users to build ordinary least squares regression
models. Includes comprehensive regression output, heteroskedasticity
tests, collinearity diagnostics, residual diagnostics, measures of
influence, model fit assessment and variable selection procedures.

``` r
app_linear_regression()
```

#### Logistic Regression

Tools designed to make it easier for beginner and intermediate users to
build and validate binary logistic regression models. Includes bivariate
analysis, comprehensive regression output, model fit statistics,
variable selection procedures, model validation techniques and a ‘shiny’
app for interactive model building.

``` r
app_logistic_regression()
```

#### RFM Analysis

Tools for RFM (recency, frequency and monetary value) analysis. Generate
RFM score from both transaction and customer level data. Visualize the
relationship between recency, frequency and monetary value using
heatmap, histograms, bar charts and scatter plots.

``` r
app_rfm_analysis()
```

#### Data Visualization

Tools for interactive data visualization . Users can visualize data
using ‘ggplot2’, ‘plotly’, ‘rbokeh’ and ‘highcharter’ libraries.

``` r
app_visualizer()
```
