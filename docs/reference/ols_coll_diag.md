# Collinearity diagnostics

Variance inflation factor, tolerance, eigenvalues and condition indices.

## Usage

``` r
ols_coll_diag(model)

ols_vif_tol(model)

ols_eigen_cindex(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

`ols_coll_diag` returns an object of class `"ols_coll_diag"`. An object
of class `"ols_coll_diag"` is a list containing the following
components:

- vif_t:

  tolerance and variance inflation factors

- eig_cindex:

  eigen values and condition index

## Details

Collinearity implies two variables are near perfect linear combinations
of one another. Multicollinearity involves more than two variables. In
the presence of multicollinearity, regression estimates are unstable and
have high standard errors.

*Tolerance*

Percent of variance in the predictor that cannot be accounted for by
other predictors.

Steps to calculate tolerance:

- Regress the kth predictor on rest of the predictors in the model.

- Compute \\R^2\\ - the coefficient of determination from the regression
  in the above step.

- \\Tolerance = 1 - R^2\\

*Variance Inflation Factor*

Variance inflation factors measure the inflation in the variances of the
parameter estimates due to collinearities that exist among the
predictors. It is a measure of how much the variance of the estimated
regression coefficient \\\beta_k\\ is inflated by the existence of
correlation among the predictor variables in the model. A VIF of 1 means
that there is no correlation among the kth predictor and the remaining
predictor variables, and hence the variance of \\\beta_k\\ is not
inflated at all. The general rule of thumb is that VIFs exceeding 4
warrant further investigation, while VIFs exceeding 10 are signs of
serious multicollinearity requiring correction.

Steps to calculate VIF:

- Regress the kth predictor on rest of the predictors in the model.

- Compute \\R^2\\ - the coefficient of determination from the regression
  in the above step.

- \\Tolerance = 1 / 1 - R^2 = 1 / Tolerance\\

*Condition Index*

Most multivariate statistical approaches involve decomposing a
correlation matrix into linear combinations of variables. The linear
combinations are chosen so that the first combination has the largest
possible variance (subject to some restrictions), the second combination
has the next largest variance, subject to being uncorrelated with the
first, the third has the largest possible variance, subject to being
uncorrelated with the first and second, and so forth. The variance of
each of these linear combinations is called an eigenvalue. Collinearity
is spotted by finding 2 or more variables that have large proportions of
variance (.50 or more) that correspond to large condition indices. A
rule of thumb is to label as large those condition indices in the range
of 30 or larger.

## References

Belsley, D. A., Kuh, E., and Welsch, R. E. (1980). Regression
Diagnostics: Identifying Influential Data and Sources of Collinearity.
New York: John Wiley & Sons.

## Examples

``` r
# model
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)

# vif and tolerance
ols_vif_tol(model)
#>   Variables Tolerance      VIF
#> 1      disp 0.1218116 8.209402
#> 2        hp 0.3454979 2.894373
#> 3        wt 0.1962092 5.096601
#> 4      drat 0.4386836 2.279547

# eigenvalues and condition indices
ols_eigen_cindex(model)
#>    Eigenvalue Condition Index    intercept        disp          hp           wt
#> 1 4.692806914        1.000000 0.0002323252 0.001106455 0.002566185 0.0007172086
#> 2 0.240308641        4.419078 0.0036813894 0.034132904 0.031334562 0.0009394254
#> 3 0.052153430        9.485821 0.0009192095 0.058394262 0.735003722 0.0700789813
#> 4 0.011406889       20.283026 0.0014476535 0.885725642 0.207337511 0.7179834661
#> 5 0.003324127       37.573144 0.9937194224 0.020640737 0.023758021 0.2102809185
#>           drat
#> 1 0.0003775503
#> 2 0.0148250672
#> 3 0.0026259361
#> 4 0.0568226912
#> 5 0.9253487552

# collinearity diagnostics
ols_coll_diag(model)
#> Tolerance and Variance Inflation Factor
#> ---------------------------------------
#>   Variables Tolerance      VIF
#> 1      disp 0.1218116 8.209402
#> 2        hp 0.3454979 2.894373
#> 3        wt 0.1962092 5.096601
#> 4      drat 0.4386836 2.279547
#> 
#> 
#> Eigenvalue and Condition Index
#> ------------------------------
#>    Eigenvalue Condition Index    intercept        disp          hp           wt
#> 1 4.692806914        1.000000 0.0002323252 0.001106455 0.002566185 0.0007172086
#> 2 0.240308641        4.419078 0.0036813894 0.034132904 0.031334562 0.0009394254
#> 3 0.052153430        9.485821 0.0009192095 0.058394262 0.735003722 0.0700789813
#> 4 0.011406889       20.283026 0.0014476535 0.885725642 0.207337511 0.7179834661
#> 5 0.003324127       37.573144 0.9937194224 0.020640737 0.023758021 0.2102809185
#>           drat
#> 1 0.0003775503
#> 2 0.0148250672
#> 3 0.0026259361
#> 4 0.0568226912
#> 5 0.9253487552
```
