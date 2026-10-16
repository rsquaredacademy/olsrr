# Part and partial correlations

Zero-order, part and partial correlations.

## Usage

``` r
ols_correlations(model)
```

## Arguments

- model:

  An object of class `lm`.

## Value

`ols_correlations` returns an object of class `"ols_correlations"`. An
object of class `"ols_correlations"` is a data frame containing the
following components:

- Zero-order:

  zero order correlations

- Partial:

  partial correlations

- Part:

  part correlations

## Details

`ols_correlations()` returns the relative importance of independent
variables in determining response variable. How much each variable
uniquely contributes to rsquare over and above that which can be
accounted for by the other predictors? Zero order correlation is the
Pearson correlation coefficient between the dependent variable and the
independent variables. Part correlations indicates how much rsquare will
decrease if that variable is removed from the model and partial
correlations indicates amount of variance in response variable, which is
not estimated by the other independent variables in the model, but is
estimated by the specific variable.

## References

Morrison, D. F. 1976. Multivariate statistical methods. New York:
McGraw-Hill.

## Examples

``` r
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_correlations(model)
#>                Correlations                 
#> -------------------------------------------
#> Variable    Zero Order    Partial     Part  
#> -------------------------------------------
#> disp            -0.848      0.048     0.019 
#> hp              -0.776     -0.224    -0.093 
#> wt              -0.868     -0.574    -0.285 
#> qsec             0.419      0.219     0.091 
#> -------------------------------------------
```
