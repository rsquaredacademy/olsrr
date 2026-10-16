# ANOVA

ANOVA for regression model.

## Usage

``` r
ols_anova(model, ...)
```

## Arguments

- model:

  An object of class `lm`.

- ...:

  Other arguments.

## Value

`ols_anova` returns an object of class `"ols_anova"`. An object of class
`"ols_anova"` is a list containing the following components:

- error_df:

  residual degrees of freedom

- model_df:

  regression degrees of freedom

- total_df:

  total degrees of freedom

- ess:

  error sum of squares

- rss:

  regression sum of squares

- tss:

  total sum of squares

- rms:

  regression mean square

- ems:

  error mean square

- f:

  f statistis

- p:

  p-value for `f`

## Examples

``` r
# model
model <- lm(mpg ~ disp + hp + wt, data = mtcars)

# anova
ols_anova(model)
#>                                ANOVA                                 
#> --------------------------------------------------------------------
#>                 Sum of                                              
#>                Squares        DF    Mean Square      F         Sig. 
#> --------------------------------------------------------------------
#> Regression     931.057         3        310.352    44.566    0.0000 
#> Residual       194.991        28          6.964                     
#> Total         1126.047        31                                    
#> --------------------------------------------------------------------
#> 
```
