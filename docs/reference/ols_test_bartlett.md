# Bartlett test

Test if k samples are from populations with equal variances.

## Usage

``` r
ols_test_bartlett(data, ...)

# Default S3 method
ols_test_bartlett(data, ..., group_var = NULL)
```

## Arguments

- data:

  A `data.frame` or `tibble`.

- ...:

  Columns in `data`.

- group_var:

  Grouping variable.

## Value

`ols_test_bartlett` returns an object of class `"ols_test_bartlett"`. An
object of class `"ols_test_bartlett"` is a list containing the following
components:

- fstat:

  f statistic

- pval:

  p-value of `fstat`

- df:

  degrees of freedom

## Details

Bartlett's test is used to test if variances across samples is equal. It
is sensitive to departures from normality. The Levene test is an
alternative test that is less sensitive to departures from normality.

## References

Snedecor, George W. and Cochran, William G. (1989), Statistical Methods,
Eighth Edition, Iowa State University Press.

## See also

Other heteroskedasticity tests:
[`ols_test_breusch_pagan()`](https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.md),
[`ols_test_f()`](https://olsrr.rsquaredacademy.com/reference/ols_test_f.md),
[`ols_test_score()`](https://olsrr.rsquaredacademy.com/reference/ols_test_score.md)

## Examples

``` r
# using grouping variable
if (require("descriptr")) {
  library(descriptr)
  ols_test_bartlett(mtcarz, 'mpg', group_var = 'cyl')
}
#> Loading required package: descriptr
#> Warning: package 'descriptr' was built under R version 4.5.3
#> 
#> Attaching package: 'descriptr'
#> The following object is masked from 'package:olsrr':
#> 
#>     hsb
#> 
#>     Bartlett's Test of Homogenity of Variances    
#> ------------------------------------------------
#> Ho: Variances are equal across groups
#> Ha: Variances are unequal for atleast two groups
#> 
#>         Test Summary          
#>  -----------------------------
#>  DF            =    2 
#>  Chi2          =    8.39345 
#>  Prob > Chi2   =    0.01504477 

# using variables
ols_test_bartlett(hsb, 'read', 'write')
#> 
#>     Bartlett's Test of Homogenity of Variances    
#> ------------------------------------------------
#> Ho: Variances are equal across groups
#> Ha: Variances are unequal for atleast two groups
#> 
#>         Data          
#>  ---------------------
#>  Variables: read write 
#> 
#>         Test Summary         
#>  ----------------------------
#>  DF            =    1 
#>  Chi2          =    1.222871 
#>  Prob > Chi2   =    0.2687979 
```
