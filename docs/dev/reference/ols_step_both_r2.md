# `Stepwise R-Squared regression`

# `Stepwise R-Squared regression`

Source:
[`R/ols-stepaic-both-regression.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-stepaic-both-regression.R)

`ols_step_both_r2.Rd`

Build regression model from a set of candidate predictor variables by
entering and removing predictors based on r-squared, in a stepwise
manner until there is no variable left to enter or remove any more.

## Usage

``` r
ols_step_both_r2(model, ...)

# Default S3 method
ols_step_both_r2(
  model,
  include = NULL,
  exclude = NULL,
  progress = FALSE,
  details = FALSE,
  steps = NULL,
  ...
)

# S3 method for class 'ols_step_both_r2'
plot(x, print_plot = TRUE, details = TRUE, digits = 3, ...)
```

## Arguments

- model:

  An object of class `lm`.

- ...:

  Other arguments.

- include:

  Character or numeric vector; variables to be included in selection
  process.

- exclude:

  Character or numeric vector; variables to be excluded from selection
  process.

- progress:

  Logical; if `TRUE`, will display variable selection progress.

- details:

  Logical; if `TRUE`, details of variable selection will be printed on
  screen.

- steps:

  Number of steps after which the stepwise procedures should stop.

- x:

  An object of class `ols_step_both_*`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

- digits:

  Number of decimal places to display.

## Value

List containing the following components:

- model:

  final model; an object of class `lm`

- metrics:

  selection metrics

- others:

  list; info used for plotting and printing

## Details

Refits accept explicit contrast matrices or the base contrast names
`contr.treatment`, `contr.sum`, `contr.helmert`, `contr.poly` and
`contr.SAS` (resolved from the stats namespace); models fitted with
custom named contrast functions must be refitted with an explicit
matrix, for example `contrasts = list(g = my_matrix)`. Refits carry the
fitted model's frozen total offset as an argument with all formula
[`offset()`](https://rdrr.io/r/stats/offset.html) terms removed for
standardized-beta and selection computations. Returned stepwise models
are fitted from the original call, preserving formula and argument
offsets for prediction on new data. Their coefficients, fitted values
and total offset must be identical to the frozen-offset selection fit;
otherwise selection stops with an error because prediction with new data
is not supported for that model.

For non-hierarchical stepwise selection, candidate rankings and
criterion improvement tests use 10 significant digits: values that agree
to 10 significant digits are ties, equal criterion values mean no
improvement, and a remaining tie goes to the first candidate in formula
order after any F tie is broken by p value (smaller for entry, larger
for removal). P-value entry and removal cutoffs are compared without
rounding.

## References

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with
S. Fourth edition. Springer.

## See also

Other both direction selection procedures:
[`ols_step_both_adj_r2()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_step_both_adj_r2.md),
[`ols_step_both_aic()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_step_both_aic.md),
[`ols_step_both_sbc()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_step_both_sbc.md),
[`ols_step_both_sbic()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_step_both_sbic.md)

## Examples

``` r
# \donttest{
# stepwise regression
model <- lm(y ~ ., data = stepdata)
ols_step_both_r2(model)
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    56988.482    57004.289       228.318    0.00000    0.00000 
#>  1      x6 (+)        33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  2      x1 (+)        32931.758    32963.372    -23826.833    0.69972    0.69969 
#>  3      x3 (+)        31912.722    31952.239    -24845.827    0.71466    0.71462 
#>  4      x2 (+)        29304.296    29351.717    -27453.243    0.74958    0.74953 
#>  5      x4 (+)        29302.797    29358.121    -27454.740    0.74962    0.74956 
#>  6      x5 (+)        29304.734    29367.962    -27452.802    0.74962    0.74955 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.866       RMSE                   0.503 
#> R-Squared               0.750       MSE                    0.253 
#> Adj. R-Squared          0.750       Coef. Var           6431.168 
#> Pred R-Squared          0.749       AIC                29304.734 
#> MAE                     0.402       SBC                29367.962 
#> -----------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                    ANOVA                                     
#> ----------------------------------------------------------------------------
#>                  Sum of                                                     
#>                 Squares           DF    Mean Square       F            Sig. 
#> ----------------------------------------------------------------------------
#> Regression    15163.548            6       2527.258    9976.429    < 0.0001 
#> Residual       5064.685        19993          0.253                         
#> Total         20228.233        19999                                        
#> ----------------------------------------------------------------------------
#> 
#>                                    Parameter Estimates                                     
#> ------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower    upper 
#> ------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.496      0.1347    -0.012    0.002 
#>          x6     0.000         0.004       -0.002    -0.129      0.8974    -0.007    0.007 
#>          x1     0.256         0.005        0.363    54.591    < 0.0001     0.247    0.265 
#>          x3     0.253         0.005        0.356    53.997    < 0.0001     0.244    0.262 
#>          x2     0.249         0.005        0.347    52.766    < 0.0001     0.240    0.258 
#>          x4    -0.007         0.004       -0.007    -1.870      0.0615    -0.014    0.000 
#>          x5    -0.001         0.004       -0.001    -0.250      0.8023    -0.008    0.006 
#> ------------------------------------------------------------------------------------------
#> 

# stepwise regression plot
model <- lm(y ~ ., data = stepdata)
k <- ols_step_both_r2(model)
plot(k)


# selection metrics
k$metrics
#>   step variable   method        r2    adj_r2      aic      sbc      sbic
#> 1    1       x6 addition 0.6914463 0.6914308 33473.30 33497.01 -23285.07
#> 2    2       x1 addition 0.6997189 0.6996888 32931.76 32963.37 -23826.83
#> 3    3       x3 addition 0.7146640 0.7146212 31912.72 31952.24 -24845.83
#> 4    4       x2 addition 0.7495784 0.7495283 29304.30 29351.72 -27453.24
#> 5    5       x4 addition 0.7496222 0.7495596 29302.80 29358.12 -27454.74
#> 6    6       x5 addition 0.7496230 0.7495478 29304.73 29367.96 -27452.80

# final model
k$model
#> 
#> Call:
#> (function (formula, data, subset, weights, na.action, method = "qr", 
#>     model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
#>     contrasts = NULL, offset, ...) 
#> {
#>     ret.x <- x
#>     ret.y <- y
#>     cl <- match.call()
#>     mf <- match.call(expand.dots = FALSE)
#>     m <- match(c("formula", "data", "subset", "weights", "na.action", 
#>         "offset"), names(mf), 0L)
#>     mf <- mf[c(1L, m)]
#>     mf$drop.unused.levels <- TRUE
#>     mf[[1L]] <- quote(stats::model.frame)
#>     mf <- eval(mf, parent.frame())
#>     if (method == "model.frame") 
#>         return(mf)
#>     else if (method != "qr") 
#>         warning(gettextf("method = '%s' is not supported. Using 'qr'", 
#>             method), domain = NA)
#>     mt <- attr(mf, "terms")
#>     y <- model.response(mf, "numeric")
#>     w <- as.vector(model.weights(mf))
#>     if (!is.null(w) && !is.numeric(w)) 
#>         stop("'weights' must be a numeric vector")
#>     offset <- model.offset(mf)
#>     mlm <- is.matrix(y)
#>     ny <- if (mlm) 
#>         nrow(y)
#>     else length(y)
#>     if (!is.null(offset)) {
#>         if (!mlm) 
#>             offset <- as.vector(offset)
#>         if (NROW(offset) != ny) 
#>             stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
#>                 NROW(offset), ny), domain = NA)
#>     }
#>     if (is.empty.model(mt)) {
#>         x <- NULL
#>         z <- list(coefficients = if (mlm) matrix(NA_real_, 0, 
#>             ncol(y)) else numeric(), residuals = y, fitted.values = 0 * 
#>             y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
#>             0) else ny)
#>         if (!is.null(offset)) {
#>             z$fitted.values <- offset
#>             z$residuals <- y - offset
#>         }
#>     }
#>     else {
#>         x <- model.matrix(mt, mf, contrasts)
#>         z <- if (is.null(w)) 
#>             lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
#>                 ...)
#>         else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
#>             ...)
#>     }
#>     class(z) <- c(if (mlm) "mlm", "lm")
#>     z$na.action <- attr(mf, "na.action")
#>     z$offset <- offset
#>     z$contrasts <- attr(x, "contrasts")
#>     z$xlevels <- .getXlevels(mt, mf)
#>     z$call <- cl
#>     z$terms <- mt
#>     if (model) 
#>         z$model <- mf
#>     if (ret.x) 
#>         z$x <- x
#>     if (ret.y) 
#>         z$y <- y
#>     if (!qr) 
#>         z$qr <- NULL
#>     z
#> })(formula = y ~ x6 + x1 + x3 + x2 + x4 + x5, data = stepdata)
#> 
#> Coefficients:
#> (Intercept)           x6           x1           x3           x2           x4  
#>  -0.0053247   -0.0004597    0.2558123    0.2530031    0.2492181   -0.0066105  
#>          x5  
#>  -0.0008877  
#> 

# include or exclude variables
# force variable to be included in selection process
model <- lm(y ~ ., data = stepdata)

ols_step_both_r2(model, include = c("x6"))
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  1      x1 (+)        32931.758    32963.372    -23826.833    0.69972    0.69969 
#>  2      x3 (+)        31912.722    31952.239    -24845.827    0.71466    0.71462 
#>  3      x2 (+)        29304.296    29351.717    -27453.243    0.74958    0.74953 
#>  4      x4 (+)        29302.797    29358.121    -27454.740    0.74962    0.74956 
#>  5      x5 (+)        29304.734    29367.962    -27452.802    0.74962    0.74955 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.866       RMSE                   0.503 
#> R-Squared               0.750       MSE                    0.253 
#> Adj. R-Squared          0.750       Coef. Var           6431.168 
#> Pred R-Squared          0.749       AIC                29304.734 
#> MAE                     0.402       SBC                29367.962 
#> -----------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                    ANOVA                                     
#> ----------------------------------------------------------------------------
#>                  Sum of                                                     
#>                 Squares           DF    Mean Square       F            Sig. 
#> ----------------------------------------------------------------------------
#> Regression    15163.548            6       2527.258    9976.429    < 0.0001 
#> Residual       5064.685        19993          0.253                         
#> Total         20228.233        19999                                        
#> ----------------------------------------------------------------------------
#> 
#>                                    Parameter Estimates                                     
#> ------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower    upper 
#> ------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.496      0.1347    -0.012    0.002 
#>          x6     0.000         0.004       -0.002    -0.129      0.8974    -0.007    0.007 
#>          x1     0.256         0.005        0.363    54.591    < 0.0001     0.247    0.265 
#>          x3     0.253         0.005        0.356    53.997    < 0.0001     0.244    0.262 
#>          x2     0.249         0.005        0.347    52.766    < 0.0001     0.240    0.258 
#>          x4    -0.007         0.004       -0.007    -1.870      0.0615    -0.014    0.000 
#>          x5    -0.001         0.004       -0.001    -0.250      0.8023    -0.008    0.006 
#> ------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_both_r2(model, include = c(6))
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  1      x1 (+)        32931.758    32963.372    -23826.833    0.69972    0.69969 
#>  2      x3 (+)        31912.722    31952.239    -24845.827    0.71466    0.71462 
#>  3      x2 (+)        29304.296    29351.717    -27453.243    0.74958    0.74953 
#>  4      x4 (+)        29302.797    29358.121    -27454.740    0.74962    0.74956 
#>  5      x5 (+)        29304.734    29367.962    -27452.802    0.74962    0.74955 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.866       RMSE                   0.503 
#> R-Squared               0.750       MSE                    0.253 
#> Adj. R-Squared          0.750       Coef. Var           6431.168 
#> Pred R-Squared          0.749       AIC                29304.734 
#> MAE                     0.402       SBC                29367.962 
#> -----------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                    ANOVA                                     
#> ----------------------------------------------------------------------------
#>                  Sum of                                                     
#>                 Squares           DF    Mean Square       F            Sig. 
#> ----------------------------------------------------------------------------
#> Regression    15163.548            6       2527.258    9976.429    < 0.0001 
#> Residual       5064.685        19993          0.253                         
#> Total         20228.233        19999                                        
#> ----------------------------------------------------------------------------
#> 
#>                                    Parameter Estimates                                     
#> ------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower    upper 
#> ------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.496      0.1347    -0.012    0.002 
#>          x6     0.000         0.004       -0.002    -0.129      0.8974    -0.007    0.007 
#>          x1     0.256         0.005        0.363    54.591    < 0.0001     0.247    0.265 
#>          x3     0.253         0.005        0.356    53.997    < 0.0001     0.244    0.262 
#>          x2     0.249         0.005        0.347    52.766    < 0.0001     0.240    0.258 
#>          x4    -0.007         0.004       -0.007    -1.870      0.0615    -0.014    0.000 
#>          x5    -0.001         0.004       -0.001    -0.250      0.8023    -0.008    0.006 
#> ------------------------------------------------------------------------------------------
#> 

# force variable to be excluded from selection process
ols_step_both_r2(model, exclude = c("x2"))
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    56988.482    57004.289       228.318    0.00000    0.00000 
#>  1      x6 (+)        33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  2      x1 (+)        32931.758    32963.372    -23826.833    0.69972    0.69969 
#>  3      x3 (+)        31912.722    31952.239    -24845.827    0.71466    0.71462 
#>  4      x4 (+)        31908.533    31955.954    -24850.258    0.71475    0.71470 
#>  5      x5 (+)        31910.339    31965.663    -24848.695    0.71476    0.71468 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.845       RMSE                   0.537 
#> R-Squared               0.715       MSE                    0.289 
#> Adj. R-Squared          0.715       Coef. Var           6864.212 
#> Pred R-Squared          0.715       AIC                31910.339 
#> MAE                     0.428       SBC                31965.663 
#> -----------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                     ANOVA                                     
#> -----------------------------------------------------------------------------
#>                  Sum of                                                      
#>                 Squares           DF    Mean Square        F            Sig. 
#> -----------------------------------------------------------------------------
#> Regression    14458.233            5       2891.647    10020.031    < 0.0001 
#> Residual       5770.000        19994          0.289                          
#> Total         20228.233        19999                                         
#> -----------------------------------------------------------------------------
#> 
#>                                     Parameter Estimates                                     
#> -------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower     upper 
#> -------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.214      0.2246    -0.012     0.003 
#>          x6     0.142         0.002        0.507    57.004    < 0.0001     0.137     0.147 
#>          x1     0.148         0.004        0.210    32.842    < 0.0001     0.139     0.157 
#>          x3     0.146         0.005        0.206    32.371    < 0.0001     0.137     0.155 
#>          x4    -0.009         0.004       -0.009    -2.487      0.0129    -0.017    -0.002 
#>          x5    -0.002         0.004       -0.002    -0.440      0.6597    -0.009     0.006 
#> -------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_both_r2(model, exclude = c(2))
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    56988.482    57004.289       228.318    0.00000    0.00000 
#>  1      x6 (+)        33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  2      x1 (+)        32931.758    32963.372    -23826.833    0.69972    0.69969 
#>  3      x3 (+)        31912.722    31952.239    -24845.827    0.71466    0.71462 
#>  4      x4 (+)        31908.533    31955.954    -24850.258    0.71475    0.71470 
#>  5      x5 (+)        31910.339    31965.663    -24848.695    0.71476    0.71468 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.845       RMSE                   0.537 
#> R-Squared               0.715       MSE                    0.289 
#> Adj. R-Squared          0.715       Coef. Var           6864.212 
#> Pred R-Squared          0.715       AIC                31910.339 
#> MAE                     0.428       SBC                31965.663 
#> -----------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                     ANOVA                                     
#> -----------------------------------------------------------------------------
#>                  Sum of                                                      
#>                 Squares           DF    Mean Square        F            Sig. 
#> -----------------------------------------------------------------------------
#> Regression    14458.233            5       2891.647    10020.031    < 0.0001 
#> Residual       5770.000        19994          0.289                          
#> Total         20228.233        19999                                         
#> -----------------------------------------------------------------------------
#> 
#>                                     Parameter Estimates                                     
#> -------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower     upper 
#> -------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.214      0.2246    -0.012     0.003 
#>          x6     0.142         0.002        0.507    57.004    < 0.0001     0.137     0.147 
#>          x1     0.148         0.004        0.210    32.842    < 0.0001     0.139     0.157 
#>          x3     0.146         0.005        0.206    32.371    < 0.0001     0.137     0.155 
#>          x4    -0.009         0.004       -0.009    -2.487      0.0129    -0.017    -0.002 
#>          x5    -0.002         0.004       -0.002    -0.440      0.6597    -0.009     0.006 
#> -------------------------------------------------------------------------------------------
#> 

# include & exclude variables in the selection process
ols_step_both_r2(model, include = c("x6"), exclude = c("x2"))
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  1      x1 (+)        32931.758    32963.372    -23826.833    0.69972    0.69969 
#>  2      x3 (+)        31912.722    31952.239    -24845.827    0.71466    0.71462 
#>  3      x4 (+)        31908.533    31955.954    -24850.258    0.71475    0.71470 
#>  4      x5 (+)        31910.339    31965.663    -24848.695    0.71476    0.71468 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.845       RMSE                   0.537 
#> R-Squared               0.715       MSE                    0.289 
#> Adj. R-Squared          0.715       Coef. Var           6864.212 
#> Pred R-Squared          0.715       AIC                31910.339 
#> MAE                     0.428       SBC                31965.663 
#> -----------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                     ANOVA                                     
#> -----------------------------------------------------------------------------
#>                  Sum of                                                      
#>                 Squares           DF    Mean Square        F            Sig. 
#> -----------------------------------------------------------------------------
#> Regression    14458.233            5       2891.647    10020.031    < 0.0001 
#> Residual       5770.000        19994          0.289                          
#> Total         20228.233        19999                                         
#> -----------------------------------------------------------------------------
#> 
#>                                     Parameter Estimates                                     
#> -------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower     upper 
#> -------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.214      0.2246    -0.012     0.003 
#>          x6     0.142         0.002        0.507    57.004    < 0.0001     0.137     0.147 
#>          x1     0.148         0.004        0.210    32.842    < 0.0001     0.139     0.157 
#>          x3     0.146         0.005        0.206    32.371    < 0.0001     0.137     0.155 
#>          x4    -0.009         0.004       -0.009    -2.487      0.0129    -0.017    -0.002 
#>          x5    -0.002         0.004       -0.002    -0.440      0.6597    -0.009     0.006 
#> -------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_both_r2(model, include = c(6), exclude = c(2))
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  1      x1 (+)        32931.758    32963.372    -23826.833    0.69972    0.69969 
#>  2      x3 (+)        31912.722    31952.239    -24845.827    0.71466    0.71462 
#>  3      x4 (+)        31908.533    31955.954    -24850.258    0.71475    0.71470 
#>  4      x5 (+)        31910.339    31965.663    -24848.695    0.71476    0.71468 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.845       RMSE                   0.537 
#> R-Squared               0.715       MSE                    0.289 
#> Adj. R-Squared          0.715       Coef. Var           6864.212 
#> Pred R-Squared          0.715       AIC                31910.339 
#> MAE                     0.428       SBC                31965.663 
#> -----------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                     ANOVA                                     
#> -----------------------------------------------------------------------------
#>                  Sum of                                                      
#>                 Squares           DF    Mean Square        F            Sig. 
#> -----------------------------------------------------------------------------
#> Regression    14458.233            5       2891.647    10020.031    < 0.0001 
#> Residual       5770.000        19994          0.289                          
#> Total         20228.233        19999                                         
#> -----------------------------------------------------------------------------
#> 
#>                                     Parameter Estimates                                     
#> -------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower     upper 
#> -------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.214      0.2246    -0.012     0.003 
#>          x6     0.142         0.002        0.507    57.004    < 0.0001     0.137     0.147 
#>          x1     0.148         0.004        0.210    32.842    < 0.0001     0.139     0.157 
#>          x3     0.146         0.005        0.206    32.371    < 0.0001     0.137     0.155 
#>          x4    -0.009         0.004       -0.009    -2.487      0.0129    -0.017    -0.002 
#>          x5    -0.002         0.004       -0.002    -0.440      0.6597    -0.009     0.006 
#> -------------------------------------------------------------------------------------------
#> 

# steps
ols_step_both_r2(model, steps = 3)
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    56988.482    57004.289       228.318    0.00000    0.00000 
#>  1      x6 (+)        33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  2      x1 (+)        32931.758    32963.372    -23826.833    0.69972    0.69969 
#>  3      x3 (+)        31912.722    31952.239    -24845.827    0.71466    0.71462 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.845       RMSE                   0.537 
#> R-Squared               0.715       MSE                    0.289 
#> Adj. R-Squared          0.715       Coef. Var           6864.964 
#> Pred R-Squared          0.715       AIC                31912.722 
#> MAE                     0.428       SBC                31952.239 
#> -----------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                     ANOVA                                     
#> -----------------------------------------------------------------------------
#>                  Sum of                                                      
#>                 Squares           DF    Mean Square        F            Sig. 
#> -----------------------------------------------------------------------------
#> Regression    14456.391            3       4818.797    16694.265    < 0.0001 
#> Residual       5771.842        19996          0.289                          
#> Total         20228.233        19999                                         
#> -----------------------------------------------------------------------------
#> 
#>                                    Parameter Estimates                                     
#> ------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower    upper 
#> ------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.224      0.2209    -0.012    0.003 
#>          x6     0.142         0.002        0.507    57.014    < 0.0001     0.137    0.147 
#>          x1     0.148         0.004        0.210    32.815    < 0.0001     0.139    0.156 
#>          x3     0.146         0.005        0.205    32.363    < 0.0001     0.137    0.155 
#> ------------------------------------------------------------------------------------------
#> 
# }
```
