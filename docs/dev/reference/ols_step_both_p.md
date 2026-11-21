# `Stepwise regression`

# `Stepwise regression`

Source:
[`R/ols-stepwise-regression.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-stepwise-regression.R)

`ols_step_both_p.Rd`

Build regression model from a set of candidate predictor variables by
entering and removing predictors based on p values, in a stepwise manner
until there is no variable left to enter or remove any more.

## Usage

``` r
ols_step_both_p(model, ...)

# Default S3 method
ols_step_both_p(
  model,
  p_enter = 0.1,
  p_remove = 0.3,
  include = NULL,
  exclude = NULL,
  progress = FALSE,
  details = FALSE,
  steps = NULL,
  ...
)

# S3 method for class 'ols_step_both_p'
plot(x, model = NA, print_plot = TRUE, details = TRUE, ...)
```

## Arguments

- model:

  An object of class `lm`; the model should include all candidate
  predictor variables.

- ...:

  Other arguments.

- p_enter:

  p value; variables with p value less than `p_enter` will enter into
  the model.

- p_remove:

  p value; variables with p more than `p_remove` will be removed from
  the model.

- include:

  Character or numeric vector; variables to be included in selection
  process.

- exclude:

  Character or numeric vector; variables to be excluded from selection
  process.

- progress:

  Logical; if `TRUE`, will display variable selection progress.

- details:

  Logical; if `TRUE`, will print the regression result at each step.

- steps:

  Number of steps after which the stepwise procedures should stop.

- x:

  An object of class `ols_step_both_p`.

- print_plot:

  logical; if `TRUE`, prints the plot else returns a plot object.

## Value

`ols_step_both_p` returns an object of class `"ols_step_both_p"`. An
object of class `"ols_step_both_p"` is a list containing the following
components:

- model:

  final model; an object of class `lm`

- metrics:

  selection metrics

- beta_pval:

  beta and p values of models in each selection step

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

Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th
ed. N.p.: John Wiley & Sons, 2012. Print.

## Examples

``` r
# \donttest{
# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model)
#> 
#> 
#>                                 Stepwise Summary                                
#> ------------------------------------------------------------------------------
#> Step    Variable             AIC        SBC       SBIC        R2       Adj. R2 
#> ------------------------------------------------------------------------------
#>  0      Base Model         802.606    806.584    646.794    0.00000    0.00000 
#>  1      liver_test (+)     771.875    777.842    616.009    0.45454    0.44405 
#>  2      alc_heavy (+)      761.439    769.395    605.506    0.56674    0.54975 
#>  3      enzyme_test (+)    750.509    760.454    595.297    0.65900    0.63854 
#>  4      pindex (+)         735.715    747.649    582.943    0.75015    0.72975 
#>  5      bcs (+)            730.620    744.543    579.638    0.78091    0.75808 
#> ------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.884       RMSE                 195.454 
#> R-Squared                 0.781       MSE                38202.426 
#> Adj. R-Squared            0.758       Coef. Var             27.839 
#> Pred R-Squared            0.700       AIC                  730.620 
#> MAE                     137.656       SBC                  744.543 
#> -------------------------------------------------------------------
#>  RMSE: Root Mean Square Error 
#>  MSE: Mean Square Error 
#>  MAE: Mean Absolute Error 
#>  AIC: Akaike Information Criteria 
#>  SBC: Schwarz Bayesian Criteria 
#> 
#>                                   ANOVA                                   
#> -------------------------------------------------------------------------
#>                    Sum of                                                
#>                   Squares        DF    Mean Square      F           Sig. 
#> -------------------------------------------------------------------------
#> Regression    6535804.090         5    1307160.818    34.217    < 0.0001 
#> Residual      1833716.447        48      38202.426                       
#> Total         8369520.537        53                                      
#> -------------------------------------------------------------------------
#> 
#>                                         Parameter Estimates                                         
#> ---------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t         Sig           lower       upper 
#> ---------------------------------------------------------------------------------------------------
#> (Intercept)    -1178.330       208.682                 -5.647    < 0.0001    -1597.914    -758.746 
#>  liver_test       58.064        40.144        0.156     1.446      0.1546      -22.652     138.779 
#>   alc_heavy      317.848        71.634        0.314     4.437    < 0.0001      173.818     461.878 
#> enzyme_test        9.748         1.656        0.521     5.887    < 0.0001        6.419      13.077 
#>      pindex        8.924         1.808        0.380     4.935    < 0.0001        5.288      12.559 
#>         bcs       59.864        23.060        0.241     2.596      0.0125       13.498     106.230 
#> ---------------------------------------------------------------------------------------------------
#> 

# stepwise regression plot
model <- lm(y ~ ., data = surgical)
k <- ols_step_both_p(model)
plot(k)


# selection metrics
k$metrics
#>   step    variable   method        r2    adj_r2      aic      sbc     sbic
#> 1    1  liver_test addition 0.4545389 0.4440492 771.8753 777.8423 616.0089
#> 2    2   alc_heavy addition 0.5667409 0.5497504 761.4394 769.3953 605.5062
#> 3    3 enzyme_test addition 0.6590000 0.6385400 750.5089 760.4538 595.2974
#> 4    4      pindex addition 0.7501457 0.7297495 735.7146 747.6485 582.9426
#> 5    5         bcs addition 0.7809054 0.7580831 730.6204 744.5433 579.6377
#>   mallows_cp     rmse
#> 1  62.511923 296.2992
#> 2  41.368078 266.6484
#> 3  24.337853 238.9145
#> 4   7.537284 206.5835
#> 5   3.192498 195.4544

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
#> })(formula = y ~ liver_test + alc_heavy + enzyme_test + pindex + 
#>     bcs, data = surgical)
#> 
#> Coefficients:
#> (Intercept)   liver_test    alc_heavy  enzyme_test       pindex          bcs  
#>   -1178.330       58.064      317.848        9.748        8.924       59.864  
#> 

# include or exclude variables
model <- lm(y ~ ., data = stepdata)

# force variable to be included in selection process
ols_step_both_p(model, include = c("x6"))
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
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.866       RMSE                   0.503 
#> R-Squared               0.750       MSE                    0.253 
#> Adj. R-Squared          0.750       Coef. Var           6431.017 
#> Pred R-Squared          0.749       AIC                29302.797 
#> MAE                     0.402       SBC                29358.121 
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
#> Regression    15163.532            5       3032.706    11972.264    < 0.0001 
#> Residual       5064.701        19994          0.253                          
#> Total         20228.233        19999                                         
#> -----------------------------------------------------------------------------
#> 
#>                                    Parameter Estimates                                     
#> ------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower    upper 
#> ------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.497      0.1345    -0.012    0.002 
#>          x6     0.000         0.004       -0.002    -0.129      0.8974    -0.007    0.007 
#>          x1     0.256         0.005        0.363    54.592    < 0.0001     0.247    0.265 
#>          x3     0.253         0.005        0.356    53.998    < 0.0001     0.244    0.262 
#>          x2     0.249         0.005        0.347    52.769    < 0.0001     0.240    0.258 
#>          x4    -0.007         0.004       -0.007    -1.870      0.0614    -0.014    0.000 
#> ------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_both_p(model, include = c(6))
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
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.866       RMSE                   0.503 
#> R-Squared               0.750       MSE                    0.253 
#> Adj. R-Squared          0.750       Coef. Var           6431.017 
#> Pred R-Squared          0.749       AIC                29302.797 
#> MAE                     0.402       SBC                29358.121 
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
#> Regression    15163.532            5       3032.706    11972.264    < 0.0001 
#> Residual       5064.701        19994          0.253                          
#> Total         20228.233        19999                                         
#> -----------------------------------------------------------------------------
#> 
#>                                    Parameter Estimates                                     
#> ------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower    upper 
#> ------------------------------------------------------------------------------------------
#> (Intercept)    -0.005         0.004                 -1.497      0.1345    -0.012    0.002 
#>          x6     0.000         0.004       -0.002    -0.129      0.8974    -0.007    0.007 
#>          x1     0.256         0.005        0.363    54.592    < 0.0001     0.247    0.265 
#>          x3     0.253         0.005        0.356    53.998    < 0.0001     0.244    0.262 
#>          x2     0.249         0.005        0.347    52.769    < 0.0001     0.240    0.258 
#>          x4    -0.007         0.004       -0.007    -1.870      0.0614    -0.014    0.000 
#> ------------------------------------------------------------------------------------------
#> 

# force variable to be excluded from selection process
ols_step_both_p(model, exclude = c("x1"))
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    56988.482    57004.289       228.318    0.00000    0.00000 
#>  1      x6 (+)        33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  2      x3 (+)        32959.771    32991.385    -23798.829    0.69930    0.69927 
#>  3      x2 (+)        32080.424    32119.941    -24678.188    0.71226    0.71222 
#>  4      x4 (+)        32079.710    32127.131    -24679.159    0.71230    0.71224 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.844       RMSE                   0.539 
#> R-Squared               0.712       MSE                    0.291 
#> Adj. R-Squared          0.712       Coef. Var           6893.511 
#> Pred R-Squared          0.712       AIC                32079.710 
#> MAE                     0.430       SBC                32127.131 
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
#> Regression    14408.579            4       3602.145    12376.147    < 0.0001 
#> Residual       5819.654        19995          0.291                          
#> Total         20228.233        19999                                         
#> -----------------------------------------------------------------------------
#> 
#>                                    Parameter Estimates                                     
#> ------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower    upper 
#> ------------------------------------------------------------------------------------------
#> (Intercept)    -0.004         0.004                 -1.006      0.3146    -0.011    0.004 
#>          x6     0.148         0.002        0.528    59.728    < 0.0001     0.143    0.153 
#>          x3     0.141         0.005        0.199    31.259    < 0.0001     0.132    0.150 
#>          x2     0.137         0.005        0.190    29.983    < 0.0001     0.128    0.145 
#>          x4    -0.006         0.004       -0.006    -1.647      0.0996    -0.014    0.001 
#> ------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_both_p(model, exclude = c(1))
#> 
#> 
#>                                  Stepwise Summary                                 
#> --------------------------------------------------------------------------------
#> Step    Variable         AIC          SBC          SBIC         R2       Adj. R2 
#> --------------------------------------------------------------------------------
#>  0      Base Model    56988.482    57004.289       228.318    0.00000    0.00000 
#>  1      x6 (+)        33473.297    33497.007    -23285.069    0.69145    0.69143 
#>  2      x3 (+)        32959.771    32991.385    -23798.829    0.69930    0.69927 
#>  3      x2 (+)        32080.424    32119.941    -24678.188    0.71226    0.71222 
#>  4      x4 (+)        32079.710    32127.131    -24679.159    0.71230    0.71224 
#> --------------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                           Model Summary                           
#> -----------------------------------------------------------------
#> R                       0.844       RMSE                   0.539 
#> R-Squared               0.712       MSE                    0.291 
#> Adj. R-Squared          0.712       Coef. Var           6893.511 
#> Pred R-Squared          0.712       AIC                32079.710 
#> MAE                     0.430       SBC                32127.131 
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
#> Regression    14408.579            4       3602.145    12376.147    < 0.0001 
#> Residual       5819.654        19995          0.291                          
#> Total         20228.233        19999                                         
#> -----------------------------------------------------------------------------
#> 
#>                                    Parameter Estimates                                     
#> ------------------------------------------------------------------------------------------
#>       model      Beta    Std. Error    Std. Beta      t         Sig        lower    upper 
#> ------------------------------------------------------------------------------------------
#> (Intercept)    -0.004         0.004                 -1.006      0.3146    -0.011    0.004 
#>          x6     0.148         0.002        0.528    59.728    < 0.0001     0.143    0.153 
#>          x3     0.141         0.005        0.199    31.259    < 0.0001     0.132    0.150 
#>          x2     0.137         0.005        0.190    29.983    < 0.0001     0.128    0.145 
#>          x4    -0.006         0.004       -0.006    -1.647      0.0996    -0.014    0.001 
#> ------------------------------------------------------------------------------------------
#> 

# steps
ols_step_both_p(model, steps = 3)
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
