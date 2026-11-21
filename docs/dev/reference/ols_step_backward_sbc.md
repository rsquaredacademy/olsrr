# `Stepwise SBC backward regression`

# `Stepwise SBC backward regression`

Source:
[`R/ols-stepaic-backward-regression.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-stepaic-backward-regression.R)

`ols_step_backward_sbc.Rd`

Build regression model from a set of candidate predictor variables by
removing predictors based on schwarz bayesian criterion, in a stepwise
manner until there is no variable left to remove any more.

## Usage

``` r
ols_step_backward_sbc(model, ...)

# Default S3 method
ols_step_backward_sbc(
  model,
  include = NULL,
  exclude = NULL,
  progress = FALSE,
  details = FALSE,
  steps = NULL,
  ...
)

# S3 method for class 'ols_step_backward_sbc'
plot(x, print_plot = TRUE, details = TRUE, digits = 3, ...)
```

## Arguments

- model:

  An object of class `lm`; the model should include all candidate
  predictor variables.

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

  Logical; if `TRUE`, will print the regression result at each step.

- steps:

  Number of steps after which the stepwise procedures should stop.

- x:

  An object of class `ols_step_backward_*`.

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

Other backward selection procedures:
[`ols_step_backward_adj_r2()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_step_backward_adj_r2.md),
[`ols_step_backward_aic()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_step_backward_aic.md),
[`ols_step_backward_p()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_step_backward_p.md),
[`ols_step_backward_r2()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_step_backward_r2.md),
[`ols_step_backward_sbic()`](https://olsrr.rsquaredacademy.com/dev/reference/ols_step_backward_sbic.md)

## Examples

``` r
# stepwise backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_sbc(model)
#> 
#> 
#>                              Stepwise Summary                              
#> -------------------------------------------------------------------------
#> Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
#> -------------------------------------------------------------------------
#>  0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
#>  1      alc_mod       734.407    752.308    584.276    0.78177    0.74856 
#>  2      gender        732.494    748.406    581.938    0.78142    0.75351 
#>  3      age           730.620    744.543    579.638    0.78091    0.75808 
#>  4      liver_test    730.924    742.858    579.087    0.77136    0.75269 
#> -------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.878       RMSE                 197.620 
#> R-Squared                 0.771       MSE                39053.801 
#> Adj. R-Squared            0.753       Coef. Var             28.147 
#> Pred R-Squared            0.695       AIC                  730.924 
#> MAE                     140.619       SBC                  742.858 
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
#> Regression    6455884.265         4    1613971.066    41.327    < 0.0001 
#> Residual      1913636.272        49      39053.801                       
#> Total         8369520.537        53                                      
#> -------------------------------------------------------------------------
#> 
#>                                         Parameter Estimates                                         
#> ---------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t         Sig           lower       upper 
#> ---------------------------------------------------------------------------------------------------
#> (Intercept)    -1334.424       180.589                 -7.389    < 0.0001    -1697.332    -971.516 
#>         bcs       81.439        17.781        0.329     4.580    < 0.0001       45.706     117.171 
#>      pindex       10.131         1.622        0.431     6.246    < 0.0001        6.871      13.390 
#> enzyme_test       11.243         1.308        0.601     8.596    < 0.0001        8.614      13.871 
#>   alc_heavy      312.777        72.341        0.309     4.324    < 0.0001      167.402     458.152 
#> ---------------------------------------------------------------------------------------------------
#> 

# stepwise backward regression plot
model <- lm(y ~ ., data = surgical)
k <- ols_step_backward_sbc(model)
plot(k)


# selection metrics
k$metrics
#>   step   variable        r2    adj_r2      aic      sbc     sbic
#> 1    1    alc_mod 0.7817703 0.7485615 734.4068 752.3077 584.2757
#> 2    2     gender 0.7814169 0.7535127 732.4942 748.4061 581.9383
#> 3    3        age 0.7809054 0.7580831 730.6204 744.5433 579.6377
#> 4    4 liver_test 0.7713565 0.7526917 730.9241 742.8580 579.0865

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
#> })(formula = y ~ bcs + pindex + enzyme_test + alc_heavy, data = surgical)
#> 
#> Coefficients:
#> (Intercept)          bcs       pindex  enzyme_test    alc_heavy  
#>    -1334.42        81.44        10.13        11.24       312.78  
#> 

# include or exclude variable
# force variables to be included in the selection process
ols_step_backward_sbc(model, include = c("alc_mod", "gender"))
#> 
#> 
#>                              Stepwise Summary                              
#> -------------------------------------------------------------------------
#> Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
#> -------------------------------------------------------------------------
#>  0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
#>  1      age           734.544    752.445    584.367    0.78121    0.74792 
#>  2      liver_test    734.502    750.413    583.364    0.77314    0.74418 
#> -------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.879       RMSE                 200.993 
#> R-Squared                 0.773       MSE                40398.325 
#> Adj. R-Squared            0.744       Coef. Var             28.628 
#> Pred R-Squared            0.672       AIC                  734.502 
#> MAE                     138.127       SBC                  750.413 
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
#> Regression    6470799.239         6    1078466.540    26.696    < 0.0001 
#> Residual      1898721.298        47      40398.325                       
#> Total         8369520.537        53                                      
#> -------------------------------------------------------------------------
#> 
#>                                         Parameter Estimates                                         
#> ---------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t         Sig           lower       upper 
#> ---------------------------------------------------------------------------------------------------
#> (Intercept)    -1332.543       187.069                 -7.123    < 0.0001    -1708.878    -956.208 
#>         bcs       80.623        18.135        0.325     4.446    < 0.0001       44.140     117.105 
#>      pindex       10.000         1.668        0.425     5.995    < 0.0001        6.644      13.355 
#> enzyme_test       11.116         1.349        0.595     8.243    < 0.0001        8.403      13.829 
#>      gender       33.509        56.149        0.042     0.597      0.5535      -79.448     146.465 
#>     alc_mod        7.110        64.258        0.009     0.111      0.9124     -122.161     136.381 
#>   alc_heavy      321.131        84.770        0.317     3.788      0.0004      150.596     491.666 
#> ---------------------------------------------------------------------------------------------------
#> 

# use index of variable instead of name
ols_step_backward_sbc(model, include = c(7, 6))
#> 
#> 
#>                              Stepwise Summary                              
#> -------------------------------------------------------------------------
#> Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
#> -------------------------------------------------------------------------
#>  0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
#>  1      age           734.544    752.445    584.367    0.78121    0.74792 
#>  2      liver_test    734.502    750.413    583.364    0.77314    0.74418 
#> -------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.879       RMSE                 200.993 
#> R-Squared                 0.773       MSE                40398.325 
#> Adj. R-Squared            0.744       Coef. Var             28.628 
#> Pred R-Squared            0.672       AIC                  734.502 
#> MAE                     138.127       SBC                  750.413 
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
#> Regression    6470799.239         6    1078466.540    26.696    < 0.0001 
#> Residual      1898721.298        47      40398.325                       
#> Total         8369520.537        53                                      
#> -------------------------------------------------------------------------
#> 
#>                                         Parameter Estimates                                         
#> ---------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t         Sig           lower       upper 
#> ---------------------------------------------------------------------------------------------------
#> (Intercept)    -1332.543       187.069                 -7.123    < 0.0001    -1708.878    -956.208 
#>         bcs       80.623        18.135        0.325     4.446    < 0.0001       44.140     117.105 
#>      pindex       10.000         1.668        0.425     5.995    < 0.0001        6.644      13.355 
#> enzyme_test       11.116         1.349        0.595     8.243    < 0.0001        8.403      13.829 
#>      gender       33.509        56.149        0.042     0.597      0.5535      -79.448     146.465 
#>     alc_mod        7.110        64.258        0.009     0.111      0.9124     -122.161     136.381 
#>   alc_heavy      321.131        84.770        0.317     3.788      0.0004      150.596     491.666 
#> ---------------------------------------------------------------------------------------------------
#> 

# force variable to be excluded from selection process
ols_step_backward_sbc(model, exclude = c("alc_heavy", "bcs"))
#> [1] "No variables have been removed from the model."

# use index of variable instead of name
ols_step_backward_sbc(model, exclude = c(8, 1))
#> [1] "No variables have been removed from the model."

# steps
ols_step_backward_sbc(model, steps = 2)
#> 
#> 
#>                              Stepwise Summary                              
#> -------------------------------------------------------------------------
#> Step    Variable        AIC        SBC       SBIC        R2       Adj. R2 
#> -------------------------------------------------------------------------
#>  0      Full Model    736.390    756.280    586.665    0.78184    0.74305 
#>  1      alc_mod       734.407    752.308    584.276    0.78177    0.74856 
#>  2      gender        732.494    748.406    581.938    0.78142    0.75351 
#> -------------------------------------------------------------------------
#> 
#> Final Model Output 
#> ------------------
#> 
#>                            Model Summary                            
#> -------------------------------------------------------------------
#> R                         0.884       RMSE                 197.292 
#> R-Squared                 0.781       MSE                38924.162 
#> Adj. R-Squared            0.754       Coef. Var             28.101 
#> Pred R-Squared            0.692       AIC                  732.494 
#> MAE                     138.160       SBC                  748.406 
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
#> Regression    6540084.920         6    1090014.153    28.004    < 0.0001 
#> Residual      1829435.617        47      38924.162                       
#> Total         8369520.537        53                                      
#> -------------------------------------------------------------------------
#> 
#>                                         Parameter Estimates                                         
#> ---------------------------------------------------------------------------------------------------
#>       model         Beta    Std. Error    Std. Beta      t         Sig           lower       upper 
#> ---------------------------------------------------------------------------------------------------
#> (Intercept)    -1143.080       235.943                 -4.845    < 0.0001    -1617.737    -668.424 
#>         bcs       61.424        23.748        0.248     2.586      0.0129       13.649     109.199 
#>      pindex        8.974         1.832        0.382     4.900    < 0.0001        5.290      12.659 
#> enzyme_test        9.852         1.700        0.527     5.794    < 0.0001        6.431      13.273 
#>  liver_test       54.053        42.288        0.146     1.278      0.2074      -31.019     139.125 
#>         age       -0.850         2.563       -0.024    -0.332      0.7416       -6.007       4.307 
#>   alc_heavy      314.585        72.974        0.310     4.311    < 0.0001      167.781     461.390 
#> ---------------------------------------------------------------------------------------------------
#> 
```
