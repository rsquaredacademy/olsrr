# `Stepwise R-Squared backward regression`

# `Stepwise R-Squared backward regression`

Source:
[`R/ols-stepaic-backward-regression.R`](https://github.com/rsquaredacademy/olsrr/blob/HEAD/R/ols-stepaic-backward-regression.R)

`ols_step_backward_r2.Rd`

Build regression model from a set of candidate predictor variables by
removing predictors based on r-squared, in a stepwise manner until there
is no variable left to remove any more.

## Usage

``` r
ols_step_backward_r2(model, ...)

# Default S3 method
ols_step_backward_r2(
  model,
  include = NULL,
  exclude = NULL,
  progress = FALSE,
  details = FALSE,
  steps = NULL,
  ...
)

# S3 method for class 'ols_step_backward_r2'
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
[`ols_step_backward_adj_r2()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_adj_r2.md),
[`ols_step_backward_aic()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_aic.md),
[`ols_step_backward_p()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_p.md),
[`ols_step_backward_sbc()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_sbc.md),
[`ols_step_backward_sbic()`](https://olsrr.rsquaredacademy.com/reference/ols_step_backward_sbic.md)

## Examples

``` r
# stepwise backward regression
model <- lm(y ~ ., data = surgical)
ols_step_backward_r2(model)
#> [1] "No variables have been removed from the model."

# final model and selection metrics
k <- ols_step_backward_aic(model)
k$metrics
#>   step variable        r2    adj_r2      aic      sbc     sbic
#> 1    1  alc_mod 0.7817703 0.7485615 734.4068 752.3077 584.2757
#> 2    2   gender 0.7814169 0.7535127 732.4942 748.4061 581.9383
#> 3    3      age 0.7809054 0.7580831 730.6204 744.5433 579.6377
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
#> })(formula = y ~ bcs + pindex + enzyme_test + liver_test + alc_heavy, 
#>     data = surgical)
#> 
#> Coefficients:
#> (Intercept)          bcs       pindex  enzyme_test   liver_test    alc_heavy  
#>   -1178.330       59.864        8.924        9.748       58.064      317.848  
#> 

# include or exclude variable
# force variables to be included in the selection process
ols_step_backward_r2(model, include = c("alc_mod", "gender"))
#> [1] "No variables have been removed from the model."

# use index of variable instead of name
ols_step_backward_r2(model, include = c(7, 6))
#> [1] "No variables have been removed from the model."

# force variable to be excluded from selection process
ols_step_backward_r2(model, exclude = c("alc_heavy", "bcs"))
#> [1] "No variables have been removed from the model."

# use index of variable instead of name
ols_step_backward_r2(model, exclude = c(8, 1))
#> [1] "No variables have been removed from the model."

# steps
ols_step_backward_r2(model, steps = 2)
#> [1] "No variables have been removed from the model."
```
