# Stepwise regression

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

## References

Chatterjee, Samprit and Hadi, Ali. Regression Analysis by Example. 5th
ed. N.p.: John Wiley & Sons, 2012. Print.

## Examples

``` r
if (FALSE) { # \dontrun{
# stepwise regression
model <- lm(y ~ ., data = surgical)
ols_step_both_p(model)

# stepwise regression plot
model <- lm(y ~ ., data = surgical)
k <- ols_step_both_p(model)
plot(k)

# selection metrics
k$metrics

# final model
k$model

# include or exclude variables
model <- lm(y ~ ., data = stepdata)

# force variable to be included in selection process
ols_step_both_p(model, include = c("x6"))

# use index of variable instead of name
ols_step_both_p(model, include = c(6))

# force variable to be excluded from selection process
ols_step_both_p(model, exclude = c("x1"))

# use index of variable instead of name
ols_step_both_p(model, exclude = c(1))

# steps
ols_step_both_p(model, steps = 3)

} # }
```
