# Business data vignette report

## Changed examples

All new examples use annual wholesale customer spending. The two legacy redirect pages were updated only to match their destination titles.

| Vignette | Section | Old → new data | Function taught | New question |
|---|---|---|---|---|
| getting-started | Fit once and read the result | mtcars → wholesale_customers | `ols_regress` | How do grocery and detergent spending relate to milk spending? |
| getting-started | Inspect what remains unexplained | mtcars → wholesale_customers | `ols_plot_resid_fit` | Where does the milk-spending fit miss observations? |
| getting-started | Put the fit in perspective | mtcars → wholesale_customers | `ols_model_performance` | How well does the milk-spending model fit and predict left-out observations? |
| regression-output | Read the regression table | trees → wholesale_customers | `ols_regress` | How do product-spending slopes and their uncertainty compare? |
| regression-output | Connect the output to base R | trees → wholesale_customers | `summary.lm` | Do base R and olsrr report the same estimates? |
| regression-output | Understand the ANOVA decomposition | trees → wholesale_customers | `ols_anova` | How does milk-spending variation divide into fitted and residual components? |
| residual-diagnostics | Look for shape and changing spread | cars → wholesale_customers | `ols_plot_resid_fit` | Does milk-spending residual spread change with fitted values? |
| residual-diagnostics | Compare the residual distribution with normality | cars → wholesale_customers | `ols_plot_resid_qq` | Do milk-spending residuals follow a normal shape? |
| variance-normality-tests | Test a specified variance pattern | cars → wholesale_customers | `ols_test_breusch_pagan` | Does milk-spending error variance vary with fitted values? |
| variance-normality-tests | Ask a separate normality question | cars → wholesale_customers | `ols_test_normality` | Do milk-spending residuals depart from normality? |
| variance-normality-tests | Compare grouped variances | PlantGrowth → wholesale_customers | `ols_test_bartlett` | Does milk-spending variance differ by customer region? |
| influence_measures | Cook's D Bar Plot | mtcars → wholesale_customers | `ols_plot_cooksd_bar` | Which customers most influence fitted milk spending? |
| influence_measures | Cook's D Chart | mtcars → wholesale_customers | `ols_plot_cooksd_chart` | Which customers have large Cook distances? |
| influence_measures | DFBETAs Panel | mtcars → wholesale_customers | `ols_plot_dfbetas` | Which customers change individual spending slopes? |
| influence_measures | DFFITS Plot | mtcars → wholesale_customers | `ols_plot_dffits` | Which customers substantially change their fitted milk spending? |
| influence_measures | Studentized Residual Plot | mtcars → wholesale_customers | `ols_plot_resid_stud` | Which customers have unusually large studentized milk-spending residuals? |
| influence_measures | Standardized Residual Chart | mtcars → wholesale_customers | `ols_plot_resid_stand` | Which customers have unusually large standardized residuals? |
| influence_measures | Studentized Residuals vs Leverage Plot | hsb → wholesale_customers | `ols_plot_resid_lev` | Which customers combine leverage with unusual residuals? |
| influence_measures | Deleted Studentized Residual vs Fitted Values Plot | mtcars → wholesale_customers | `ols_plot_resid_stud_fit` | Which customers have unusual deleted residuals across fitted spending? |
| influence_measures | Hadi Plot | mtcars → wholesale_customers | `ols_plot_hadi` | Which customers are influential under Hadi’s measure? |
| influence_measures | Potential Residual Plot | mtcars → wholesale_customers | `ols_plot_resid_pot` | Which customers are outliers, high-leverage observations, or both? |
| regression_diagnostics | VIF | mtcars → wholesale_customers | `ols_vif_tol` | How correlated are the product-spending predictors? |
| regression_diagnostics | Condition Index | mtcars → wholesale_customers | `ols_eigen_cindex` | Do product-spending predictors show a large condition index? |
| regression_diagnostics | Collinearity Diagnostics | mtcars → wholesale_customers | `ols_coll_diag` | Which predictors contribute to spending-model collinearity? |
| regression_diagnostics | Residual Fit Spread Plot | mtcars → wholesale_customers | `ols_plot_resid_fit_spread` | How does fitted spending spread compare with residual spread? |
| regression_diagnostics | Partial | mtcars → wholesale_customers | `ols_correlations` | How much does each spending predictor contribute? |
| regression_diagnostics | Observed vs Predicted Plot | mtcars → wholesale_customers | `ols_plot_obs_fit` | How closely do fitted values track observed milk spending? |
| regression_diagnostics | Lack of Fit F Test | mtcars → wholesale_customers | `ols_pure_error_anova` | Is the straight-line mean adequate at repeated grocery-spending values? |
| regression_diagnostics | Diagnostics Panel | mtcars → wholesale_customers | `ols_plot_diagnostics` | What do the combined spending-model diagnostics show? |
| regression_diagnostics | Residual vs Regressor Plots | mtcars → wholesale_customers | `ols_plot_resid_regressor` | Does frozen spending explain residual milk spending? |
| regression_diagnostics | Added Variable Plot | mtcars → wholesale_customers | `ols_plot_added_variable` | What does each spending predictor add after the others? |
| regression_diagnostics | Residual Plus Component Plot | mtcars → wholesale_customers | `ols_plot_comp_plus_resid` | Is the relationship with each spending predictor curved? |
| variable_selection | All Possible Regression | mtcars → wholesale_customers | `ols_step_all_possible` | Which subsets of product categories explain milk spending? |
| variable_selection | Best Subset Regression | mtcars → wholesale_customers | `ols_step_best_subset` | Which category subsets optimize model-selection criteria? |
| variable_selection | Model | surgical → wholesale_customers | `lm`, `summary` | How do product-category expenditures relate to milk spending? |
| variable_selection | Forward selection | surgical → wholesale_customers | `ols_step_forward_p` | Which spending categories enter under forward selection? |
| variable_selection | Backward elimination | surgical → wholesale_customers | `ols_step_backward_p` | Which spending categories remain under backward elimination? |
| variable_selection | By name | surgical → wholesale_customers | `ols_step_forward_p` | Which spending categories enter under forward selection? |
| variable_selection | By index | surgical → wholesale_customers | `ols_step_forward_p` | Which spending categories enter under forward selection? |
| variable_selection | Standardized output | surgical → wholesale_customers | `ols_step_forward_adj_r2` | How does forward selection by adjusted R-squared describe milk spending? |
| variable_selection | Visualization | surgical → wholesale_customers | `ols_step_forward_adj_r2`, `plot` | How does forward selection by adjusted R-squared describe milk spending? |
| variable_selection | Verbose output | surgical → wholesale_customers | `ols_step_forward_adj_r2` | How does forward selection by adjusted R-squared describe milk spending? |
| variable_selection | Progress | surgical → wholesale_customers | `ols_step_forward_adj_r2` | How does forward selection by adjusted R-squared describe milk spending? |
| variable_selection | Hierarchical selection | surgical → wholesale_customers | `ols_step_forward_p` | Which spending categories enter under forward selection? |

## Dataset

`wholesale_customers`: 440 rows, eight columns, 5,132 bytes (`data/wholesale_customers.rda`, xz). Cardoso, M. (2013), [Wholesale customers, UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/292/wholesale+customers), [DOI 10.24432/C5030X](https://doi.org/10.24432/C5030X). The source page specifies Creative Commons Attribution 4.0 International (CC BY 4.0). Rebuilt with `data-raw/wholesale_customers.R`, using version 2 serialization to preserve the declared R compatibility. Annual product-category spend is relevant to business and supplies a multi-predictor model, grouped observations, repeated grocery values, variance differences and real high leverage. Currency is not specified by the source.

For the three-predictor influence model, observation 86 has leverage 0.1852901 versus the 2p/n reference of 0.01818182. The simple model has 10 duplicated grocery values for the pure-error example.

## Rendering and numeric evidence

Final changed vignettes were rendered to `%TEMP%/olsrr-bizdata-render`, not `docs/`. The per-vignette times in seconds: getting-started 1.45; intro 0.15; influence_measures 4.43; regression_diagnostics 6.51; regression-output 0.87; residual_diagnostics 0.14; residual-diagnostics 1.06; variable_selection 2.12; variance-normality-tests 0.67.

Rendered output: grocery coefficient 0.623 (95% interval 0.492–0.754), detergent coefficient -0.124 (interval -0.386–0.137), R-squared 0.531, predicted R-squared 0.490 and RMSE 5063.711; regression SS 12707124418.370, residual SS 11205191199.519, total SS 23912315617.889 and F 247.788. The single-predictor slope is 0.565642. The Breusch–Pagan output reports chi-square 936.0886 (1 df, p < 0.0001), all four normality tests report p < 0.0001, and Bartlett reports chi-square 16.35007 (2 df, p = 0.0003).

## Checks

`devtools::document()` succeeded. The final `devtools::check(document = FALSE, args = "--as-cran", check_dir = ...)` completed in 8 minutes 20.7 seconds with **0 errors, 0 warnings and 0 notes**. Tests took 230 seconds (under four minutes); ordinary examples took 37 seconds and vignette rebuilding took 28 seconds (each under one minute). The additional `--run-donttest` example pass took 123 seconds, exceeding one minute. `devtools::check` invoked R CMD check with `--no-manual --as-cran` and disabled CRAN incoming checks; no PDF manual or incoming checks were run.

An initial check failed at DESCRIPTION metadata (1 error and 1 warning) while the environment tried to use an unavailable `C.UTF-8` locale, and the build reported that version 3 serialization would raise the minimum R version. The final check used `LC_ALL=C`, `LANG=C`, and version 2 dataset serialization; it succeeded. All nine changed vignettes rendered successfully, and `git diff --check` was clean.

## Search

`rg -n -i '\b(trees|mtcars|cars|iris|penguins|PlantGrowth|hsb|surgical)\b' vignettes` produced no matches (exit 1): zero uses or mentions remain.

## Commands and limitations

The investigation used `Get-Location`, `git status --short`, `git branch --show-current`, `Get-Content AGENTS.md`, `rg` for old datasets and model/data references, `Get-ChildItem` for vignettes and data docs, `Get-Command R, Rscript` and `Get-Command pandoc`, and `Get-Content` for affected vignettes, DESCRIPTION, NEWS, .Rbuildignore and the release checklist. `notes/package-release-checklist.md` was absent. `git diff --stat`, `git diff --check`, `git config user.name`, `git config user.email`, and `Get-ChildItem Env:R*` were used to inspect the changes and environment. The UCI source licence was verified on the source page through web search. The temporary editing scripts below record the full transformation commands.

The R commands were `Rscript data-raw/wholesale_customers.R`, `Rscript -e "devtools::document()"`, `Rscript -e "devtools::check(document=FALSE, args='--as-cran')"`, and, with `$env:LC_ALL='C'; $env:LANG='C'`, `Rscript -e "devtools::check(document=FALSE, args='--as-cran', check_dir=file.path(Sys.getenv('TEMP'), 'olsrr-bizdata-check'))"`; the other R commands inspected installed dependencies, model summaries, tests, and leverage. Final HTML was searched with `rg` for the quoted output. `Get-Item data/wholesale_customers.rda` supplied the file size. The rendering and editing scripts were written to `%TEMP%` and run with `Rscript` and `python`, respectively. `git diff --check` and the forbidden-dataset search were repeated before commit. The primary editing and rendering scripts are reproduced below. An exact transcript of every exploratory shell command was not retained; the investigation and verification commands above are the available record.

<details><summary>olsrr_replace.py</summary>

```python
from pathlib import Path
p=Path('vignettes')
for name in ['influence_measures.Rmd','regression_diagnostics.Rmd','variable_selection.Rmd']:
 s=(p/name).read_text(encoding='utf-8')
 s=s.replace('mpg ~ disp + hp + wt + qsec, data = mtcars','milk ~ grocery + detergents_paper + fresh + frozen, data = wholesale_customers')
 s=s.replace('mpg ~ disp + hp + wt, data = mtcars','milk ~ grocery + detergents_paper + fresh, data = wholesale_customers')
 s=s.replace('read ~ write + math + science, data = hsb','milk ~ grocery + detergents_paper + fresh, data = wholesale_customers')
 s=s.replace('mpg ~ disp, data = mtcars','milk ~ grocery, data = wholesale_customers')
 s=s.replace("model, 'drat'", "model, 'frozen'")
 s=s.replace('You can learn more about the data [here](https://olsrr.rsquaredacademy.com/reference/surgical).','The wholesale customer data record annual spending by product category. Can spending in the other categories help explain milk spending?')
 s=s.replace('lm(y ~ ., data = surgical)','lm(milk ~ grocery + detergents_paper + fresh + frozen + delicassen, data = wholesale_customers)')
 s=s.replace('c("age", "alc_mod")','c("grocery", "fresh")').replace('c(5, 7)','c(2, 4)')
 s=s.replace('In the below example, as `liver_test` does not meet the threshold for selection, none of the \nvariables after `liver_test` are considered for further selection i.e. the stepwise selection ends as soon\nas it comes across a variable that does not meet the selection threshold.', 'In the example below, selection stops at the first spending category that does not meet the threshold; later categories are not considered.')
 s=s.replace('lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)','lm(milk ~ grocery + detergents_paper + fresh + frozen + delicassen, data = wholesale_customers)')
 anchor='library(nortest)\n```'
 s=s.replace(anchor, anchor+'\n\nThe examples use annual spending by 440 wholesale customers. They ask how milk spending relates to spending in other product categories; these are observational associations. See `?wholesale_customers` for source and units.\n',1)
 if name=='influence_measures.Rmd': s=s.replace('Graph for detecting influential observations.','Graph for detecting influential observations. The wholesale data include customers with unusual spending combinations; inspect them rather than deleting them automatically.')
 (p/name).write_text(s,encoding='utf-8')
(p/'residual_diagnostics.Rmd').write_text((p/'residual_diagnostics.Rmd').read_text(encoding='utf-8').replace('What does a braking-distance model miss?', 'What does a customer-spending model miss?'),encoding='utf-8')

```
</details>

<details><summary>olsrr_guides.py</summary>

```python
from pathlib import Path
p=Path('vignettes')
def write(name,title,body):
 (p/name).write_text('---\ntitle: "'+title+'"\noutput: rmarkdown::html_vignette\nvignette: >\n  %\\VignetteIndexEntry{'+title+'}\n  %\\VignetteEngine{knitr::rmarkdown}\n  %\\VignetteEncoding{UTF-8}\n---\n\n'+body,encoding='utf-8')
write('getting-started.Rmd','Can product spending explain milk spending?', '''Can grocery and detergent spending help describe milk spending? We fit one model, read its coefficients and inspect what it misses. The model accounts for about 53% of observed variation, while substantial residuals remain.

## Understand the data

The `wholesale_customers` data contain annual spending for 440 wholesale customers. Each row is a customer; spending is in monetary units, with currency unspecified. These observational associations do not establish the effect of changing a customer's purchases. See `?wholesale_customers` for the source and licence.

```{r setup, include=FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4.5, fig.align = "center")
```

```{r data, message=FALSE}
library(olsrr)
customer_data <- wholesale_customers[c("milk", "grocery", "detergents_paper")]
stopifnot(nrow(customer_data) == 440L, !anyNA(customer_data))
head(customer_data)
```

All three columns are complete. When adapting the example, decide how to handle missing values before comparing models.

## Fit once and read the result

Use `lm()` with the response on the left of `~` and predictors on the right. The additive model has no interaction.

```{r regression}
spending_model <- lm(milk ~ grocery + detergents_paper, data = customer_data)
ols_regress(spending_model)
```

Holding detergent spending fixed, one additional monetary unit of grocery spending is associated with about 0.623 units more milk spending. Holding grocery spending fixed, the detergent slope is -0.124, with a 95% confidence interval from -0.386 to 0.137. These conditional comparisons do not imply an intervention effect. The intercept refers to zero spending on both predictors, so extrapolation needs care.

## Inspect what remains unexplained

A residual is observed minus fitted milk spending. A positive residual identifies a customer spending more on milk than fitted.

```{r residual-plot, fig.cap="Milk spending residuals against fitted spending.", fig.alt="Residual milk spending against fitted milk spending, with several large positive and negative residuals."}
residual_plot <- ols_plot_resid_fit(spending_model, print_plot = FALSE)
residual_plot
```

The large residuals warrant investigation of individual customers and the mean relationship. R-squared does not establish that the straight-line mean or uncertainty calculations are adequate. Here `print_plot = FALSE` returns a ggplot directly; other plotting functions can return lists.

## Put the fit in perspective

```{r performance}
performance <- ols_model_performance(spending_model)
knitr::kable(data.frame(
  Measure = c("R-squared", "Adjusted R-squared", "Predicted R-squared", "RMSE"),
  Value = c(performance$rsq, performance$adjr, performance$prsq, performance$rmse)
), digits = 3, caption = "Performance for the same milk-spending model.")
```

R-squared is 0.531, and RMSE is 5063.711 monetary units. Predicted R-squared is 0.490 and uses leave-one-out prediction errors; it is not independent validation on new customers. RMSE describes residual scatter, not an error bound for an individual prediction.

Next, learn to [read a regression table](regression-output.html), then investigate [residual diagnostics](residual-diagnostics.html). See `?ols_regress`, `?ols_plot_resid_fit` and `?ols_model_performance` for their interfaces.
''')
write('regression-output.Rmd','Read a customer-spending regression model', '''How does milk spending vary with grocery spending, after accounting for detergent and paper spending? We read one regression result and connect its coefficients, uncertainty and residual error.

## Understand the measurements

The `wholesale_customers` data contain annual spending by 440 wholesale customers in monetary units; the currency is unspecified. Each row is a customer. See `?wholesale_customers` for the source and licence.

```{r setup, include=FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
```

```{r data, message=FALSE}
library(olsrr)
customer_data <- wholesale_customers[c("milk", "grocery", "detergents_paper")]
stopifnot(nrow(customer_data) == 440L, !anyNA(customer_data))
head(customer_data)
```

All three measurements are present. The model describes observed associations, not the effect of an intervention on purchases.

## Fit one model

```{r model}
spending_model <- lm(milk ~ grocery + detergents_paper, data = customer_data)
```

Each additive slope compares customers holding the other predictor fixed. The model does not include interactions or curved relationships.

## Read the regression table

```{r regression}
ols_regress(spending_model)
```

The **Beta** estimate for `grocery`, 0.623, means about 0.623 additional monetary units of milk spending per additional unit of grocery spending, holding detergent and paper spending fixed. The corresponding detergent slope is -0.124. Standardized betas use a different scale.

The **lower** and **upper** columns give 95% confidence intervals under the model assumptions: 0.492 to 0.754 for grocery and -0.386 to 0.137 for detergents and paper. These describe uncertainty in conditional mean slopes, not individual customer spending. The intercept is 1198.484 units when both predictors are zero; extrapolation requires care.

## Connect the output to base R

```{r base-comparison}
base_summary <- summary(spending_model)
knitr::kable(base_summary$coefficients[, c("Estimate", "Std. Error")],
             digits = 3, caption = "The same coefficients and standard errors from summary(lm(...)).")
```

The estimates and standard errors agree. R's residual standard error is the RMSE, 5063.711, calculated from the residual sum of squares and 437 residual degrees of freedom. It describes scatter rather than a prediction error bound.

## Understand the ANOVA decomposition

```{r anova, results="hide"}
spending_anova <- ols_anova(spending_model)
spending_anova
```

The total sum of squares is 23912315617.889, comprising 12707124418.370 for regression and 11205191199.519 for residuals. Their ratio gives R-squared of 0.531. There are two regression and 437 residual degrees of freedom. The overall F statistic is 247.788 and tests whether both slopes are zero; it does not validate the model's shape or error assumptions.

Continue with [residual diagnostics](residual-diagnostics.html), or revisit [getting started](getting-started.html). See `?ols_regress` and `?ols_anova` for the remaining fields.
''')
write('residual-diagnostics.Rmd','What does a customer-spending model miss?', '''Does a straight line capture how milk spending changes with grocery spending? We fit one model and inspect two residual plots. Large residuals and an uneven spread raise questions about the mean and variance.

## Understand the data and fit

The `wholesale_customers` data contain annual spending by 440 customers in monetary units, with currency unspecified. All observations used here are complete. See `?wholesale_customers` for the source and licence.

```{r setup, include=FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7, fig.height = 4.5, fig.align = "center")
```

```{r data, message=FALSE}
library(olsrr)
customer_data <- wholesale_customers[c("milk", "grocery")]
stopifnot(nrow(customer_data) == 440L, !anyNA(customer_data))
head(customer_data)
```

```{r model}
spending_model <- lm(milk ~ grocery, data = customer_data)
coef(spending_model)
```

The fitted slope is about 0.566 units of milk spending per unit of grocery spending. A positive residual means observed milk spending exceeds the fitted value. R-squared is about 0.531, but it does not establish whether the residual pattern is acceptable.

The assumptions concern unobserved errors conditional on predictors. Fitted residuals estimate those errors; estimating coefficients makes residuals dependent and gives them different variances even under constant error variance.

## Look for shape and changing spread

```{r residual-fit, fig.cap="Milk spending residuals against fitted spending, in monetary units.", fig.alt="Residuals against fitted milk spending; residuals vary substantially, with extreme positive and negative observations at high fitted values."}
ols_plot_resid_fit(spending_model)
```

The residual spread is much larger among some customers with high fitted spending. Investigate both the variance and whether the straight-line mean misses structure. A large residual calls for checking the observation and its context; it is not a reason by itself to delete a row.

## Compare the residual distribution with normality

```{r residual-qq, fig.cap="Normal Q-Q plot of milk spending residuals.", fig.alt="Normal Q-Q plot of residuals with substantial departures from the reference line in both tails."}
ols_plot_resid_qq(spending_model)
```

The tails depart substantially from the reference line. A missed mean relationship, changing variance or unusual observations can affect this plot. Normal errors are relevant to exact small-sample inference under the usual assumptions, but are not required merely to calculate OLS coefficients. Neither plot checks independence.

## Choose the next investigation

Continue with [variance and normality tests](variance-normality-tests.html) on this same model. Tests cannot choose a transformation or certify the entire model.

| Question | Function |
|---|---|
| View several diagnostic summaries together | `ols_plot_diagnostics()` |
| Inspect residuals as bars or a box plot | `ols_plot_resid_hist()`, `ols_plot_resid_box()` |
| Inspect the observed response before fitting | `ols_plot_response()` |
| Draw a simple regression line from vectors | `ols_plot_reg_line(customer_data$milk, customer_data$grocery)` |

Return to [regression output](regression-output.html) for coefficient and RMSE interpretation. Keep the same observations when comparing tables and plots.
''')

```
</details>

<details><summary>olsrr_variance.py</summary>

```python
from pathlib import Path
p=Path('vignettes/variance-normality-tests.Rmd')
s=p.read_text(encoding='utf-8')
s=s.replace('braking-distance residual plot','customer-spending residual plot')
a=s.index("**Example data: R's built-in cars dataset**")
b=s.index('```{r setup',a)
s=s[:a]+'''**Example data: wholesale customer spending**

The 440 observations record annual spending in monetary units, with currency unspecified. The earlier plots showed uneven residual spread and departures from normality. Fit the same milk-on-grocery model so that the tests address those findings. See `?wholesale_customers` for the source and licence.

'''+s[b:]
s=s.replace('braking_data <- datasets::cars','customer_data <- wholesale_customers[c("milk", "grocery")]').replace('nrow(braking_data) == 50L, !anyNA(braking_data)','nrow(customer_data) == 440L, !anyNA(customer_data)').replace('braking_model <- lm(dist ~ speed, data = braking_data)','spending_model <- lm(milk ~ grocery, data = customer_data)').replace('braking_model','spending_model').replace('conditional on speed','conditional on grocery spending').replace('The chi-square statistic is 4.650 with one degree of freedom and p = 0.0310.','The chi-square statistic is 936.089 with one degree of freedom and p < 0.0001.').replace('The Shapiro-Wilk p-value is 0.0215; the Lilliefors, Cramer-von Mises and\nAnderson-Darling p-values are 0.0353, 0.0483 and 0.0369.','The Shapiro-Wilk, Lilliefors, Cramer-von Mises and Anderson-Darling p-values are all below 0.0001.').replace("the Q-Q plot's upper tail", "the Q-Q plot's tails").replace('new vehicles','new customers')
a=s.index("**Example data: R's built-in PlantGrowth dataset**")
b=s.index('Next, revisit',a)
s=s[:a]+'''**Example data: wholesale customer regions**

Bartlett's test asks whether population variances are equal across groups. Here the question concerns milk spending in Lisbon, Porto and the other regions. This is a separate grouped question, not a replacement test of regression residual variance.

```{r grouped-variance}
ols_test_bartlett(wholesale_customers, "milk", group_var = "region")
```

The statistic is 16.350 on two degrees of freedom, with p = 0.0003. This is evidence against equal group variances. Bartlett's calibration is sensitive to departures from normality and assumes independent observations within and between groups. Inspect the group distributions before relying on it.

'''+s[b:]
p.write_text(s,encoding='utf-8')
p=Path('NEWS.md'); s=p.read_text(encoding='utf-8').replace('# olsrr (development version)\n','# olsrr (development version)\n\n- New business datasets: wholesale customer spending is used in the vignettes (pkg-olsrr-bizdata-vign).\n',1); p.write_text(s,encoding='utf-8')

```
</details>

<details><summary>olsrr_eol.py</summary>

```python
from pathlib import Path
files = ['.Rbuildignore','NEWS.md','R/ols-data-wholesale-customers.R','data-raw/wholesale_customers.R'] + [str(x) for x in Path('vignettes').glob('*.Rmd') if x.name in {'getting-started.Rmd','influence_measures.Rmd','regression-output.Rmd','regression_diagnostics.Rmd','residual-diagnostics.Rmd','residual_diagnostics.Rmd','variable_selection.Rmd','variance-normality-tests.Rmd'}]
for name in files:
 p=Path(name); b=p.read_bytes(); p.write_bytes(b.replace(b'\r\n',b'\n').removeprefix(b'\xef\xbb\xbf'))

```
</details>

<details><summary>olsrr_fixes.py</summary>

```python
from pathlib import Path
p=Path('vignettes/intro.Rmd'); s=p.read_text(encoding='utf-8').replace('Can weight and horsepower explain fuel economy?', 'Can product spending explain milk spending?'); p.write_bytes(s.encode())
p=Path('vignettes/regression-output.Rmd'); s=p.read_text(encoding='utf-8').replace('```{r anova, results="hide"}', '```{r anova}'); p.write_bytes(s.encode())
p=Path('NEWS.md'); s=p.read_text(encoding='utf-8').replace('(pkg-olsrr-bizdata-vign).','(@aravindhebbali, pkg-olsrr-bizdata-vign).',1); p.write_bytes(s.encode())

```
</details>

<details><summary>olsrr_version.py</summary>

```python
from pathlib import Path
p=Path('data-raw/wholesale_customers.R'); s=p.read_text(encoding='utf-8').replace('compress = "xz")','compress = "xz", version = 2)'); p.write_bytes(s.encode())

```
</details>

<details><summary>olsrr_render.R</summary>

```r
devtools::load_all(quiet = TRUE)
files <- c('getting-started', 'influence_measures', 'regression_diagnostics', 'regression-output', 'residual_diagnostics', 'residual-diagnostics', 'variable_selection', 'variance-normality-tests')
out <- file.path(tempdir(), 'olsrr-bizdata-render')
dir.create(out)
cat('Render directory:', out, '\n')
for (f in files) {
  cat('RENDER', f, '\n')
  t <- system.time(tryCatch(rmarkdown::render(file.path('vignettes', paste0(f, '.Rmd')), output_dir = out, quiet = TRUE, envir = new.env(parent = globalenv())), error = function(e) cat('ERROR', conditionMessage(e), '\n')))
  cat('TIME', f, t[['elapsed']], '\n')
}

```
</details>

<details><summary>olsrr_render_final.R</summary>

```r
devtools::load_all(quiet = TRUE)
files <- c('getting-started', 'intro', 'influence_measures', 'regression_diagnostics', 'regression-output', 'residual_diagnostics', 'residual-diagnostics', 'variable_selection', 'variance-normality-tests')
out <- file.path(Sys.getenv('TEMP'), 'olsrr-bizdata-render')
dir.create(out, showWarnings = FALSE)
cat('Render directory:', out, '\n')
for (f in files) {
  cat('RENDER', f, '\n')
  t <- system.time(rmarkdown::render(file.path('vignettes', paste0(f, '.Rmd')), output_dir = out, quiet = TRUE, envir = new.env(parent = globalenv())))
  cat('TIME', f, t[['elapsed']], '\n')
}

```
</details>

<details><summary>olsrr_report.py</summary>

```python
from pathlib import Path
import re
p=Path('BIZDATA-REPORT.md')
rows=[]
def add(v, section, old, fn, q): rows.append(f'| {v} | {section} | {old} → wholesale_customers | `{fn}` | {q} |')
add('getting-started','Fit once and read the result','mtcars','ols_regress','How do grocery and detergent spending relate to milk spending?')
add('getting-started','Inspect what remains unexplained','mtcars','ols_plot_resid_fit','Where does the milk-spending fit miss observations?')
add('getting-started','Put the fit in perspective','mtcars','ols_model_performance','How well does the milk-spending model fit and predict left-out observations?')
add('regression-output','Read the regression table','trees','ols_regress','How do product-spending slopes and their uncertainty compare?')
add('regression-output','Connect the output to base R','trees','summary.lm','Do base R and olsrr report the same estimates?')
add('regression-output','Understand the ANOVA decomposition','trees','ols_anova','How does milk-spending variation divide into fitted and residual components?')
add('residual-diagnostics','Look for shape and changing spread','cars','ols_plot_resid_fit','Does milk-spending residual spread change with fitted values?')
add('residual-diagnostics','Compare the residual distribution with normality','cars','ols_plot_resid_qq','Do milk-spending residuals follow a normal shape?')
add('variance-normality-tests','Test a specified variance pattern','cars','ols_test_breusch_pagan','Does milk-spending error variance vary with fitted values?')
add('variance-normality-tests','Ask a separate normality question','cars','ols_test_normality','Do milk-spending residuals depart from normality?')
add('variance-normality-tests','Compare grouped variances','PlantGrowth','ols_test_bartlett','Does milk-spending variance differ by customer region?')
for v in ['influence_measures','regression_diagnostics','variable_selection']:
 s=Path('vignettes',v+'.Rmd').read_text(encoding='utf-8'); section=''; in_chunk=False; chunk=''; lastsection=''
 for line in s.splitlines():
  if line.startswith('#'): section=re.sub(r'^#+\s*','',line).strip()
  if line.startswith('```{r'): in_chunk=True; chunk=''; lastsection=section; continue
  if line.startswith('```') and in_chunk:
   in_chunk=False
   funcs=re.findall(r'\b(ols_[a-zA-Z0-9_]+)\s*\(',chunk)
   if funcs:
    old=('hsb' if 'ols_plot_resid_lev' in funcs else 'mtcars') if v=='influence_measures' else ('mtcars' if v=='regression_diagnostics' or lastsection in ['All Possible Regression','Best Subset Regression'] else 'surgical')
    for fn in dict.fromkeys(funcs): add(v,lastsection,old,fn,'What does this method reveal about the milk and other product spending model?')
   continue
  if in_chunk: chunk+=line+'\n'
report='''# Business data vignette report

## Changed examples

All new examples use annual wholesale customer spending. The two legacy redirect pages were updated only to match their destination titles.

| Vignette | Section | Old → new data | Function taught | New question |
|---|---|---|---|---|
'''+ '\n'.join(rows)+'''

## Dataset

`wholesale_customers`: 440 rows, eight columns, 5,132 bytes (`data/wholesale_customers.rda`, xz). Cardoso, M. (2013), [Wholesale customers, UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/292/wholesale+customers), [DOI 10.24432/C5030X](https://doi.org/10.24432/C5030X). The source page specifies Creative Commons Attribution 4.0 International (CC BY 4.0). Rebuilt with `data-raw/wholesale_customers.R`, using version 2 serialization to preserve the declared R compatibility. Annual product-category spend is relevant to business and supplies a multi-predictor model, grouped observations, repeated grocery values, variance differences and real high leverage. Currency is not specified by the source.

For the three-predictor influence model, observation 86 has leverage 0.1852901 versus the 2p/n reference of 0.01818182. The simple model has 10 duplicated grocery values for the pure-error example.

## Rendering and numeric evidence

Final changed vignettes were rendered to `%TEMP%/olsrr-bizdata-render`, not `docs/`. The per-vignette times in seconds: getting-started 1.45; intro 0.15; influence_measures 4.43; regression_diagnostics 6.51; regression-output 0.87; residual_diagnostics 0.14; residual-diagnostics 1.06; variable_selection 2.12; variance-normality-tests 0.67.

Rendered output: grocery coefficient 0.623 (95% interval 0.492–0.754), detergent coefficient -0.124 (interval -0.386–0.137), R-squared 0.531, predicted R-squared 0.490 and RMSE 5063.711; regression SS 12707124418.370, residual SS 11205191199.519, total SS 23912315617.889 and F 247.788. The single-predictor slope is 0.565642. The Breusch–Pagan output reports chi-square 936.0886 (1 df, p < 0.0001), all four normality tests report p < 0.0001, and Bartlett reports chi-square 16.35007 (2 df, p = 0.0003).

## Checks

`devtools::document()` succeeded. The final `devtools::check(document = FALSE, args = "--as-cran", check_dir = ...)` completed in 8 minutes 20.7 seconds with **0 errors, 0 warnings and 0 notes**. Tests took 230 seconds (under four minutes); ordinary examples took 37 seconds and vignette rebuilding took 28 seconds (each under one minute). The additional `--run-donttest` example pass took 123 seconds, exceeding one minute. `devtools::check` invoked R CMD check with `--no-manual --as-cran` and disabled CRAN incoming checks; no PDF manual or incoming checks were run.

An initial check failed at DESCRIPTION metadata (1 error and 1 warning) while the environment tried to use an unavailable `C.UTF-8` locale, and the build reported that version 3 serialization would raise the minimum R version. The final check used `LC_ALL=C`, `LANG=C`, and version 2 dataset serialization; it succeeded. All nine changed vignettes rendered successfully, and `git diff --check` was clean.

## Search

`rg -n -i '\\b(trees|mtcars|cars|iris|penguins|PlantGrowth|hsb|surgical)\\b' vignettes` produced no matches (exit 1): zero uses or mentions remain.

## Commands and limitations

The investigation used `Get-Location`, `git status --short`, `git branch --show-current`, `Get-Content AGENTS.md`, `rg` for old datasets and model/data references, `Get-ChildItem` for vignettes and data docs, `Get-Command R, Rscript` and `Get-Command pandoc`, and `Get-Content` for affected vignettes, DESCRIPTION, NEWS, .Rbuildignore and the release checklist. `notes/package-release-checklist.md` was absent. `git diff --stat`, `git diff --check`, `git config user.name`, `git config user.email`, and `Get-ChildItem Env:R*` were used to inspect the changes and environment. The UCI source licence was verified on the source page through web search. The temporary editing scripts below record the full transformation commands.

The R commands were `Rscript data-raw/wholesale_customers.R`, `Rscript -e "devtools::document()"`, `Rscript -e "devtools::check(document=FALSE, args='--as-cran')"`, and, with `$env:LC_ALL='C'; $env:LANG='C'`, `Rscript -e "devtools::check(document=FALSE, args='--as-cran', check_dir=file.path(Sys.getenv('TEMP'), 'olsrr-bizdata-check'))"`; the other R commands inspected installed dependencies, model summaries, tests, and leverage. Final HTML was searched with `rg` for the quoted output. `Get-Item data/wholesale_customers.rda` supplied the file size. The rendering and editing scripts were written to `%TEMP%` and run with `Rscript` and `python`, respectively. `git diff --check` and the forbidden-dataset search were repeated before commit. The complete temporary scripts are reproduced below.
'''
for name in ['olsrr_replace.py','olsrr_guides.py','olsrr_variance.py','olsrr_eol.py','olsrr_fixes.py','olsrr_version.py','olsrr_render.R','olsrr_render_final.R']:
 f=Path(__import__('os').environ['TEMP'])/name
 if f.exists(): report+='\n<details><summary>'+name+'</summary>\n\n```'+('r' if name.endswith('.R') else 'python')+'\n'+f.read_text(encoding='utf-8-sig')+'\n```\n</details>\n'
p.write_bytes(report.encode('utf-8'))

```
</details>

<details><summary>olsrr_report_questions.py</summary>

```python
from pathlib import Path
p=Path('BIZDATA-REPORT.md'); s=p.read_text(encoding='utf-8')
qs={'ols_plot_cooksd_bar':'Which customers most influence fitted milk spending?', 'ols_plot_cooksd_chart':'Which customers have large Cook distances?', 'ols_plot_dfbetas':'Which customers change individual spending slopes?', 'ols_plot_dffits':'Which customers substantially change their fitted milk spending?', 'ols_plot_resid_stud':'Which customers have unusually large studentized milk-spending residuals?', 'ols_plot_resid_stand':'Which customers have unusually large standardized residuals?', 'ols_plot_resid_lev':'Which customers combine leverage with unusual residuals?', 'ols_plot_resid_stud_fit':'Which customers have unusual deleted residuals across fitted spending?', 'ols_plot_hadi':'Which customers are influential under Hadi’s measure?', 'ols_plot_resid_pot':'Which customers are outliers, high-leverage observations, or both?', 'ols_vif_tol':'How correlated are the product-spending predictors?', 'ols_eigen_cindex':'Do product-spending predictors show a large condition index?', 'ols_coll_diag':'Which predictors contribute to spending-model collinearity?', 'ols_plot_resid_fit_spread':'How does fitted spending spread compare with residual spread?', 'ols_correlations':'How much does each spending predictor contribute?', 'ols_plot_obs_fit':'How closely do fitted values track observed milk spending?', 'ols_pure_error_anova':'Is the straight-line mean adequate at repeated grocery-spending values?', 'ols_plot_diagnostics':'What do the combined spending-model diagnostics show?', 'ols_plot_resid_regressor':'Does frozen spending explain residual milk spending?', 'ols_plot_added_variable':'What does each spending predictor add after the others?', 'ols_plot_comp_plus_resid':'Is the relationship with each spending predictor curved?', 'ols_step_all_possible':'Which subsets of product categories explain milk spending?', 'ols_step_best_subset':'Which category subsets optimize model-selection criteria?', 'ols_step_forward_p':'Which spending categories enter under forward selection?', 'ols_step_backward_p':'Which spending categories remain under backward elimination?', 'ols_step_forward_adj_r2':'How does forward selection by adjusted R-squared describe milk spending?'}
for fn,q in qs.items():
 lines=s.splitlines(); lines=[line.replace('What does this method reveal about the milk and other product spending model?',q) if ('`'+fn+'`') in line else line for line in lines]; s='\n'.join(lines)+'\n'
s=s.replace('| variable_selection | Visualization | surgical → wholesale_customers | `ols_step_forward_adj_r2` |', '| variable_selection | Visualization | surgical → wholesale_customers | `ols_step_forward_adj_r2`, `plot` |')
p.write_bytes(s.encode('utf-8'))

```
</details>

Final verification and commit commands: `git diff --check`; `rg -n -i "\b(trees|mtcars|cars|iris|penguins|PlantGrowth|hsb|surgical)\b" vignettes`; `git status --short`; `git add .Rbuildignore NEWS.md BIZDATA-REPORT.md R/ols-data-wholesale-customers.R data-raw/wholesale_customers.R data/wholesale_customers.rda man/wholesale_customers.Rd vignettes`; `git diff --cached --check`; `git commit -m "Vignettes: business data for every example; new datasets wholesale_customers (pkg-olsrr-bizdata-vign)"`; `git status --short`.
