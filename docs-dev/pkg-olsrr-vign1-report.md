# olsrr vignette rewrite: phases 1-2 (pkg-olsrr-vign1)

Date: 2026-11-20. Branch: `task/olsrr-vign1`, starting at `07c48c2`.
Applied the editorial corrections, A0/A1/A3/A5 plan and feasibility steps 1-2
in `pkg-standards-3-olsrr-vignettes.md`, with the common article template
in `pkg-standards-2-yahoofinancer-vignettes.md`. Those two supplied plan files
were already untracked at the start; they are not included in this commit.

## Files and scope

| File | Change |
|---|---|
| `vignettes/getting-started.Rmd` | A0: weight and horsepower associated with fuel economy, using mtcars. |
| `vignettes/regression-output.Rmd` | A1: read coefficients, intervals, residual RMSE and ANOVA for trees. |
| `vignettes/residual-diagnostics.Rmd` | A3: read two residual plots for cars. |
| `vignettes/variance-normality-tests.Rmd` | A5: interpret variance and normality tests for cars, with a separate PlantGrowth sidebar. |
| `vignettes/intro.Rmd` | Legacy URL stub linking to getting-started. |
| `vignettes/residual_diagnostics.Rmd` | Legacy URL stub linking to residual-diagnostics. |
| `vignettes/heteroskedasticity.Rmd` | Legacy URL stub linking to variance-normality-tests. |
| `_pkgdown.yml` | Put the four new articles first in Guides and the article index; list compatibility pages separately. |
| `NEWS.md` | Add only the requested development bullet, verbatim. |
| `docs-dev/pkg-olsrr-vign1-report.md` | This implementation and validation report. |

The four reserved vignette sources (`regression_diagnostics`,
`influence_measures`, `variable_selection`, `reference-media`) are unchanged.
No package implementation, tests, DESCRIPTION, workflows, other repositories
or generated `docs/` files were changed. No full pkgdown build, branch switch,
push, GitHub access or PR operation was performed.

The brief calls this 0.7.2 development, but DESCRIPTION at the requested HEAD
actually declares **0.7.1**. The temporary installation was built from this
branch's code, not fetched from CRAN. DESCRIPTION was left unchanged as required.
The articles contain no package version claims.

## Phase 1: baseline execution and complete output comparison

Before changing the old sources, copied all seven Rmd files to the temporary
workspace and rendered each using `rmarkdown::render()` with the branch-built
olsrr first on `.libPaths()`. All analytical chunks evaluated. Network entry
points were traced to stop on access (base `url`/`socketConnection`,
utils `download.file`, curl fetch/download, httr GET/POST and httr2
`req_perform`); none was called. This was an R-level offline execution check,
not an operating-system firewall test.

| Original vignette | Offline render | Console comparison | Figures inspected |
|---|---|---|---:|
| `intro` | PASS | Four of five output blocks changed: p-value display. | 5 |
| `residual_diagnostics` | PASS | One of two blocks changed: Lilliefors label and p-value. | 3 |
| `heteroskedasticity` | PASS | All 14 blocks changed: p-value display. | 0 |
| `regression_diagnostics` | PASS | One of seven blocks changed: pure-error ANOVA p-value display. | 9 |
| `influence_measures` | PASS | No console output; plotted quantities unchanged on visual comparison. | 10 |
| `variable_selection` | PASS | Eight of 13 blocks changed: p-value display. Search paths and other results unchanged. | 1 |
| `reference-media` | PASS | No computational output. | 0 |

Compared every console/message block in fresh HTML with committed
`docs/articles/<slug>.html`; independently confirmed that the committed
Markdown companions have exactly the same normalized console/message rows.
Normalization removes blank lines, `##` prefixes, table rules and repeated
whitespace. Of 41 blocks, 28 changed and 13 did not. There are 111 changed
row occurrences, representing 53 distinct before/after row pairs, all listed
in the appendix with every occurrence identified. No changed numeric row
is omitted by the deduplication.

The one substantive numerical change is the normality result:
**Kolmogorov-Smirnov p = 0.7464 -> Lilliefors-corrected p = 0.3446**,
with statistic unchanged at 0.1152. The other changes are display precision:
four decimal p-values and `< 0.0001` for smaller values. Associated table
rules, headings and padding expand or contract to fit those strings.
Coefficients, sums of squares, MSE/RMSE, test statistics, degrees of freedom,
selection paths and non-p-value model metrics agree at the displayed precision.

The older approved audit identified MSE 5.801/RMSE 2.409 and Cramer-von Mises
2.8122/p 0.0000 in its older site snapshot. They are **already corrected at
this task's HEAD**: committed and fresh output both have MSE 6.875/RMSE 2.622
and Cramer-von Mises 0.0764/p 0.2230. These are not new changes in this job.
The current committed site metadata says last built 2026-11-19T10:05Z.

### Figure and non-numeric output differences

Inspected all 28 baseline figure files against their committed counterparts.
The fit-spread subplots in `intro:rfsplot` and
`regression_diagnostics:rfsplot` change from two identical
"Residual Fit Spread Plot" titles to **"Fit - Mean"** and **"Residual"**.
The same two titles change on the second `regression_diagnostics:diagpanel`
figure. The plotted values and thresholds are unchanged. The remaining
figures show no changed statistical content, including the histogram and
its normal curve, Cook's thresholds and labels, and selection-path values.

Standalone html_vignette rendering differs from pkgdown's graphics settings:
27 PNGs have half the committed pixel width and height; `variable_selection`'s
unsized plot is 288 x 288 rather than the site's 1400 x 865, clipping its title
and crowding labels. Font rasterization and transparent/white backgrounds also
differ. These are rendering-setting differences, not new estimates. The
selection source is reserved for a later phase; its explicit figure sizing is
recorded as a follow-up. All three figures in the new pages have explicit
dimensions and were visually checked at their final rendered size.

### Editorial corrections implemented and deferred

The rewrites interpret actual results, fit once per main question, preserve
rows, state units and observational limits, and remove the introduction's
function catalogue and p-value selection demonstration. Removing the old
introduction also removes the obsolete `dfbetas_panel` name. A0 demonstrates
that `ols_plot_resid_fit(..., print_plot = FALSE)` returns a ggplot directly
and says other plotting functions can return lists.

A3 distinguishes model errors from fitted residuals, explains estimation
constraints and dependence, separates mean shape from spread, and avoids
automatic deletion of unusual observations. A5 states the variance and
normality nulls, the BP normal-error calibration, conditional-mean/exogeneity
conditions for coefficient claims, and the limits of rejection and
non-rejection. It explains that normal-score correlation is not a serial
correlation diagnostic and that weighted diagnostics require interface checks.
Bartlett's grouped question is explicitly separate from regression variance.

The scope explicitly reserves four old sources for later phases. Consequently
the component-plus-residual formula, squared semipartial interpretation,
VIF/influence threshold wording, DFFITS parameter-count notation and selection
overfitting claims in those files were not edited here. Their precise
corrections are written in full under follow-ups below. This resolves the
general phase-1 correction request in favour of the narrower instruction to
leave those sources unchanged.

## Phase 2: reader path and article records

The common template is: question and answer early; identify data/units/rows;
fit once; answer with generated output; interpret specific numbers or visible
features; state limits; link a prerequisite/related task, a next task and
function references. There is no repeated complete script or skipped
analytical chunk. Approximate prose counts omit YAML, code and URL targets.

| Article | Question | Dataset and model | Main outcome | Prose words / chunks |
|---|---|---|---|---|
| A0, getting-started | Can weight and horsepower explain fuel economy? | R's built-in mtcars, 32 complete cars; `mpg ~ wt + hp`. | Conditional slopes about -3.88 mpg per 1,000 lb and -0.032 mpg per horsepower; R-squared 0.827 and RMSE 2.593 mpg; residuals motivate investigation. | 625 / 5 |
| A1, regression-output | How does tree volume change with diameter after accounting for height? | R's built-in trees, 31 complete black cherry trees; `Volume ~ Girth + Height`. | Diameter slope 4.708 cubic feet/inch with 95% interval 4.167 to 5.249; RMSE 3.882 cubic feet; interpreted ANOVA decomposition. Girth is explicitly identified as diameter. | 688 / 6 |
| A3, residual-diagnostics | What does a straight-line braking-distance model miss? | R's built-in cars, 50 complete observations; `dist ~ speed`. | Widening residual spread and a long positive tail prompt investigation of mean shape and variance; no diagnosis from R-squared alone. | 667 / 5 |
| A5, variance-normality-tests | Do tests support the residual-plot concerns, and what do they establish? | Same cars model; separate R's built-in PlantGrowth weight-by-group sidebar, 10 plants per group. | BP p = 0.0310 and normality p-values 0.0215, 0.0353, 0.0483, 0.0369 give evidence against specified nulls, without choosing a remedy. Bartlett p = 0.2371 is insufficient evidence against equal grouped variances. | 780 / 5 |

The core code is deliberately shorter than the plan's approximate 40-80 lines:
these high-level functions answer the questions without duplicated models or
padded examples. A1 prints one complete regression summary and a small base-R
coefficient comparison; its `ols_anova()` chunk evaluates with results hidden
because the same ANOVA table is already visible in the main summary. A5 links
score/F/RHS/multiple/p-adjustment/normal-score options instead of running a
catalogue of alternate tests. P-value stepwise selection appears only in the
unchanged later-phase article and the baseline audit inventory, not in the new
reader path.

### Complete figure list and alt text

| Article / chunk | Figure | Exact alt text |
|---|---|---|
| A0 / `residual-plot` | Residual fuel economy versus fitted mpg; caption identifies 32 mtcars observations and miles per US gallon. | Residual fuel economy against fitted mpg for 32 cars. Several residuals exceed five mpg, and the highest fitted values include large positive residuals above the zero line. |
| A3 / `residual-fit` | Stopping-distance residuals versus fitted distance; caption states feet on both axes. | Residuals against fitted stopping distance for 50 cars. The points spread more widely at larger fitted distances, with large positive residuals around 40 feet and negative residuals approaching minus 30 feet. |
| A3 / `residual-qq` | Normal Q-Q plot of the same stopping-distance residuals; caption states feet. | Normal Q-Q plot of stopping-distance residuals. Middle observations lie near the reference line, while several upper-tail observations bend above it, indicating a long positive tail. |

A1 and A5 have no figures; their interpreted tables are generated by the shown
code. Every actual image in the new rendered pages has the intended nonempty
alt attribute, checked in HTML.

## Stable URLs and navigation

Used **stub vignettes**, not `_pkgdown.yml` redirect rules. The original source
basenames and titles remain. Each is a short compatibility landing page with
a normal link to its replacement:

| Existing URL | Destination |
|---|---|
| `articles/intro.html` | `articles/getting-started.html` |
| `articles/residual_diagnostics.html` | `articles/residual-diagnostics.html` |
| `articles/heteroskedasticity.html` | `articles/variance-normality-tests.html` |

These are link-based migration stubs, not HTTP redirects or JavaScript/meta
refreshes. They also build as installed vignettes and preserve the old URL
without relying on hosting-specific redirect support. YAML titles and
VignetteIndexEntry titles match. The new pages are first in Guides and the
article index; the four remaining guides retain their slugs and existing
navbar links. The Media reference-menu entry is unchanged.

## Validation and environment

Environment: Windows 11, R 4.6.1 (ucrt), devtools 2.5.2, knitr 1.52,
rmarkdown 2.32, Pandoc 3.12.1. Build libraries, package archives, render copies,
figures, logs and R CMD check outputs are all under
`C:/Users/DELL/AppData/Local/Temp/pkg-olsrr-vign1`, outside the repository.

The requested `upgrade = "never"` is rejected by this installed devtools,
whose `upgrade` argument accepts TRUE/FALSE/NA. Used `upgrade = FALSE`, its
no-upgrade equivalent, with the temporary library first on `.libPaths()`.
An initial attempt to supply `lib` directly also failed because that version
of `devtools::install()` has no `lib` argument. Both errors occurred before
installation. The successful install path and loaded namespace were verified.

The inherited `C.UTF-8` locale is unavailable on Windows and generated startup
warnings during the first baseline run. Final renders, spelling/navigation
checks and R CMD check used the supported `English_United States.utf8` locale.
All 11 final articles render offline with no R-level render warnings. Pandoc
prints its existing `--mathjax` deprecation diagnostic for html_vignette.

- Package check: **0 errors, 0 warnings, 0 notes**; `devtools::check(document = FALSE, args = c("--no-manual"))`, with vignettes enabled. The final-source run under `check-final/olsrr.Rcheck` finished in 11m 4.5s; examples, run-donttest examples, tests, package vignettes and rebuilding vignette outputs all passed. Its testthat summary is **3,694 passing expectations, 0 failures, 0 warnings, 0 skips**. NOTEs: none. The earlier run under `check/olsrr.Rcheck`, before the canonical-reference-URL edit, also finished with 0 errors, 0 warnings and 0 notes in 10m 26.6s.
- Individual renders: original seven PASS; final 11 PASS, with all analytical chunks executed under the network guards.
- HTML/link checks: **191 internal destinations** across the four new pages and three stubs resolve, including heading fragments. Canonical olsrr reference URLs are mapped to committed reference HTML for offline checking in a temporary preview layout; using canonical URLs also avoids missing relative reference paths when reading installed vignettes. No site generator was run.
- Figure checks: all three new images have meaningful alt text and readable unclipped plots, verified in the rendered HTML and PNGs.
- Numeric checks: the quoted fuel-model performance, tree ANOVA and cars/PlantGrowth test results agree with values calculated using the temporary installation; coefficients and confidence intervals were also read from its actual output.
- Spelling: `spelling::spell_check_files()` reports no errors after correcting ordinary spelling variants and reviewing proper names and technical terms. Its ignore list contains olsrr, dataset names, test surnames, RMSE/ANOVA, mpg, ggplot, exogeneity, unbiasedness and von.
- Navigation: `_pkgdown.yml` parses; every one of the 11 source articles is listed exactly once.
- Live URL checking/urlchecker: skipped for this offline task. Local destinations, fragments and cached canonical reference targets were checked; no claim is made about live hosting or external-site availability.
- Git whitespace check: `git diff --check` passes. Reserved files and prohibited paths are unchanged.

### Commands run

Executed from the package root, using scripts outside the repository:

```r
task_work <- file.path(Sys.getenv("TEMP"), "pkg-olsrr-vign1")
task_lib <- file.path(task_work, "lib")
dir.create(task_lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(task_lib, .libPaths()))
devtools::install(quick = TRUE, upgrade = FALSE)
find.package("olsrr")
packageVersion("olsrr")

# For each original source, then each final source, copied to a temp directory:
rmarkdown::render(
  source_copy,
  output_options = list(keep_md = TRUE),
  envir = new.env(parent = globalenv()),
  quiet = TRUE
)

spelling::spell_check_files(new_sources, ignore = reviewed_terms)
yaml::read_yaml("_pkgdown.yml")
devtools::check(
  document = FALSE, args = c("--no-manual"),
  check_dir = file.path(task_work, "check-final"), error_on = "never"
)
```

PowerShell/R entry points:

```powershell
$taskTemp = Join-Path $env:TEMP 'pkg-olsrr-vign1'
Rscript --vanilla (Join-Path $taskTemp 'install.R')
Rscript --vanilla (Join-Path $taskTemp 'render.R') baseline
Rscript --vanilla (Join-Path $taskTemp 'results.R')
Rscript --vanilla (Join-Path $taskTemp 'extract.R')
python (Join-Path $taskTemp 'blocks.py')
python (Join-Path $taskTemp 'markdown_compare.py')
python (Join-Path $taskTemp 'figures.py')
Rscript --vanilla (Join-Path $taskTemp 'render.R') rewritten
$env:LANG = 'English_United States.utf8'
$env:LC_ALL = 'English_United States.utf8'
$env:LC_CTYPE = 'English_United States.utf8'
Rscript --vanilla (Join-Path $taskTemp 'editorial.R')
Rscript --vanilla (Join-Path $taskTemp 'render.R') final
Rscript --vanilla (Join-Path $taskTemp 'verify.R')
Rscript --vanilla (Join-Path $taskTemp 'check.R')
Rscript --vanilla (Join-Path $taskTemp 'check-final.R')
git diff --check
git add NEWS.md _pkgdown.yml vignettes/getting-started.Rmd vignettes/regression-output.Rmd vignettes/residual-diagnostics.Rmd vignettes/variance-normality-tests.Rmd vignettes/intro.Rmd vignettes/residual_diagnostics.Rmd vignettes/heteroskedasticity.Rmd docs-dev/pkg-olsrr-vign1-report.md
git diff --cached --check
git commit -m "olsrr: vignette rewrite phases 1-2 (A0, A1, A3, A5) (pkg-olsrr-vign1)"
```

The scripts additionally extract HTML with xml2, compare normalized output
using Python difflib, inventory plot dimensions with Pillow, and verify
rounded prose results against the installed functions. `results.R` smoke-runs
the planned models and saves plots for visual inspection. The intermediate
`rewritten` render exposed stub title mismatches, which were corrected before
the final render and package check. Final `verify.R` constructs a temporary
article/reference layout and checks href targets, fragment IDs and img alt
attributes. After replacing relative function-reference paths with canonical
olsrr URLs, repeated the guarded final renders and offline link checks and
completed the final-source full package check. No verification code was added
to package tests.

## Follow-ups, written in full

1. In a separately authorized site-build job, regenerate the site from these sources against the intended release build. Verify the new Guides order, all three legacy stub URLs, article cross-links, reference links, mobile tables and image alt text on the generated site. The committed `docs/` still contains the previous articles because this job explicitly prohibits a full pkgdown build and manual generated-file edits.
2. In the later A4/A7/A8 split of `regression_diagnostics.Rmd`, correct the component-plus-residual construction to **residual + coefficient * predictor**, plotted against the predictor. Distinguish it from added-variable plots, which residualize both the response and predictor against the other terms. Explain that **squared** semipartial correlation gives the unique change in R-squared; the signed correlation itself is not that change. Treat VIF and condition-index cutoffs as investigation aids. Keep `ols_pure_error_anova()` in supported simple regression with replicated predictor values, not arbitrary multivariable fits.
3. In the later A6 influence rewrite, preserve original observation IDs; treat Cook's D, DFBETAs and residual cutoffs as screening conventions, inspect the data and compare a clearly labelled sensitivity refit rather than automatically dropping cases. Define the DFFITS parameter count consistently: if p includes the intercept, the size-adjusted cutoff is `2 * sqrt(p / n)`; reconcile the other displayed formula and captions with that convention and the actual threshold option. Document plot extraction per function instead of promising every return has `$plot`.
4. In the later A2 selection rewrite, use the planned bounded swiss candidate set and common observations, demonstrate a justified all-subsets/forward-AIC comparison, state any forced terms by name and explain selection optimism and the need for independent validation. Replace the claim that searching prevents overfitting. Explain exponential subset growth with a bounded example and that the illustrated hierarchical option is ordered stopping, not a general interaction-heredity guarantee. Do not carry p-value stepwise selection into the new reader path. Give plots explicit dimensions so standalone vignette rendering does not clip titles or overlap labels.
5. Keep `reference-media.Rmd` unchanged until the separately planned resources phase. That phase should move the existing resource link to site resources/navigation while retaining its old URL, and should keep interactive launch snippets unevaluated during documentation builds.
6. Add the remaining planned family pages and any short media assets only in their authorized later phases. Reuse the exact article datasets, models and generated results, with one interpreted claim and one limitation per clip. A9 custom plot extraction remains optional; it is not a prerequisite for these four articles.
7. The release owner should reconcile the brief's 0.7.2 development label with the actual 0.7.1 DESCRIPTION in the appropriate version/release task. This vignette-only commit intentionally makes no version change. Run live external-link checks when network checking and site publication are in scope.

## Appendix: exhaustive phase-1 changed output rows

Locations use `<old-slug>:<output-block-number>`, counting visible console or
message blocks in source order, not code or figures. Before/after rows below
retain every displayed numeric field; only layout whitespace is normalized.
Identical row changes are listed once with **all** occurrence locations.

Changed-block key:

| Article | Output blocks and source calls |
|---|---|
| `heteroskedasticity` | 1 grouped Bartlett; 2 two-column Bartlett; 3 default BP; 4 RHS BP; 5 multiple BP; 6 Bonferroni BP; 7 Sidak BP; 8 Holm BP; 9 default score; 10 RHS score; 11 selected-variable score; 12 default F; 13 RHS F; 14 selected-variable F. |
| `intro` | 1 initial `ols_regress()`; 2 BP; 4 both-direction selection result; 5 backward-AIC result. |
| `regression_diagnostics` | 5 pure-error ANOVA. |
| `residual_diagnostics` | 1 normality table. |
| `variable_selection` | 4 forward result; 5 backward result; 6 forced terms by name; 7 forced terms by index; 8 adjusted-R-squared result; 10 final result following verbose output; 12 final result following progress output; 13 ordered hierarchical result. |

| Output location(s) | Committed output row | Fresh branch output row |
|---|---|---|
| `heteroskedasticity:1` | `Prob > Chi2 = 0.6657139` | `Prob > Chi2 = 0.6657` |
| `heteroskedasticity:2` | `Prob > Chi2 = 0.2687995` | `Prob > Chi2 = 0.2688` |
| `heteroskedasticity:3`, `intro:2` | `Prob > Chi2 = 0.231818` | `Prob > Chi2 = 0.2318` |
| `heteroskedasticity:4` | `Prob > Chi2 = 0.8241927` | `Prob > Chi2 = 0.8242` |
| `heteroskedasticity:5` | `disp 1.2355345 1 0.2663334` | `disp 1.2355345 1 0.2663` |
| `heteroskedasticity:5` | `hp 0.9209878 1 0.3372157` | `hp 0.9209878 1 0.3372` |
| `heteroskedasticity:5` | `wt 1.2529988 1 0.2629805` | `wt 1.2529988 1 0.2630` |
| `heteroskedasticity:5` | `drat 1.1668486 1 0.2800497` | `drat 1.1668486 1 0.2800` |
| `heteroskedasticity:5`, `heteroskedasticity:6`, `heteroskedasticity:7`, `heteroskedasticity:8` | `simultaneous 1.5138083 4 0.8241927` | `simultaneous 1.5138083 4 0.8242` |
| `heteroskedasticity:6`, `heteroskedasticity:8` | `disp 1.2355345 1 1.0000000` | `disp 1.2355345 1 1.0000` |
| `heteroskedasticity:6`, `heteroskedasticity:8` | `hp 0.9209878 1 1.0000000` | `hp 0.9209878 1 1.0000` |
| `heteroskedasticity:6`, `heteroskedasticity:8` | `wt 1.2529988 1 1.0000000` | `wt 1.2529988 1 1.0000` |
| `heteroskedasticity:6`, `heteroskedasticity:8` | `drat 1.1668486 1 1.0000000` | `drat 1.1668486 1 1.0000` |
| `heteroskedasticity:7` | `disp 1.2355345 1 0.7102690` | `disp 1.2355345 1 0.7103` |
| `heteroskedasticity:7` | `hp 0.9209878 1 0.8070305` | `hp 0.9209878 1 0.8070` |
| `heteroskedasticity:7` | `wt 1.2529988 1 0.7049362` | `wt 1.2529988 1 0.7049` |
| `heteroskedasticity:7` | `drat 1.1668486 1 0.7313356` | `drat 1.1668486 1 0.7313` |
| `heteroskedasticity:9` | `Prob > Chi2 = 0.4723832` | `Prob > Chi2 = 0.4724` |
| `heteroskedasticity:10` | `Prob > Chi2 = 0.7285114` | `Prob > Chi2 = 0.7285` |
| `heteroskedasticity:11` | `Prob > Chi2 = 0.6070405` | `Prob > Chi2 = 0.6070` |
| `heteroskedasticity:12` | `Prob > F = 0.4884154` | `Prob > F = 0.4884` |
| `heteroskedasticity:13` | `Prob > F = 0.7647271` | `Prob > F = 0.7647` |
| `heteroskedasticity:14` | `Prob > F = 0.631555` | `Prob > F = 0.6316` |
| `intro:1` | `Regression 940.412 4 235.103 34.195 0.0000` | `Regression 940.412 4 235.103 34.195 < 0.0001` |
| `intro:1` | `(Intercept) 27.330 8.639 3.164 0.004 9.604 45.055` | `(Intercept) 27.330 8.639 3.164 0.0038 9.604 45.055` |
| `intro:1` | `disp 0.003 0.011 0.055 0.248 0.806 -0.019 0.025` | `disp 0.003 0.011 0.055 0.248 0.8058 -0.019 0.025` |
| `intro:1` | `hp -0.019 0.016 -0.212 -1.196 0.242 -0.051 0.013` | `hp -0.019 0.016 -0.212 -1.196 0.2423 -0.051 0.013` |
| `intro:1` | `wt -4.609 1.266 -0.748 -3.641 0.001 -7.206 -2.012` | `wt -4.609 1.266 -0.748 -3.641 0.0011 -7.206 -2.012` |
| `intro:1` | `qsec 0.544 0.466 0.161 1.166 0.254 -0.413 1.501` | `qsec 0.544 0.466 0.161 1.166 0.2536 -0.413 1.501` |
| `intro:4`, `intro:5`, `variable_selection:4`, `variable_selection:5`, `variable_selection:8`, `variable_selection:10`, `variable_selection:12` | `Regression 6535804.090 5 1307160.818 34.217 0.0000` | `Regression 6535804.090 5 1307160.818 34.217 < 0.0001` |
| `intro:4`, `intro:5`, `variable_selection:4`, `variable_selection:5`, `variable_selection:8`, `variable_selection:10`, `variable_selection:12` | `(Intercept) -1178.330 208.682 -5.647 0.000 -1597.914 -758.746` | `(Intercept) -1178.330 208.682 -5.647 < 0.0001 -1597.914 -758.746` |
| `intro:4`, `intro:5`, `variable_selection:4`, `variable_selection:5`, `variable_selection:8`, `variable_selection:10`, `variable_selection:12` | `liver_test 58.064 40.144 0.156 1.446 0.155 -22.652 138.779` | `liver_test 58.064 40.144 0.156 1.446 0.1546 -22.652 138.779` |
| `intro:4`, `intro:5`, `variable_selection:4`, `variable_selection:5`, `variable_selection:8`, `variable_selection:10`, `variable_selection:12` | `alc_heavy 317.848 71.634 0.314 4.437 0.000 173.818 461.878` | `alc_heavy 317.848 71.634 0.314 4.437 < 0.0001 173.818 461.878` |
| `intro:4`, `intro:5`, `variable_selection:4`, `variable_selection:5`, `variable_selection:8`, `variable_selection:10`, `variable_selection:12` | `enzyme_test 9.748 1.656 0.521 5.887 0.000 6.419 13.077` | `enzyme_test 9.748 1.656 0.521 5.887 < 0.0001 6.419 13.077` |
| `intro:4`, `intro:5`, `variable_selection:4`, `variable_selection:5`, `variable_selection:8`, `variable_selection:10`, `variable_selection:12` | `pindex 8.924 1.808 0.380 4.935 0.000 5.288 12.559` | `pindex 8.924 1.808 0.380 4.935 < 0.0001 5.288 12.559` |
| `intro:4`, `intro:5`, `variable_selection:4`, `variable_selection:5`, `variable_selection:8`, `variable_selection:10`, `variable_selection:12` | `bcs 59.864 23.060 0.241 2.596 0.012 13.498 106.230` | `bcs 59.864 23.060 0.241 2.596 0.0125 13.498 106.230` |
| `regression_diagnostics:5` | `disp 1 808.8885 808.8885 76.51266 9.380327e-10` | `disp 1 808.8885 808.8885 76.51266 < 0.0001` |
| `regression_diagnostics:5` | `Lack of fit 25 304.2787 12.17115 4.724824 0.04563623` | `Lack of fit 25 304.2787 12.17115 4.724824 0.0456` |
| `residual_diagnostics:1` | `Kolmogorov-Smirnov 0.1152 0.7464` | `Kolmogorov-Smirnov (Lilliefors) 0.1152 0.3446` |
| `variable_selection:6`, `variable_selection:7` | `Regression 6279697.346 6 1046616.224 23.538 0.0000` | `Regression 6279697.346 6 1046616.224 23.538 < 0.0001` |
| `variable_selection:6`, `variable_selection:7` | `(Intercept) -814.092 213.222 -3.818 0.000 -1243.041 -385.144` | `(Intercept) -814.092 213.222 -3.818 0.0004 -1243.041 -385.144` |
| `variable_selection:6`, `variable_selection:7` | `age 0.458 2.706 0.013 0.169 0.866 -4.985 5.902` | `age 0.458 2.706 0.013 0.169 0.8662 -4.985 5.902` |
| `variable_selection:6`, `variable_selection:7` | `alc_mod 1.088 67.941 0.001 0.016 0.987 -135.591 137.768` | `alc_mod 1.088 67.941 0.001 0.016 0.9873 -135.591 137.768` |
| `variable_selection:6`, `variable_selection:7` | `liver_test 126.675 33.832 0.341 3.744 0.000 58.613 194.737` | `liver_test 126.675 33.832 0.341 3.744 0.0005 58.613 194.737` |
| `variable_selection:6`, `variable_selection:7` | `enzyme_test 7.523 1.543 0.402 4.874 0.000 4.418 10.628` | `enzyme_test 7.523 1.543 0.402 4.874 < 0.0001 4.418 10.628` |
| `variable_selection:6`, `variable_selection:7` | `alc_heavy 361.751 87.140 0.357 4.151 0.000 186.448 537.053` | `alc_heavy 361.751 87.140 0.357 4.151 0.0001 186.448 537.053` |
| `variable_selection:6`, `variable_selection:7` | `pindex 7.862 1.908 0.334 4.120 0.000 4.023 11.700` | `pindex 7.862 1.908 0.334 4.120 0.0002 4.023 11.700` |
| `variable_selection:13` | `Regression 6455884.265 4 1613971.066 41.327 0.0000` | `Regression 6455884.265 4 1613971.066 41.327 < 0.0001` |
| `variable_selection:13` | `(Intercept) -1334.424 180.589 -7.389 0.000 -1697.332 -971.516` | `(Intercept) -1334.424 180.589 -7.389 < 0.0001 -1697.332 -971.516` |
| `variable_selection:13` | `bcs 81.439 17.781 0.329 4.580 0.000 45.706 117.171` | `bcs 81.439 17.781 0.329 4.580 < 0.0001 45.706 117.171` |
| `variable_selection:13` | `alc_heavy 312.777 72.341 0.309 4.324 0.000 167.402 458.152` | `alc_heavy 312.777 72.341 0.309 4.324 < 0.0001 167.402 458.152` |
| `variable_selection:13` | `pindex 10.131 1.622 0.431 6.246 0.000 6.871 13.390` | `pindex 10.131 1.622 0.431 6.246 < 0.0001 6.871 13.390` |
| `variable_selection:13` | `enzyme_test 11.243 1.308 0.601 8.596 0.000 8.614 13.871` | `enzyme_test 11.243 1.308 0.601 8.596 < 0.0001 8.614 13.871` |
