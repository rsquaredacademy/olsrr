# pkg-olsrr-c25-aud — audit of the pkg-olsrr-c25 build

2026-11-18. Worktree `F:/R/Packages/_worktrees/olsrr-c25`, branch `task/olsrr-c25`.
Audited `git diff origin/develop..HEAD` = one commit `355c9bf` ("Lilliefors KS
p-value and distinct diagnostic panel titles (#254, #253)") on top of
`origin/develop` `4283adb`. Brief:
`F:/pm/notes/2026-11-15-day/briefs/pkg-olsrr-c25-brief.md`. Builder report:
`docs-dev/olsrr-c25-report.md` (see I5 for the file name). R 4.6.1, Windows.
Read and verify only: no package code changed, nothing pushed, no branch switch.

Documented domains used for the stop rule:
`ols_test_normality()` — a numeric vector or an `lm` fit; each test runs inside
its documented sample-size range (Shapiro-Wilk 3–5000, Lilliefors > 4,
AD/CvM >= 8), tests outside their range are skipped with a warning, and the
function stops if every test is skipped. `ols_plot_diagnostics()` /
`ols_plot_resid_fit_spread()` / `ols_plot_fm()` / `ols_plot_resid_spread()` —
an `lm` fit.

## Findings by severity

### High

None.

### Medium

None.

### Low

**L1. NEWS does not mention that the stand-alone residual fit spread plots changed title too.**
The fix is in the shared helper `ols_plot_rfs()` (`R/ols-residual-fit-spread-plot.R:104`,
`ggtitle(y_lab)`), so the title changes in `ols_plot_fm()`,
`ols_plot_resid_spread()` and both panels of `ols_plot_resid_fit_spread()`, and
in panels 7/8 of `ols_plot_diagnostics()`. The brief asks for titles that match
`ols_plot_resid_fit_spread()`'s panels, but on `origin/develop` both of those
panels were also titled "Residual Fit Spread Plot". So the change to the helper is
reasonable: it brings back the pre-refactor titles. I confirmed in `git show bb5ec0b`
that the old code had `ggtitle("Fit - Mean")` and `ggtitle("Residual")`.
`man/ols_plot_resid_fit_spread.Rd` documents the new titles. The NEWS bullet only
names `ols_plot_diagnostics()`.

```r
m <- lm(mpg ~ disp + hp + wt, mtcars)
ols_plot_fm(m, print_plot = FALSE)$labels$title
# origin/develop: "Residual Fit Spread Plot"    HEAD: "Fit - Mean"
ols_plot_resid_spread(m, print_plot = FALSE)$labels$title
# origin/develop: "Residual Fit Spread Plot"    HEAD: "Residual"
```

Expected: NEWS names every function whose output changed. Actual: only
`ols_plot_diagnostics()` is named. This is cosmetic: plot data, axis labels and
the combined figure's "Residual Fit Spread Plot" banner are unchanged (checked
for 4 models, see "Passing checks" item 2).

**L2. Constant input gives a less helpful error message (outside the domain).**
`nortest::lillie.test()` runs first and fails on zero variance with an internal
`if (NA)`. Before the fix, `shapiro.test()` raised the error.

```r
ols_test_normality(rep(3, 10))
# origin/develop: Error: all 'x' values are identical   (plus a ks.test ties warning)
# HEAD:           Error in if (pvalue > 0.1) ...: missing value where TRUE/FALSE needed
```

Expected: an error that says what is wrong with the input. Actual: an error
from inside `lillie.test()`. A constant sample has no defined normality test,
so both versions are right to error. Only the message is worse, and no number is
wrong. A short guard (`if (sd(y) == 0) stop(...)`) would fix it. This is a
follow-up, not a blocker.

### Info (no action required)

- **I1. n = 3 or 4: the KS row is now skipped.** Before the fix it held an uncorrected
  `ks.test()` p-value. Now it is `NA`, with the package's standard skip warning
  (`Kolmogorov-Smirnov (Lilliefors) test skipped: it requires at least 5
  observations (n = 4).`). This matches `nortest::lillie.test()`'s own limit
  (`n > 4`) and is documented in `?ols_test_normality` and NEWS. Shapiro-Wilk
  still runs, so no valid input is newly rejected: inputs that worked before
  still work, and the all-skipped error still starts at n < 3. Checked on
  `norm3/4`, `exp3/4`, `na4` (4 non-NA values plus 1 NA) and
  `lm(mpg ~ disp, mtcars[1:4, ])`.
- **I2.** `ks.test()`'s "ties should not be present" warning no longer appears
  for tied data (`round(rnorm(40), 1)`, integer data, constant data), because
  `lillie.test()` does not warn about ties. This is benign.
- **I3.** The KS statistic matches `origin/develop` to within floating-point
  error. The largest relative difference across 57 inputs is 1.03e-14
  (`stepdata`, n = 20000). `lillie.test()` computes the same D with a different
  operation order. The NEWS claim "the statistic is unchanged" holds.
- **I4.** 42 of the 106 vdiffr SVGs in `tests/testthat/_snaps/visual/` are 0 bytes
  on `origin/develop`, including `fm-plot.svg`, `rsd-plot.svg` and
  `residual-fit-spread-plot*.svg`, so the stand-alone fit spread plots are not
  checked visually. The build did not change this. The new
  `test-diagnostics-panel-titles.R` covers their titles. The builder lists it as
  follow-up 1.
- **I5.** The builder report is committed as `docs-dev/olsrr-c25-report.md`, not
  `docs-dev/pkg-olsrr-c25-report.md` (the brief's `<id>` is `pkg-olsrr-c25`).
  This is a naming issue only.
- **I6.** The builder reported `R CMD check --as-cran` with 1 WARNING:
  "Insufficient package version (0.7.1 = CRAN)". The brief says not to bump the
  version, and the same tarball gave `Status: OK` with
  `_R_CHECK_CRAN_INCOMING_=false`. I did not re-run `R CMD check`; it was not
  part of this audit's checks.

## Passing checks

1. **The issues in the brief are fixed (reprexes re-run on both trees).**

   `#254`, `lm(mpg ~ disp + hp + wt, mtcars)`:

   | | origin/develop | HEAD | `nortest::lillie.test(residuals(m))` |
   |---|---|---|---|
   | statistic | 0.108265447892069 | 0.108265447892069 | 0.108265447892069 |
   | p-value | 0.808868677941475 | **0.443473239076533** | 0.443473239076533 |
   | method | Exact one-sample Kolmogorov-Smirnov test | Lilliefors (Kolmogorov-Smirnov) normality test | — |

   Printed label on HEAD: `Kolmogorov-Smirnov (Lilliefors)        0.1083         0.4435`.
   `nortest` is in `DESCRIPTION` `Imports:`. `NAMESPACE` imports
   `nortest::lillie.test` and no longer imports `stats::ks.test`.

   `#253`, `ols_plot_diagnostics(m, print_plot = FALSE)` titles of panels 7/8:
   origin/develop `"Residual Fit Spread Plot"` / `"Residual Fit Spread Plot"`
   (identical). HEAD `"Fit - Mean"` / `"Residual"` (different). These match
   `ols_plot_resid_fit_spread()`'s `fm_plot` / `rsd_plot` titles on HEAD. The
   other 8 panel titles are unchanged.

2. **Ordinary inputs give the same numbers as origin/develop.** I ran a grid
   (`grid.R`) on both trees in separate Rscript processes and compared the
   results (`compare.R`, `compare2.R`). The grid has 57 `ols_test_normality()`
   inputs:
   - 16 `lm` fits: mtcars ×5 variants including factor and weighted, plus
     surgical, stepdata (n = 20000), auto, cement, hsb and fitness, and mtcars
     subsets with n = 4, 5, 6, 9.
   - 41 vectors: normal and exponential samples with n = 3–10, 20, 50, 100, 101,
     500, 5000, 5001; ties; integers; NAs; n = 1, 2; a constant vector; large and
     tiny scales; a character vector.

   Results:
   - Shapiro-Wilk, Cramér-von Mises and Anderson-Darling statistics and p-values
     are **identical** for every input.
   - The KS statistic is equal to within 1.03e-14 relative wherever both trees
     compute it.
   - The KS p-value on HEAD **equals `nortest::lillie.test()`** for every input
     where it is computed. It is lower than the old uncorrected p-value in every
     case, which is the intended change.
   - Errors are unchanged: n <= 2, character input and the constant-vector
     error, apart from that message (L2).
   - Warnings change only as described in I1/I2.

   The plots, built for 4 models: every layer's data in all 10
   `ols_plot_diagnostics()` panels, in `ols_plot_fm()`,
   `ols_plot_resid_spread()` and both `ols_plot_resid_fit_spread()` panels, is
   **identical**. Every label is identical except `title` on the
   fit-mean/residual-spread plots (L1).
3. **New errors don't reject valid inputs.** The build adds no new `stop()`. The
   only new restriction is the KS skip at n < 5. It is a skip with a warning,
   not an error, and matches `lillie.test()`'s supported range (I1).
4. **The tests are meaningful.** I copied HEAD's `tests/testthat` to
   `%TEMP%/olsrr-c25-aud-tests` and ran it against the `origin/develop` code
   (`pkgload::load_all` on the extracted base, `NOT_CRAN=true`, filter
   `normality|norm-output|golden|diagnostics-panel-titles|visual`). Result:
   **10 failing tests**:
   - both tests in `test-diagnostics-panel-titles.R` (5 + 4 expectations)
   - `golden: ols_test_normality()` (3)
   - the `norm-output` snapshot
   - all 5 changed or new `test-normality.R` tests (1, 5, 4, 2, 1)
   - `test-visual.R` "diagnostics panel is as expected" (2 expectations: the
     `diag-7`/`diag-8` doppelgangers)

   testthat stopped reporting at its 10-failure limit. All of these pass on
   HEAD. The `.new` snapshot files were written only to the temp copy; the
   worktree stayed clean.
5. **Docs and NEWS match the behaviour.**
   - `?ols_test_normality` says `lillie.test()` "requires more than 4", which
     matches the `min = 5L` code. It explains why the uncorrected p-value is too
     large, and the `kolmogorv` return item is relabelled.
   - The vignette `residual_diagnostics.Rmd` adds a sentence. It renders the
     table live, so the table updates by itself.
   - `?ols_plot_resid_fit_spread` names the two panel titles.
   - NEWS: the "p-value is smaller" and "statistic unchanged" claims and the
     n < 5 skip claim all hold on the grid. NEWS has the
     `# olsrr (development version)` heading. The version stays 0.7.1, as the
     brief says.
   - Gaps: L1, and the generated `docs/` site is not rebuilt (the builder's
     follow-up 3, expected).
6. **`devtools::test()` on HEAD:** `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3613 ]`,
   57 files, 435 tests, 972.6 s. `git status` stayed clean afterwards.

## Commands run

```powershell
Get-Content F:/pm/notes/2026-11-15-day/briefs/pkg-olsrr-c25-brief.md
git status; git log --oneline origin/develop..HEAD; git diff --stat origin/develop..HEAD
git diff origin/develop..HEAD -- . ':!docs-dev' ':!tests/testthat/_snaps/visual'
git diff origin/develop..HEAD -- tests/testthat/_snaps/visual
git log -G'ggtitle\(...' -- R/; git show bb5ec0b -- R/ols-residual-fit-spread-plot.R
git cat-file -s origin/develop:tests/testthat/_snaps/visual/fm-plot.svg   # 0 bytes
# base tree (a piped `git archive | tar -x` got mangled by PowerShell's binary pipe, so I used -o)
git archive --format=tar -o $env:TEMP/olsrr-c25-aud-base.tar origin/develop
tar -xf $env:TEMP/olsrr-c25-aud-base.tar -C $env:TEMP/olsrr-c25-aud-base
Rscript scratch/reprex.R <base>; Rscript scratch/reprex.R <worktree>
Rscript scratch/grid.R <base> grid_base.rds; Rscript scratch/grid.R <worktree> grid_head.rds
Rscript scratch/compare.R; Rscript scratch/compare2.R
Copy-Item -Recurse tests/testthat $env:TEMP/olsrr-c25-aud-tests
Rscript scratch/newtests_on_old2.R <base> $env:TEMP/olsrr-c25-aud-tests   # HEAD tests vs old code
Rscript scratch/run_tests.R      # devtools::test() on HEAD
```

The scratch scripts and logs are in
`C:/Users/DELL/.gemini/antigravity-cli/brain/7c01db4f-db1e-48e3-91ab-56b4403b0ac7/scratch/`
(not in the repository). The scripts load each tree with `pkgload::load_all()`.
Nothing was installed and no worktree was created.

Any wrong result left inside the domain: no
