# pkg-ofx14: platform-stable stepwise ties

Baseline: `373993c` on `task/olsrr-fix-071`. Platform: Windows 11 x64
(build 26300), R 4.6.1 (2026-06-24 ucrt), reference BLAS/LAPACK.
Scope: M1-M4, L1, L2 (documentation only), and L4 from
`docs-dev/ofxx13-audit.md`. L3 is intentionally unchanged.

## Findings and verification

| Finding | Regression coverage | Fix | Verification | Status |
| --- | --- | --- | --- | --- |
| L1 | `test-step-ties-round14.R:1`: noisy F tie, p tie, and the audit's factor/one-df fixture; the existing removal-helper pin now expects the larger p | `R/ols-stepwise-utils.R:841`: order removal by F ascending, rounded p descending, candidate order | New test failed before the fix (2 failures); copied p15 probe now removes `x, f` | Fixed first, commit `1af9727` |
| M1 | `test-step-ties-round14.R:24`: inject last-bit criterion differences into all five backward selectors, at both initial and subsequent removals, with character and numeric locked terms | `R/ols-step.R:405`: choose from eligible original indices through `ols_threshold()`; use `preds[minc]` for the recorded term | 40 assertions fail against the baseline; all pass after the fix; p21 now selects u1 in 100/100 runs | Fixed, commit `df48182` |
| M2 | `test-step-ties-round14.R:83`: audit R-squared pair and nearby IC/R-squared pairs; test both improvement helpers, exact ties, last-bit improvements, worse values, and real improvements | `R/ols-stepwise-utils.R:866,878`: compare ten-digit keys on both sides | 12 assertions fail against baseline; p10 and every p20 nudge now have one selection result | Fixed, commit `df48182` |
| M4 | `test-step-ties-round14.R:24,64,83`: distinct F literals, AIC gaps of 3e-14, p gaps of 3e-17, R-squared gaps of two ulps; selection identities stay exact | Deterministic helper tests plus injected metrics in exported backward selectors | Removing `signif(x, 10)` in an isolated baseline copy makes eight of the dedicated rounding assertions fail, even on x86 | Fixed, commit `df48182` |
| L4 | `test-step-selection-round11.R:94` | Replace only the two F/AIC `expect_identical()` assertions with `expect_equal(tolerance = 1e-12)` | Focused tests pass; term identity assertions remain exact | Fixed, commit `df48182` |
| M3 | NEWS and generated Rd reviewed against the selection and improvement code | `NEWS.md:7` accurately scopes ten-digit comparisons and explicitly preserves raw p cutoffs | Documentation regenerated with `devtools::document()` | Fixed, commit `9b9b058` |
| L2 | Documentation only; the ten-digit window itself is unchanged | `R/ols-regression.R:32`: one sentence in the existing shared `@details`, inherited by all 18 non-hierarchical stepwise selector topics | Generated Rd contains the precision rule, tie directions, and no-improvement rule | Documented, commit `9b9b058` |

The backward loop now filters `include` before ranking, retaining the original
candidate order and mapping the selected eligible index back to the full metric
vector. Both raw sorting blocks used only for selection were removed. The
`ols_sort_metrics()` display path and table-printing functions are unchanged.
Both first-step and later-step callers already use the two improvement helpers;
they therefore receive the same rounded comparison without another comparison
at the call sites. Raw metric values remain in histories and printed tables.

P-value cutoff comparisons (audit coverage rows 6-9) remain unchanged as
requested. They test eligibility against the user's explicit entry/removal
cutoff, rather than rank candidate statistics or measure criterion improvement.
Rounding those comparisons would change that cutoff policy, including fixed-order
hierarchical selection. No such policy change is included here.

## Audit probes: before and after

The original scripts were copied into `.ofx14-tmp/` and run there; originals
were not edited. The extra copied p15 script redirects only its saved fixture
from the original scratch folder to `.ofx14-tmp/factor-tie3.rds`.
Logs are `.ofx14-tmp/{before,after}-<probe>.R.log`.

| Copied probe | Baseline `373993c` | After fix |
| --- | --- | --- |
| p15-factor3 | F(f)=2.5000008968415628, F(x)=2.500000896841279; p(f)=0.0913722, p(x)=0.119581; removal `f, x` | Same statistics, removal `x, f`, matching a3d1669 |
| p10-rsq3 forward_r2, 40 nudges | `x1,x2`: 37; `x1,x2,x4`: 3 (2 results) | `x1,x2`: 40 (1 result) |
| p10-rsq3 both_r2, 40 nudges | `x1,x2`: 31; `x1,x2,x4`: 9 (2 results) | `x1,x2`: 40 (1 result) |
| p10-rsq3 forward_adj, 40 nudges | `x1,x2`: 40 | `x1,x2`: 40 |
| p10-rsq3 forward_aic, 40 nudges | `x1`: 40 | `x1`: 40 |
| p20-backward-flip, delta=2e-16, 100 nudges | remove a: 35; none: 65 (2 results) | none: 100 (1 result) |
| p20-backward-flip, delta=5e-16, 100 nudges | remove a: 17; none: 83 (2 results) | none: 100 (1 result) |
| p20-backward-flip, delta=1e-15, 100 nudges | remove a: 44; none: 56 (2 results) | none: 100 (1 result) |
| p20-backward-flip, delta=1e-14, 100 nudges | remove a: 87; none: 13 (2 results) | none: 100 (1 result) |
| p21-backward-term, 100 nudges | first removal u1: 58; u2: 42 (2 results) | first removal u1: 100 (1 result) |

In p21 the unrounded diagnostic `which.min()` still flips 58/42, and p20's
unrounded AIC differences still have both signs. These are intentionally retained
diagnostics, not selectors. They confirm the input perturbations still exercise
the defect while the package's selection decisions are stable.

## Baseline comparison and checks

`NOT_CRAN=true` was set for the full `devtools::test()` runs.

```text
Baseline: files 51; tests 406; passed 3353; failed 0; errors 0; warnings 0; skipped 0
Final: files 52; tests 410; passed 3458; failed 0; errors 0; warnings 0; skipped 0
Original test-result summaries: all 406 rows identical (counts and outcome flags)
Audit battery: 159 calls; differences 0
Documented stepwise examples: 18 help pages; 150 selections; metrics, terms, coefficients identical
Preliminary clean archive build: OK
Preliminary R CMD check: 0 errors, 0 warnings, 2 NOTEs (maintainer and a 5.01-second example timing)
Isolated R CMD check: 0 errors, 0 warnings, 1 NOTE (maintainer only)
```

The 159-call comparison uses the audit's own `dump.R` on surgical, stepdata,
auto, cement, fitness, rivers, and mtcars, including forward/backward/both
criteria, p selection, hierarchical selection, and include/exclude/steps
variants. The full recorded metric histories are identical to `373993c`.

The additional example comparison extracts the actual examples from all 18
non-hierarchical stepwise help pages, runs
them on each tree, and compares every captured selection's metrics, final
terms, and coefficients exactly. Plots and logs are kept only in scratch.

The only observed behavior differences against `373993c` are the intended
near-tie cases listed in the probe table and the synthetic regression tests.
The existing removal-helper test changes its expected index from 2 to 1 for
F=(2,2,4), p=(.4,.3,.01), restoring less-significant-first removal. No
snapshots were changed. The two scalar F/AIC comparisons use tolerance without
changing any term-selection assertion.

The preliminary check ran while the comparison jobs were active. Its extra
timing NOTE was for the unchanged `ols_plot_diagnostics` example:

```text
Examples with CPU (user + system) or elapsed time > 5s
                     user system elapsed
ols_plot_diagnostics  4.8   0.12    5.01
```

This preliminary timing NOTE is not treated as acceptance evidence. The same
clean-archive tarball is checked again with no competing jobs from this task.
Both standard examples and `--run-donttest` examples passed the preliminary
check; installed-package tests passed 3248 assertions, with 83 CRAN skips,
zero failures and zero warnings. The development run above has zero skips.

The isolated run completed successfully, with no example-timing NOTE:

```text
* checking CRAN incoming feasibility ... [113s] NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'

New maintainer:
  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s):
  Aravind Hebbali <hebbali.aravind@gmail.com>
* checking examples ... [60s] OK
* checking examples with --run-donttest ... [174s] OK
* checking tests ... [276s] OK
  Running 'testthat.R' [276s]
* checking re-building of vignette outputs ... [63s] OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: 1 NOTE

Installed tests: [ FAIL 0 | WARN 0 | SKIP 83 | PASS 3248 ]
```

Full output is in `.ofx14-tmp/solo/check.log` and
`.ofx14-tmp/solo/olsrr.Rcheck/00check.log`. The unchanged maintainer NOTE
records the email change already present in the baseline; it is the same
maintainer. No timing NOTE is accepted on the basis of an unrun win-builder
check.

## Files changed

- `R/ols-stepwise-utils.R`: removal p direction, precise helper comment, both improvement comparisons.
- `R/ols-step.R`: backward removal selection through rounded eligible metrics.
- `tests/testthat/test-step-ties-round14.R`: four regression tests, 105 assertions.
- `tests/testthat/test-step-selection-round11.R`: corrected removal-helper expectation and two tolerance checks.
- `NEWS.md`: corrected existing P03 bullet, covering M1-M4 and L1-L2.
- `R/ols-regression.R`: one shared precision sentence in the existing inherited details.
- Generated `man/ols_regress.Rd`, `man/ols_step_all_possible.Rd`, and `man/ols_step_best_subset.Rd`, plus the 18 `man/ols_step_{forward,backward,both}_{aic,sbc,sbic,r2,adj_r2,p}.Rd` files: inherited precision sentence, qualified as non-hierarchical stepwise selection; no change to all-subsets/best-subsets behavior.
- `docs-dev/ofx14-report.md`: this report.

## Commands and artifacts

All heavy R commands were run through
`bash ~/.claude/bin/heavy-slot.sh pkg-ofx14 <command>`, with `LC_ALL=C` after
the first probe run. The wrapper reports that pkg-ofx14 already owns its
queue job's heavy slot. Startup locale warnings from the first probe run
are shell-environment warnings; subsequent runs used the C locale.

Executed commands and scratch drivers:

```text
git archive -o .ofx14-tmp/base.tar 373993c
tar -xf .ofx14-tmp/base.tar -C .ofx14-tmp/base
Copy-Item .ofxx13-tmp/p10-rsq3.R,.ofxx13-tmp/p20-*.R,.ofxx13-tmp/p21-backward-term.R,.ofxx13-tmp/p15-factor3.R,.ofxx13-tmp/dump.R,.ofxx13-tmp/counts.R -Destination .ofx14-tmp/
Rscript --vanilla .ofx14-tmp/probes.R before
devtools::test(filter = "step-ties-round14", reporter = "summary") # before L1: 2 failures; after L1: 6 passes
devtools::test(filter = "step-ties-round14", reporter = "summary") # before M1/M2: expected regression failures
Rscript --vanilla .ofx14-tmp/focused.R fixed
Rscript --vanilla .ofx14-tmp/focused.R fixed-final
Rscript --vanilla .ofx14-tmp/probes.R after
Rscript --vanilla .ofx14-tmp/suite.R .ofx14-tmp/base baseline
Rscript --vanilla .ofx14-tmp/suite.R . final
Rscript --vanilla .ofx14-tmp/dump.R .ofx14-tmp/base .ofx14-tmp/baseline.csv
Rscript --vanilla .ofx14-tmp/dump.R . .ofx14-tmp/final.csv
Rscript --vanilla .ofx14-tmp/examples.R .ofx14-tmp/base baseline
Rscript --vanilla .ofx14-tmp/examples.R . final
Rscript --vanilla .ofx14-tmp/compare.R
Copy-Item tests/testthat/test-step-ties-round14.R .ofx14-tmp/base/tests/testthat/
Rscript --vanilla .ofx14-tmp/regressions.R
Rscript --vanilla -e "devtools::document()"
git diff --check
git archive -o .ofx14-tmp/precheck.tar HEAD
tar -xf .ofx14-tmp/precheck.tar -C .ofx14-tmp/precheck/src
Rscript --vanilla .ofx14-tmp/check.R precheck
Copy-Item .ofx14-tmp/precheck/olsrr_0.7.1.tar.gz .ofx14-tmp/solo/olsrr_0.7.1.tar.gz
Rscript --vanilla .ofx14-tmp/check-only.R
```

`suite.R` calls `devtools::test(reporter = "silent")`, saves the complete
test result data frame, prints counts, and exits unsuccessfully on test
failures/errors. `regressions.R` runs only the new test file against the
isolated baseline copy, then temporarily replaces its `signif(x, 10)` with
`x` to verify that rounding coverage fails; it restores that scratch source.
The original baseline full-suite run was completed before the new test file
was copied into that isolated tree. `check.R` runs `R CMD build src`, then
`R CMD check --as-cran --no-manual olsrr_0.7.1.tar.gz`, stopping on a nonzero
exit code. An initial scratch driver quoted the R executable twice and
failed before building; that invocation was corrected and rerun. An inline
filter command containing `|` also failed at shell parsing and was replaced
with `focused.R`. Neither failure changed package files.
The first example-driver run saved all 150 selections but ended with a trailing
parse error after its scratch script was edited while active; it was rerun with
the script held fixed. Hierarchical selection is internal (`@noRd`), and its
coverage is in the 159-call comparison rather than a separate help page.

The final report commit adds this build-excluded report and clarifies one
regression-test comment; it changes no implementation or assertion. After that
last commit, the following fresh-archive build/check validates the committed
tree again. The exact hash is saved in `.ofx14-tmp/final/commit.txt`, and the
observed result of that final run is stated in the completion message. Logs
are `.ofx14-tmp/final/build.log`, `check.log`, and
`olsrr.Rcheck/00check.log`.

```text
git add docs-dev/ofx14-report.md tests/testthat/test-step-ties-round14.R
git commit -m "docs: record M1-M4 L1 L2 L4 evidence and checks (ofx14)"
git rev-parse HEAD | Set-Content -Encoding ascii .ofx14-tmp/final/commit.txt
git archive -o .ofx14-tmp/final/source.tar HEAD
tar -xf .ofx14-tmp/final/source.tar -C .ofx14-tmp/final/src
bash -c 'LC_ALL=C bash ~/.claude/bin/heavy-slot.sh pkg-ofx14 Rscript --vanilla .ofx14-tmp/check.R final'
```

## Deferred and follow-ups

- L3: raw ordering in candidate display tables remains unchanged, as requested.
- P-value threshold boundary rounding is outside this fix: exact user cutoffs remain in force, as requested.
- All-subsets and best-subsets ranking are outside the requested stepwise scope; no selection code there was changed.
- macOS arm64 was not available locally. The deterministic last-bit tests exercise the mechanism on Windows; a native macOS CI run remains a release-platform follow-up.
- Win-builder and R-hub submission were not performed. No package was pushed or submitted; no GitHub commands were used, no branch was switched, and no other worktree was accessed.
- Original audit Markdown files, original `.ofx*` scratch folders, and `.olrep-tmp` were left untouched. New scratch artifacts are confined to `.ofx14-tmp/`.
