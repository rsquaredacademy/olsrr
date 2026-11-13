# ofx6 - olsrr fix round 6 (Claude Sonnet)

Base `c63ab36` (task/olsrr-fix-071). Findings from `ofxx5-reaudit.md`: L01, L02 (both High, inherited).

| Finding | Test added | Fix | Verified how | Status |
|---|---|---|---|---|
| L01 - `ols_regress()` silently refits via `update()` | `test-regress.R` "rejects fits without a model frame or QR instead of refitting (L01)" (audit's separate-environment `tv` case, changed-data case, `model = FALSE`, `qr = FALSE`); "ordinary fits are unchanged" | `R/ols-regression.R` (`ols_regress.lm`): errors "refit with lm(..., qr = TRUE, model = TRUE)" before any computation; the `update()`/`eval()` refit is removed | Stored-QR fit gives prsq 0.781219577042816, rsq 0.827030132715933, mse 6.71628388198179; weighted ordinary fit equals `summary()` values. grep of `R/` found no other `update()` on user models; the other `eval(call$data)` uses (`ols_model_data()`, selection routines) build new candidate fits, not a replacement of the supplied fit. | Fixed |
| L02 - PRESS finite at unit leverage | `test-weighted-diagnostics.R` "PRESS and predicted R-squared are NA at unit leverage (L02)" (numeric indicator, singleton factor, weighted, omitted row, three row permutations, ordinary fit unchanged) | `R/ols-influence-measures.R`: `model_press_value()` returns NA with a warning naming the rows when any used row has leverage > 1 - 10 eps; used by `ols_press()` and `ols_pred_rsq()` | `ols_regress()$prsq` and `ols_model_performance()$prsq` are NA (present, not 0); ordinary PRESS equals the explicit formula, no warning | Fixed |

User-visible: `ols_regress()` on `qr = FALSE`/`model = FALSE` fits now errors (NEWS). PRESS/predicted R-squared NA + warning at unit leverage (NEWS).

Deferred: none new. Checks below.
