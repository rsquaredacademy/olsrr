weighted_mtcars_model <- function() {
  set.seed(1)
  d <- mtcars
  d$w <- runif(32, 0.5, 2)
  lm(mpg ~ disp + hp + wt, data = d, weights = w)
}

test_that("diagnostics that refit auxiliary models error on weighted models (#236)", {
  m <- weighted_mtcars_model()

  expect_error(
    ols_prep_avplot_data(m),
    "ols_prep_avplot_data\\(\\) is not available for weighted models"
  )
  expect_error(
    ols_plot_added_variable(m, print_plot = FALSE),
    "ols_plot_added_variable\\(\\) is not available for weighted models"
  )
  expect_error(
    ols_plot_comp_plus_resid(m, print_plot = FALSE),
    "ols_plot_comp_plus_resid\\(\\) is not available for weighted models"
  )
  expect_error(
    ols_plot_resid_regressor(m, "drat", print_plot = FALSE),
    "ols_plot_resid_regressor\\(\\) is not available for weighted models"
  )
  expect_error(
    ols_test_breusch_pagan(m),
    "ols_test_breusch_pagan\\(\\) is not available for weighted models"
  )
  expect_error(
    ols_test_score(m),
    "ols_test_score\\(\\) is not available for weighted models"
  )
  expect_error(
    ols_test_f(m),
    "ols_test_f\\(\\) is not available for weighted models"
  )
  expect_error(
    ols_correlations(m),
    "ols_correlations\\(\\) is not available for weighted models"
  )
})

test_that("unit weights are treated as unweighted by the diagnostics (#236)", {
  m <- lm(mpg ~ disp + hp + wt, data = mtcars, weights = rep(1, 32))

  expect_no_error(ols_prep_avplot_data(m))
  expect_no_error(ols_plot_added_variable(m, print_plot = FALSE))
  expect_no_error(ols_plot_comp_plus_resid(m, print_plot = FALSE))
  expect_no_error(ols_plot_resid_regressor(m, "drat", print_plot = FALSE))
  expect_no_error(ols_test_breusch_pagan(m))
  expect_no_error(ols_test_score(m))
  expect_no_error(ols_test_f(m))
  expect_no_error(ols_correlations(m))
})

test_that("design-matrix collinearity diagnostics remain available for weighted models", {
  m <- weighted_mtcars_model()

  expect_no_error(ols_vif_tol(m))
  expect_no_error(ols_eigen_cindex(m))
  expect_no_error(ols_coll_diag(m))
})

test_that("ols_pure_error_anova() errors on weighted models (#236)", {
  set.seed(1)
  d <- mtcars
  d$w <- runif(32, 0.5, 2)
  m <- lm(mpg ~ disp, data = d, weights = w)

  expect_error(
    ols_pure_error_anova(m),
    "ols_pure_error_anova\\(\\) is not available for weighted models"
  )

  # unit weights are still treated as unweighted
  m_unit <- lm(mpg ~ disp, data = mtcars, weights = rep(1, 32))
  expect_no_error(ols_pure_error_anova(m_unit))
})

test_that("ols_test_correlation() uses sigma and rejects weighted models (#236)", {
  m_u <- lm(mpg ~ disp + hp + wt, data = mtcars)
  expect_equal(ols_test_correlation(m_u), 0.9652493, tolerance = 1e-7)

  set.seed(1)
  d <- mtcars
  d$w <- runif(32, 0.5, 2)
  m_wt <- lm(mpg ~ disp + hp + wt, data = d, weights = w)

  expect_error(
    ols_test_correlation(m_wt),
    "ols_test_correlation\\(\\) is not available for weighted models"
  )

  m_unit <- lm(mpg ~ disp + hp + wt, data = mtcars, weights = rep(1, 32))
  expect_equal(ols_test_correlation(m_unit), ols_test_correlation(m_u))
})

test_that("weighted PRESS and predicted R-squared use the model weights (H1)", {
  d <- mtcars
  d$w <- seq_len(nrow(d)) / 8
  m   <- lm(mpg ~ wt + hp, data = d, weights = w)
  m10 <- lm(mpg ~ wt + hp, data = d, weights = 10 * w)

  h        <- hatvalues(m)
  press    <- sum(d$w * (m$residuals / (1 - h))^2)
  expected <- 1 - press / sum(anova(m)[, 2])

  expect_equal(ols_press(m), press)
  expect_equal(ols_pred_rsq(m), expected)
  expect_equal(ols_pred_rsq(m), 0.7859224, tolerance = 1e-6)

  # a constant rescaling of the weights leaves predicted R-squared unchanged
  expect_equal(ols_pred_rsq(m10), ols_pred_rsq(m))
  expect_equal(ols_press(m10), 10 * ols_press(m))

  # the summary printed by ols_regress() reports the same statistic
  expect_equal(ols_regress(m)$prsq, ols_pred_rsq(m))
})

test_that("zero prior weights are dropped consistently (H2)", {
  zero_fit <- function(pos) {
    d    <- mtcars
    d$w  <- rep(1, nrow(d))
    d$w[pos] <- 0
    list(
      fit = lm(mpg ~ wt + hp, data = d, weights = w),
      ref = lm(mpg ~ wt + hp, data = d[-pos, ])
    )
  }

  for (pos in c(1, 16, 32)) {
    z <- zero_fit(pos)

    expect_equal(ols_press(z$fit), ols_press(z$ref))
    expect_equal(ols_pred_rsq(z$fit), ols_pred_rsq(z$ref))
    expect_equal(ols_aic(z$fit), ols_aic(z$ref))
    expect_equal(ols_aic(z$fit, corrected = TRUE), ols_aic(z$ref, corrected = TRUE))
    expect_equal(ols_sbc(z$fit), ols_sbc(z$ref))
    expect_equal(ols_sbc(z$fit), BIC(z$fit))
    expect_equal(ols_regress(z$fit)$mae, ols_regress(z$ref)$mae)
  }

  # zero weights combined with na.exclude
  d <- mtcars
  d$w <- rep(1, nrow(d))
  d$w[1] <- 0
  d$mpg[2] <- NA
  me  <- lm(mpg ~ wt + hp, data = d, weights = w, na.action = na.exclude)
  mo  <- lm(mpg ~ wt + hp, data = d, weights = w, na.action = na.omit)
  ref <- lm(mpg ~ wt + hp, data = d[-(1:2), ])

  expect_equal(ols_press(me), ols_press(ref))
  expect_equal(ols_press(me), ols_press(mo))
  expect_equal(ols_pred_rsq(me), ols_pred_rsq(ref))
})

test_that("PRESS aligns leverages by model-frame row, not by row name (H2)", {
  d <- mtcars
  d$w <- 1
  d$w[1] <- 0
  d$mpg[2] <- NA
  m   <- lm(mpg ~ wt + hp, d, weights = w, na.action = na.exclude)
  ref <- lm(mpg ~ wt + hp, d[-(1:2), ])

  press <- sum((ref$residuals / (1 - hatvalues(ref))) ^ 2)

  expect_equal(ols_press(m), press, tolerance = 1e-10)
  expect_equal(ols_press(m), 236.5615444, tolerance = 1e-6)
  expect_equal(ols_pred_rsq(m), 1 - press / sum(anova(ref)[, 2]), tolerance = 1e-10)
  expect_equal(ols_pred_rsq(m), 0.7895890, tolerance = 1e-6)

  # row-name metadata cannot change the statistic: unnamed residuals
  unnamed <- m
  names(unnamed$residuals) <- NULL
  expect_equal(ols_press(unnamed), press, tolerance = 1e-10)
  expect_equal(ols_pred_rsq(unnamed), ols_pred_rsq(m))

  # and neither can repeated row names
  repeated <- m
  names(repeated$residuals) <- rep(c("a", "b"), length.out = length(repeated$residuals))
  attr(repeated$model, "row.names") <-
    rep(c("a", "b"), length.out = nrow(repeated$model))
  expect_equal(ols_press(repeated), press, tolerance = 1e-10)
  expect_equal(ols_pred_rsq(repeated), ols_pred_rsq(m))
})

test_that("leverages and PRESS ignore non-estimable QR directions (G01)", {
  m   <- lm(mpg ~ wt + I(2 * wt) + hp, mtcars)
  ref <- lm(mpg ~ wt + hp, mtcars)

  # the aliased column is not a fitted direction: the projection uses rank
  # columns only and agrees with the equivalent estimable fit
  expect_equal(m$rank, 3)
  expect_equal(olsrr:::model_hatvalues_used(m), unname(hatvalues(ref)))
  expect_equal(sum(olsrr:::model_hatvalues_used(m)), m$rank)

  press <- sum((ref$residuals / (1 - hatvalues(ref))) ^ 2)
  expect_equal(ols_press(m), press)
  expect_equal(ols_press(m), 246.5062590, tolerance = 1e-6)
  expect_equal(ols_pred_rsq(m), ols_pred_rsq(ref))
  expect_equal(ols_pred_rsq(m), 0.7810871, tolerance = 1e-6)
})

test_that("rank-deficient weighted PRESS keeps row alignment (G01)", {
  d <- mtcars
  d$w <- seq_len(32) / 8
  d$w[c(1, 16)] <- 0
  d$mpg[c(2, 32)] <- NA
  m   <- lm(mpg ~ wt + I(2 * wt) + hp, d, weights = w, na.action = na.exclude)
  ref <- lm(mpg ~ wt + hp, d[-c(1, 2, 16, 32), ], weights = w)

  press <- sum(ref$weights * (ref$residuals / (1 - hatvalues(ref))) ^ 2)

  expect_equal(m$rank, 3)
  expect_equal(sum(olsrr:::model_hatvalues_used(m)), m$rank)
  expect_equal(ols_press(m), press, tolerance = 1e-10)
  expect_equal(ols_press(m), 589.8720, tolerance = 1e-4)
  expect_equal(ols_pred_rsq(m), ols_pred_rsq(ref), tolerance = 1e-10)

  # row-name metadata remains irrelevant, including repeated names
  repeated <- m
  names(repeated$residuals) <- rep(c("a", "b"), length.out = length(repeated$residuals))
  attr(repeated$model, "row.names") <-
    rep(c("a", "b"), length.out = nrow(repeated$model))
  expect_equal(ols_press(repeated), press, tolerance = 1e-10)

  # a fit without its QR cannot give the fitted projection exactly: error
  fallback <- m
  fallback$qr <- NULL
  expect_error(ols_press(fallback), "qr = TRUE")
})

test_that("fits without a QR are rejected, never reconstructed (K01)", {
  d  <- transform(mtcars, z = wt + .001 * hp + .001 * sin(1:32))
  tv <- .1
  f  <- mpg ~ wt + z + hp
  mods <- local({
    tv <- .01
    list(lm(f, d, tol = tv), lm(f, d, tol = tv, qr = FALSE))
  })

  # stored QR: rank 3, PRESS from explicit leave-one-out fits with tol = .01
  expect_equal(mods[[1]]$rank, 3)
  loo <- vapply(seq_len(32), function(i) {
    fi <- lm(f, d[-i, ], tol = .01)
    b  <- coef(fi)
    b[is.na(b)] <- 0
    x  <- model.matrix(f, d[i, , drop = FALSE])
    d$mpg[i] - sum(x * b)
  }, numeric(1))
  expect_equal(ols_press(mods[[1]]), sum(loo ^ 2), tolerance = 1e-8)
  expect_equal(ols_press(mods[[1]]), 246.357079950998, tolerance = 1e-8)

  # no QR: clear error naming the remedy, not a different projection
  expect_error(ols_press(mods[[2]]), "refit with qr = TRUE")
  expect_error(ols_pred_rsq(mods[[2]]), "qr")

  mw <- lm(mpg ~ wt + hp, mtcars, weights = seq_len(32) / 8, qr = FALSE)
  expect_error(ols_press(mw), "refit with qr = TRUE")
})

test_that("SAS information criteria use deviance and the effective sample size (H9)", {
  d <- mtcars
  d$w <- seq_len(nrow(d)) / 8
  m <- lm(mpg ~ wt + hp, data = d, weights = w)

  n    <- nobs(m)
  sse  <- deviance(m)
  aic  <- n * log(sse / n) + 2 * m$rank
  sbc  <- n * log(sse / n) + log(n) * m$rank

  expect_equal(ols_aic(m, method = "SAS"), aic)
  expect_equal(ols_aic(m, method = "SAS", corrected = TRUE),
               n * log(sse / n) + n * (n + m$rank) / (n - m$rank - 2))
  expect_equal(ols_sbc(m, method = "SAS"), sbc)

  # na.exclude and na.omit fits are the same fit and give the same criteria
  d$mpg[1] <- NA
  me <- lm(mpg ~ wt + hp, data = d, na.action = na.exclude)
  mo <- lm(mpg ~ wt + hp, data = d, na.action = na.omit)

  for (method in c("R", "STATA", "SAS")) {
    expect_equal(ols_aic(me, method = method), ols_aic(mo, method = method))
    expect_equal(ols_sbc(me, method = method), ols_sbc(mo, method = method))
  }

  expect_equal(ols_aic(me, method = "SAS"),
               nobs(mo) * log(deviance(mo) / nobs(mo)) + 2 * mo$rank)
  expect_equal(ols_sbc(me, method = "SAS"),
               nobs(mo) * log(deviance(mo) / nobs(mo)) + log(nobs(mo)) * mo$rank)
})


test_that("ols_test_correlation() works for na.exclude fits (M5)", {  d <- mtcars
  d$mpg[1] <- NA
  me <- lm(mpg ~ disp + hp + wt, d, na.action = na.exclude)
  mo <- lm(mpg ~ disp + hp + wt, d, na.action = na.omit)

  expect_equal(ols_test_correlation(me), ols_test_correlation(mo))
})

test_that("zero-weight rows and end-of-frame omissions keep PRESS correct (H2)", {
  fit <- function(zero, missing) {
    d <- mtcars
    d$w <- 1
    d$w[zero] <- 0
    d$mpg[missing] <- NA
    list(
      fit = lm(mpg ~ wt + hp, d, weights = w, na.action = na.exclude),
      ref = lm(mpg ~ wt + hp, d[-c(zero, missing), ])
    )
  }

  # the omitted row is the last one: lm.influence() pads the reduced
  # positive-weight residuals before they can be aligned
  z <- fit(1, 32)
  press <- sum((z$ref$residuals / (1 - hatvalues(z$ref))) ^ 2)
  expect_equal(ols_press(z$fit), press, tolerance = 1e-10)
  expect_equal(ols_press(z$fit), 236.5578676, tolerance = 1e-6)
  expect_equal(ols_pred_rsq(z$fit), ols_pred_rsq(z$ref), tolerance = 1e-10)
  expect_equal(ols_regress(z$fit)$prsq, ols_pred_rsq(z$ref))

  # second-to-last row, and several zero-weight and missing rows at once
  z <- fit(31, 30)
  expect_equal(ols_press(z$fit), ols_press(z$ref), tolerance = 1e-10)
  expect_equal(ols_pred_rsq(z$fit), ols_pred_rsq(z$ref), tolerance = 1e-10)

  d <- mtcars
  d$w <- 1
  d$w[c(1, 32)] <- 0
  d$mpg[c(2, 31)] <- NA
  me  <- lm(mpg ~ wt + hp, d, weights = w, na.action = na.exclude)
  ref <- lm(mpg ~ wt + hp, d[-c(1, 2, 31, 32), ])
  expect_equal(ols_press(me), ols_press(ref), tolerance = 1e-10)
  expect_equal(ols_pred_rsq(me), ols_pred_rsq(ref), tolerance = 1e-10)
})

