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
