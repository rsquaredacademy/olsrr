context("best_subsets")

test_that("best subsets selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_best_subset(model)
  pred_exp <- c("x4", "x1 x2", "x1 x2 x4", "x1 x2 x3 x4")
  expect_equal(k$mindex, c(1, 2, 3, 4))
  expect_equivalent(k$predictors, pred_exp)
})

test_that("all_subsets fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_best_subset(m), "Please specify a OLS linear regression model.")
})

test_that("all_subsets returns an error when number of predictors < 2", {
  model <- lm(y ~ x1, data = cement)
  expect_error(ols_best_subset(model), "Please specify a model with at least 2 predictors.")
})

test_that("best subsets regression plots are as expected", {
  skip_on_cran()

  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- plot(ols_best_subset(model))

  vdiffr::expect_doppelganger("best subset rsquare", k$rsquare_plot)
  vdiffr::expect_doppelganger("best subset adjusted rsquare", k$adj_rsquare_plot)
  vdiffr::expect_doppelganger("best subset mallows cp", k$mallows_cp_plot)
  vdiffr::expect_doppelganger("best subset aic", k$aic_plot)
  vdiffr::expect_doppelganger("best subset sbic", k$sbic_plot)
  vdiffr::expect_doppelganger("best subset sbc", k$sbc_plot)
})
