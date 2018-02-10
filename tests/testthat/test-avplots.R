context("added variable plots")

test_that("added variable plot is as expected", {
  skip_on_cran()

  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  k <- olsrr::ols_avplots(model)

  vdiffr::expect_doppelganger("avplot disp", k$plots[[1]])
  vdiffr::expect_doppelganger("avplot hp", k$plots[[2]])
  vdiffr::expect_doppelganger("avplot wt", k$plots[[3]])
})
