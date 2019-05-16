context("Visual Test")

model <- lm(mpg ~ disp + hp + wt, data = mtcars)

test_that("residual histogram plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_hist(model)
  vdiffr::expect_doppelganger("ggplot2 histogram", p)
})

test_that("hadi plot is as expected", {
  skip_on_cran()
  p <- ols_plot_hadi(model)
  vdiffr::expect_doppelganger("hadi plot", p)
})

test_that("observed vs predicted plot is as expected", {
  skip_on_cran()
  p <- ols_plot_obs_fit(model)
  vdiffr::expect_doppelganger("ovsp plot", p)
})

test_that("potential residual plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_pot(model)
  vdiffr::expect_doppelganger("potential residual plot", p)
})

test_that("residual box plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_box(model)
  vdiffr::expect_doppelganger("residual box plot", p)
})

test_that("residual fit spread plot 1 is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_spread(model)
  vdiffr::expect_doppelganger("residual fit spread plot", p)
})

test_that("residual fit spread plot 2 is as expected", {
  skip_on_cran()
  p <- ols_plot_fm(model)
  vdiffr::expect_doppelganger("residual fit spread plot 2", p)
})

test_that("residual qq plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_qq(model)
  vdiffr::expect_doppelganger("residual qq plot", p)
})

test_that("residual vs fitted plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_fit(model)
  vdiffr::expect_doppelganger("residual vs fitted plot", p)
})

test_that("residual vs regressor plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_regressor(model, drat)
  vdiffr::expect_doppelganger("residual vs regressor plot", p)
})

test_that("cooks d bar plot is as expected", {
  skip_on_cran()
  p <- ols_plot_cooksd_bar(model)
  vdiffr::expect_doppelganger("cooks d bar plot", p)
})

test_that("cooks d bar chart is as expected", {
  skip_on_cran()
  p <- ols_plot_cooksd_chart(model)
  vdiffr::expect_doppelganger("cooks d bar chart", p)
})

test_that("dffits plot is as expected", {
  skip_on_cran()
  p <- ols_plot_dffits(model)
  vdiffr::expect_doppelganger("dffits plot", p)
})

test_that("deleted studentized residual vs fitted plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_stud_fit(model)
  vdiffr::expect_doppelganger("dsrvsp plot", p)
})

test_that("residual vs regressor shiny plot is as expected", {
  skip_on_cran()
  p <- rvsr_plot_shiny(model, mtcars, "drat")
  vdiffr::expect_doppelganger("residual vs regressor shiny plot", p)
})

test_that("residual fit spread plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_fit_spread(model)
  vdiffr::expect_doppelganger("fm_plot", p$fm_plot)
  vdiffr::expect_doppelganger("rsd_plot", p$rsd_plot)
})

test_that("response profile plot is as expected", {
  skip_on_cran()
  p <- ols_plot_response(model)
  vdiffr::expect_doppelganger("resp viz dot plot", p$dot_plot)
  vdiffr::expect_doppelganger("resp viz trend plot", p$trend_plot)
  vdiffr::expect_doppelganger("resp viz histogram", p$histogram)
  vdiffr::expect_doppelganger("resp viz boxplot", p$boxplot)
})

test_that("stepAIC backward regression plot is as expected", {
  skip_on_cran()
  model <- lm(y ~ ., data = surgical)
  p <- plot(ols_step_backward_aic(model))
  vdiffr::expect_doppelganger("stepaic backward regression plot", p)
})

test_that("stepAIC forward regression plot is as expected", {
  skip_on_cran()
  model <- lm(y ~ ., data = surgical)
  p <- plot(ols_step_forward_aic(model))
  vdiffr::expect_doppelganger("stepaic forward regression plot", p)
})

test_that("stepAIC both direction regression plot is as expected", {
  skip_on_cran()
  model <- lm(y ~ ., data = surgical)
  p <- plot(ols_step_both_aic(model))
  vdiffr::expect_doppelganger("stepaic both regression plot", p)
})

