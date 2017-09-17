context("Visual Test")

model <- lm(mpg ~ disp + hp + wt, data = mtcars)

test_that('residual histogram plot is as expected', {

  skip_on_cran()
  p <- ols_rsd_hist(model)
  vdiffr::expect_doppelganger('ggplot2 histogram', p$plot)

})

test_that('hadi plot is as expected', {

  skip_on_cran()
  p <- ols_hadi_plot(model)
  vdiffr::expect_doppelganger('hadi plot', p$plot)

})

test_that('observed vs predicted plot is as expected', {

  skip_on_cran()
  p <- ols_ovsp_plot(model)
  vdiffr::expect_doppelganger('ovsp plot', p$plot)

})

test_that('potential residual plot is as expected', {

  skip_on_cran()
  p <- ols_potrsd_plot(model)
  vdiffr::expect_doppelganger('potential residual plot', p$plot)

})

test_that('residual box plot is as expected', {

  skip_on_cran()
  p <- ols_rsd_boxplot(model)
  vdiffr::expect_doppelganger('residual box plot', p$plot)

})

test_that('residual fit spread plot 1 is as expected', {

  skip_on_cran()
  p <- ols_rsd_plot(model)
  vdiffr::expect_doppelganger('residual fit spread plot', p$plot)

})

test_that('residual fit spread plot 2 is as expected', {

  skip_on_cran()
  p <- ols_fm_plot(model)
  vdiffr::expect_doppelganger('residual fit spread plot 2', p$plot)

})

test_that('residual qq plot is as expected', {

  skip_on_cran()
  p <- ols_rsd_qqplot(model)
  vdiffr::expect_doppelganger('residual qq plot', p$plot)

})

test_that('residual vs fitted plot is as expected', {

  skip_on_cran()
  p <- ols_rvsp_plot(model)
  vdiffr::expect_doppelganger('residual vs fitted plot', p$plot)

})

test_that('residual vs regressor plot is as expected', {

  skip_on_cran()
  p <- ols_rvsr_plot(model, mtcars$drat)
  vdiffr::expect_doppelganger('residual vs regressor plot', p$plot)

})
