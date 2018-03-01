context("Visual Test")

model <- lm(mpg ~ disp + hp + wt, data = mtcars)

test_that("residual histogram plot is as expected", {
  skip_on_cran()
  p <- ols_rsd_hist(model)
  vdiffr::expect_doppelganger("ggplot2 histogram", p$plot)
})

test_that("hadi plot is as expected", {
  skip_on_cran()
  p <- ols_hadi_plot(model)
  vdiffr::expect_doppelganger("hadi plot", p$plot)
})

test_that("observed vs predicted plot is as expected", {
  skip_on_cran()
  p <- ols_ovsp_plot(model)
  vdiffr::expect_doppelganger("ovsp plot", p$plot)
})

test_that("potential residual plot is as expected", {
  skip_on_cran()
  p <- ols_potrsd_plot(model)
  vdiffr::expect_doppelganger("potential residual plot", p$plot)
})

test_that("residual box plot is as expected", {
  skip_on_cran()
  p <- ols_rsd_boxplot(model)
  vdiffr::expect_doppelganger("residual box plot", p$plot)
})

test_that("residual fit spread plot 1 is as expected", {
  skip_on_cran()
  p <- ols_rsd_plot(model)
  vdiffr::expect_doppelganger("residual fit spread plot", p$plot)
})

test_that("residual fit spread plot 2 is as expected", {
  skip_on_cran()
  p <- ols_fm_plot(model)
  vdiffr::expect_doppelganger("residual fit spread plot 2", p$plot)
})

test_that("residual qq plot is as expected", {
  skip_on_cran()
  p <- ols_rsd_qqplot(model)
  vdiffr::expect_doppelganger("residual qq plot", p$plot)
})

test_that("residual vs fitted plot is as expected", {
  skip_on_cran()
  p <- ols_rvsp_plot(model)
  vdiffr::expect_doppelganger("residual vs fitted plot", p$plot)
})

test_that("residual vs regressor plot is as expected", {
  skip_on_cran()
  p <- ols_rvsr_plot(model, drat)
  vdiffr::expect_doppelganger("residual vs regressor plot", p$plot)
})

test_that("cooks d bar plot is as expected", {
  skip_on_cran()
  p <- ols_cooksd_barplot(model)
  vdiffr::expect_doppelganger("cooks d bar plot", p$plot)
})

test_that("cooks d bar chart is as expected", {
  skip_on_cran()
  p <- ols_cooksd_chart(model)
  vdiffr::expect_doppelganger("cooks d bar chart", p$plot)
})

test_that("dfbetas panel is as expected", {
  skip_on_cran()
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  k <- ols_dfbetas_panel(model)

  vdiffr::expect_doppelganger("dfbetas panel intercept", k$plots[[1]])
  vdiffr::expect_doppelganger("dfbetas panel disp", k$plots[[2]])
  vdiffr::expect_doppelganger("dfbetas panel hp", k$plots[[3]])
  vdiffr::expect_doppelganger("dfbetas panel wt", k$plots[[4]])
  vdiffr::expect_doppelganger("dfbetas panel qsec", k$plots[[5]])
})

test_that("dffits plot is as expected", {
  skip_on_cran()
  p <- ols_dffits_plot(model)
  vdiffr::expect_doppelganger("dffits plot", p$plot)
})

test_that("diagnostic panel is as expected", {
  skip_on_cran()
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  k <- ols_diagnostic_panel(model)

  vdiffr::expect_doppelganger("diag panel plot_1", k$plot_1)
  vdiffr::expect_doppelganger("diag panel plot_2", k$plot_2)
  vdiffr::expect_doppelganger("diag panel plot_3", k$plot_3)
  vdiffr::expect_doppelganger("diag panel plot_4", k$plot_4)
  vdiffr::expect_doppelganger("diag panel plot_5", k$plot_5)
  vdiffr::expect_doppelganger("diag panel plot_6", k$plot_6)
  vdiffr::expect_doppelganger("diag panel plot_7", k$plot_7)
  vdiffr::expect_doppelganger("diag panel plot_8", k$plot_8)
  vdiffr::expect_doppelganger("diag panel plot_9", k$plot_9)
  vdiffr::expect_doppelganger("diag panel plot_10", k$plot_10)
})

test_that("deleted studentized residual vs fitted plot is as expected", {
  skip_on_cran()
  p <- ols_dffits_plot(model)
  vdiffr::expect_doppelganger("dsrvsp plot", p$plot)
})

test_that("residual vs regressor shiny plot is as expected", {
  skip_on_cran()
  p <- rvsr_plot_shiny(model, mtcars, "drat")
  vdiffr::expect_doppelganger("residual vs regressor shiny plot", p$plot)
})

test_that("residual fit spread plot is as expected", {
  skip_on_cran()
  p <- ols_rfs_plot(model)
  vdiffr::expect_doppelganger("fm_plot", p$fm_plot)
  vdiffr::expect_doppelganger("rsd_plot", p$rsd_plot)
})

test_that("residual fit spread plot is as expected", {
  skip_on_cran()
  p <- ols_rpc_plot(model)
  vdiffr::expect_doppelganger("rpc plot disp", p$plots[[1]])
  vdiffr::expect_doppelganger("rpc plot hp", p$plots[[2]])
  vdiffr::expect_doppelganger("rpc plot wt", p$plots[[3]])
})

test_that("response profile plot is as expected", {
  skip_on_cran()
  p <- ols_resp_viz(model)
  vdiffr::expect_doppelganger("resp viz dot plot", p$dot_plot)
  vdiffr::expect_doppelganger("resp viz trend plot", p$trend_plot)
  vdiffr::expect_doppelganger("resp viz histogram", p$histogram)
  vdiffr::expect_doppelganger("resp viz boxplot", p$boxplot)
})

test_that("stepAIC backward regression plot is as expected", {
  skip_on_cran()
  model <- lm(y ~ ., data = surgical)
  p <- plot(ols_stepaic_backward(model))
  vdiffr::expect_doppelganger("stepaic backward regression plot", p$plot)
})

test_that("stepAIC forward regression plot is as expected", {
  skip_on_cran()
  model <- lm(y ~ ., data = surgical)
  p <- plot(ols_stepaic_forward(model))
  vdiffr::expect_doppelganger("stepaic forward regression plot", p$plot)
})

test_that("stepAIC both direction regression plot is as expected", {
  skip_on_cran()
  model <- lm(y ~ ., data = surgical)
  p <- plot(ols_stepaic_both(model))
  vdiffr::expect_doppelganger("stepaic both regression plot", p$plot)
})

test_that("stepwise backward regression plots are as expected", {
  skip_on_cran()

  model <- lm(y ~ ., data = surgical)
  k <- plot(ols_step_backward(model))

  vdiffr::expect_doppelganger("stepwise backward rsquare", k$rsquare_plot)
  vdiffr::expect_doppelganger("stepwise backward adjusted rsquare", k$adj_rsquare_plot)
  vdiffr::expect_doppelganger("stepwise backward mallows cp", k$mallows_cp_plot)
  vdiffr::expect_doppelganger("stepwise backward aic", k$aic_plot)
  vdiffr::expect_doppelganger("stepwise backward sbic", k$sbic_plot)
  vdiffr::expect_doppelganger("stepwise backward sbc", k$sbc_plot)
})

test_that("stepwise forward regression plots are as expected", {
  skip_on_cran()

  model <- lm(y ~ ., data = surgical)
  k <- plot(ols_step_forward(model))

  vdiffr::expect_doppelganger("stepwise forward rsquare", k$rsquare_plot)
  vdiffr::expect_doppelganger("stepwise forward adjusted rsquare", k$adj_rsquare_plot)
  vdiffr::expect_doppelganger("stepwise forward mallows cp", k$mallows_cp_plot)
  vdiffr::expect_doppelganger("stepwise forward aic", k$aic_plot)
  vdiffr::expect_doppelganger("stepwise forward sbic", k$sbic_plot)
  vdiffr::expect_doppelganger("stepwise forward sbc", k$sbc_plot)
})

test_that("stepwise regression plots are as expected", {
  skip_on_cran()

  model <- lm(y ~ ., data = surgical)
  k <- plot(ols_stepwise(model))

  vdiffr::expect_doppelganger("stepwise rsquare", k$rsquare_plot)
  vdiffr::expect_doppelganger("stepwise adjusted rsquare", k$adj_rsquare_plot)
  vdiffr::expect_doppelganger("stepwise mallows cp", k$mallows_cp_plot)
  vdiffr::expect_doppelganger("stepwise aic", k$aic_plot)
  vdiffr::expect_doppelganger("stepwise sbic", k$sbic_plot)
  vdiffr::expect_doppelganger("stepwise sbc", k$sbc_plot)
})

test_that("studentized residual chart is as expected", {
  skip_on_cran()
  p <- ols_srsd_chart(model)
  vdiffr::expect_doppelganger("studentized residual chart", p$plot)
})

test_that("studentized residual plot is as expected", {
  skip_on_cran()
  p <- ols_srsd_plot(model)
  vdiffr::expect_doppelganger("studentized residual plot", p$plot)
})

test_that("fitted line properties plot is as expected", {
  skip_on_cran()
  p <- ols_reg_line(mtcars$mpg, mtcars$disp)
  vdiffr::expect_doppelganger("fitted line plot", p$plot)
})

test_that("added variable plot is as expected", {
  skip_on_cran()

  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  k <- ols_plot_added_variable(model)

  vdiffr::expect_doppelganger("avplot disp", k$plots[[1]])
  vdiffr::expect_doppelganger("avplot hp", k$plots[[2]])
  vdiffr::expect_doppelganger("avplot wt", k$plots[[3]])
})

test_that("all possible regression plots are as expected", {
  skip_on_cran()

  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- plot(ols_all_subset(model))

  vdiffr::expect_doppelganger("all possible rsquare", k$rsquare_plot)
  vdiffr::expect_doppelganger("all possible adjusted rsquare", k$adj_rsquare_plot)
  vdiffr::expect_doppelganger("all possible mallows cp", k$mallows_cp_plot)
  vdiffr::expect_doppelganger("all possible aic", k$aic_plot)
  vdiffr::expect_doppelganger("all possible sbic", k$sbic_plot)
  vdiffr::expect_doppelganger("all possible sbc", k$sbc_plot)
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
