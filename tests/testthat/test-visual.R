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
  skip_if(getRversion() > '4.0.3')
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
  skip_if(getRversion() > '4.0.3')
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
  p <- ols_plot_resid_regressor(model, 'drat')
  vdiffr::expect_doppelganger("residual vs regressor plot", p)
})

test_that("cooks d bar plot is as expected", {
  skip_on_cran()
  p <- ols_plot_cooksd_bar(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("cooks d bar plot", p$plot)
})

test_that("cooks d bar chart is as expected", {
  skip_on_cran()
  p <- ols_plot_cooksd_chart(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("cooks d bar chart", p$plot)
})

test_that("dffits plot is as expected", {
  skip_on_cran()
  p <- ols_plot_dffits(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("dffits plot", p$plot)
})

test_that("deleted studentized residual vs fitted plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_stud_fit(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("dsrvsp plot", p$plot)
})

test_that("residual vs regressor shiny plot is as expected", {
  skip_on_cran()
  p <- rvsr_plot_shiny(model, mtcars, "drat")
  vdiffr::expect_doppelganger("residual vs regressor shiny plot", p)
})

test_that("residual fit spread plot is as expected", {
  skip_on_cran()
  p <- ols_plot_resid_fit_spread(model, print_plot = TRUE)
  vdiffr::expect_doppelganger("fm_plot", p$fm_plot)
  vdiffr::expect_doppelganger("rsd_plot", p$rsd_plot)
})

test_that("response profile plot is as expected", {
  skip_on_cran()
  p <- ols_plot_response(model, print_plot = FALSE)
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

test_that("added variable plot is as expected", {
  skip_on_cran()
  p <- ols_plot_added_variable(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("avplot_1", p[[1]])
  vdiffr::expect_doppelganger("avplot_2", p[[2]])
  vdiffr::expect_doppelganger("avplot_3", p[[3]])
})

test_that("all possible regression plots are as expected", {
  skip_if(getRversion() > '4.0.3')
  skip_on_cran()
  model <- lm(mpg ~ disp + hp, data = mtcars)
  k     <- ols_step_all_possible(model)
  p     <- plot(k, print_plot = FALSE)
  vdiffr::expect_doppelganger("allplot_1", p$plot_1)
  vdiffr::expect_doppelganger("allplot_2", p$plot_2)
  vdiffr::expect_doppelganger("allplot_3", p$plot_3)
  vdiffr::expect_doppelganger("allplot_4", p$plot_4)
  vdiffr::expect_doppelganger("allplot_5", p$plot_5)
  vdiffr::expect_doppelganger("allplot_6", p$plot_6)
})

test_that("best subset regression plots are as expected", {
  skip_on_cran()
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  k     <- ols_step_best_subset(model)
  p     <- plot(k, print_plot = FALSE)
  vdiffr::expect_doppelganger("bestplot_1", p$plot_1)
  vdiffr::expect_doppelganger("bestplot_2", p$plot_2)
  vdiffr::expect_doppelganger("bestplot_3", p$plot_3)
  vdiffr::expect_doppelganger("bestplot_4", p$plot_4)
  vdiffr::expect_doppelganger("bestplot_5", p$plot_5)
  vdiffr::expect_doppelganger("bestplot_6", p$plot_6)
})

test_that("dfbetas plot is as expected", {
  skip_on_cran()
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  p <- ols_plot_dfbetas(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("dfbetas_1", p$plots[[1]])
  vdiffr::expect_doppelganger("dfbetas_2", p$plots[[2]])
  vdiffr::expect_doppelganger("dfbetas_3", p$plots[[3]])
  vdiffr::expect_doppelganger("dfbetas_4", p$plots[[4]])
  vdiffr::expect_doppelganger("dfbetas_5", p$plots[[5]])
})

test_that("diagnostics panel is as expected", {
  skip_if(getRversion() > '4.0.3')
  skip_on_cran()
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  p <- ols_plot_diagnostics(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("diag_1", p[[1]])
  vdiffr::expect_doppelganger("diag_2", p[[2]])
  vdiffr::expect_doppelganger("diag_3", p[[3]])
  vdiffr::expect_doppelganger("diag_4", p[[4]])
  vdiffr::expect_doppelganger("diag_5", p[[5]])
  vdiffr::expect_doppelganger("diag_6", p[[6]])
  vdiffr::expect_doppelganger("diag_7", p[[7]])
  vdiffr::expect_doppelganger("diag_8", p[[8]])
  vdiffr::expect_doppelganger("diag_9", p[[9]])
  vdiffr::expect_doppelganger("diag_10", p[[10]])
})


test_that("fitted line plot is as expected", {
  skip_if(getRversion() > '4.0.3')
  skip_on_cran()
  p <- ols_plot_reg_line(mtcars$mpg, mtcars$disp, print_plot = FALSE)
  vdiffr::expect_doppelganger("reg_line_plot", p)
})

test_that("residual plus component plot is as expected", {
  skip_on_cran()
  model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
  p     <- ols_plot_comp_plus_resid(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("rpcplot_1", p[[1]])
  vdiffr::expect_doppelganger("rpcplot_2", p[[2]])
  vdiffr::expect_doppelganger("rpcplot_3", p[[3]])
  vdiffr::expect_doppelganger("rpcplot_4", p[[4]])
})

test_that("rstud vs lev plot is as expected", {
  skip_on_cran()
  model <- lm(read ~ write + math + science, data = hsb)
  p     <- ols_plot_resid_lev(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("rslev_1", p$plot)
})

test_that("standardized residual plot is as expected", {
  skip_on_cran()
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  p     <- ols_plot_resid_stand(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("stanres_1", p$plot)
})

test_that("studentized residual plot is as expected", {
  skip_on_cran()
  model <- lm(mpg ~ disp + hp + wt, data = mtcars)
  p     <- ols_plot_resid_stud(model, print_plot = FALSE)
  vdiffr::expect_doppelganger("studres_1", p$plot)
})

test_that("stepwise backward regression plots are as expected", {
  skip_on_cran()
  model <- lm(y ~ ., data = surgical)
  k     <- ols_step_backward_p(model)
  p     <- plot(k, print_plot = FALSE)
  vdiffr::expect_doppelganger("step_back_1", p$plot_1)
  vdiffr::expect_doppelganger("step_back_2", p$plot_2)
  vdiffr::expect_doppelganger("step_back_3", p$plot_3)
  vdiffr::expect_doppelganger("step_back_4", p$plot_4)
  vdiffr::expect_doppelganger("step_back_5", p$plot_5)
  vdiffr::expect_doppelganger("step_back_6", p$plot_6)
})

test_that("stepwise forward regression plots are as expected", {
  skip_on_cran()
  model <- lm(y ~ ., data = surgical)
  k     <- ols_step_forward_p(model)
  p     <- plot(k, print_plot = FALSE)
  vdiffr::expect_doppelganger("step_forward_1", p$plot_1)
  vdiffr::expect_doppelganger("step_forward_2", p$plot_2)
  vdiffr::expect_doppelganger("step_forward_3", p$plot_3)
  vdiffr::expect_doppelganger("step_forward_4", p$plot_4)
  vdiffr::expect_doppelganger("step_forward_5", p$plot_5)
  vdiffr::expect_doppelganger("step_forward_6", p$plot_6)
})

test_that("stepwise both regression plots are as expected", {
  skip_on_cran()
  model <- lm(y ~ ., data = surgical)
  k     <- ols_step_both_p(model)
  p     <- plot(k, print_plot = FALSE)
  vdiffr::expect_doppelganger("step_both_1", p$plot_1)
  vdiffr::expect_doppelganger("step_both_2", p$plot_2)
  vdiffr::expect_doppelganger("step_both_3", p$plot_3)
  vdiffr::expect_doppelganger("step_both_4", p$plot_4)
  vdiffr::expect_doppelganger("step_both_5", p$plot_5)
  vdiffr::expect_doppelganger("step_both_6", p$plot_6)
})

