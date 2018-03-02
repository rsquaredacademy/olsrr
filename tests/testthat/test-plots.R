context("plots")

test_that("ovsp_plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_ovsp_plot(m), "Please specify a OLS linear regression model.")
})

test_that("rvsr_plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_rvsr_plot(m), "Please specify a OLS linear regression model.")
})


test_that("rvsp_plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_rvsp_plot(m), "Please specify a OLS linear regression model.")
})

test_that("qqresid fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_rsd_qqplot(m), "Please specify a OLS linear regression model.")
})

test_that("residual histogram fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_rsd_hist(m), "Please specify a OLS linear regression model.")
})

test_that("diag_panel fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_plot_diagnostics(m), "Please specify a OLS linear regression model.")
})

test_that("rfs_plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_rfs_plot(m), "Please specify a OLS linear regression model.")
})

test_that("fm_plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_fm_plot(m), "Please specify a OLS linear regression model.")
})

test_that("rsd_plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_rsd_plot(m), "Please specify a OLS linear regression model.")
})

test_that("hadi_plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_hadi_plot(m), "Please specify a OLS linear regression model.")
})

test_that("poten_resid_plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_potrsd_plot(m), "Please specify a OLS linear regression model.")
})

test_that("added variable plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_plot_added_variable(m), "Please specify a OLS linear regression model.")
})

test_that("cplusr_plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_rpc_plot(m), "Please specify a OLS linear regression model.")
})

test_that("residual boxplot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_rsd_boxplot(m), "Please specify a OLS linear regression model.")
})

test_that("cooks d barplot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_plot_cooksd_bar(m), "Please specify a OLS linear regression model.")
})

test_that("cooks d chart fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_plot_cooksd_chart(m), "Please specify a OLS linear regression model.")
})

test_that("dfbetas panel fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_plot_dfbetas(m), "Please specify a OLS linear regression model.")
})

test_that("dffits plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_plot_dffits(m), "Please specify a OLS linear regression model.")
})

test_that("dsrvsp plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_plot_resid_fit(m), "Please specify a OLS linear regression model.")
})

test_that("rsdlev plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_rsdlev_plot(m), "Please specify a OLS linear regression model.")
})

test_that("studentized residual plot fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_srsd_plot(m), "Please specify a OLS linear regression model.")
})

test_that("studentized residual chart fails when model inherits other than 'lm'", {
  y <- sample(c(1:4), 100, replace = T)
  x <- sample(c(1, 2), 100, replace = T)
  m <- glm(x ~ y)
  expect_error(ols_srsd_chart(m), "Please specify a OLS linear regression model.")
})
