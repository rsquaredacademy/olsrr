context('plots')

test_that("ovsp_plot fails when model inherits other than 'lm'", {
	y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_ovsp_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("rvsr_plot fails when model inherits other than 'lm'", {
	y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_rvsr_plot(m), 'Please specify a OLS linear regression model.')
})


test_that("rvsp_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_rvsp_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("qqresid fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_rsd_qqplot(m), 'Please specify a OLS linear regression model.')
})

test_that("hist_resid fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_resid_hist(m), 'Please specify a OLS linear regression model.')
})

test_that("diag_panel fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_diagnostic_panel(m), 'Please specify a OLS linear regression model.')
})

test_that("rfs_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_rfs_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("fm_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_fm_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("rsd_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_rsd_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("hadi_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_hadi_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("poten_resid_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_potrsd_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_avplots fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_avplots(m), 'Please specify a OLS linear regression model.')
})

test_that("cplusr_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_rpc_plot(m), 'Please specify a OLS linear regression model.')
})
