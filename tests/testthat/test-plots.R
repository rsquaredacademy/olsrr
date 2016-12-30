context('plots')

test_that("ovsp_plot fails when model inherits other than 'lm'", {
	y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ovsp_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("rvsr_plot fails when model inherits other than 'lm'", {
	y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(rvsr_plot(m), 'Please specify a OLS linear regression model.')
})


test_that("rvsp_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(rvsp_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("qqresid fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(qqresid(m), 'Please specify a OLS linear regression model.')
})

test_that("hist_resid fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(hist_resid(m), 'Please specify a OLS linear regression model.')
})

test_that("resid_boxplot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(resid_boxplot(m), 'Please specify a OLS linear regression model.')
})

test_that("diag_panel fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(diag_panel(m), 'Please specify a OLS linear regression model.')
})

test_that("rfs_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(rfs_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("fm_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(fm_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("rsd_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(rsd_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("hadi_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(hadi_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("poten_resid_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(poten_resid_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("addvar_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(addvar_plot(m), 'Please specify a OLS linear regression model.')
})

test_that("cplusr_plot fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(cplusr_plot(m), 'Please specify a OLS linear regression model.')
})
