context('error handling')

test_that("ols_leverage fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_leverage(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_hadi fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_hadi(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_press fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_press(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_pred_rsq fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_pred_rsq(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_corr_test fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_corr_test(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_aic fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_aic(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_sbc fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_sbc(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_sbic fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    fm <- lm(mpg ~ disp + hp + drat, data = mtcars)
    expect_error(ols_sbic(m, fm), 'Please specify a OLS linear regression model.')
    expect_error(ols_sbic(fm, m), 'Please specify a OLS linear regression model.')
})

test_that("ols_mallows_cp fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    fm <- lm(mpg ~ disp + hp + drat, data = mtcars)
    m2 <- lm(mpg ~ disp + qsec, data = mtcars)
    expect_error(ols_mallows_cp(m, fm), 'Please specify a OLS linear regression model.')
    expect_error(ols_mallows_cp(fm, m), 'Please specify a OLS linear regression model.')
    expect_error(ols_mallows_cp(m2, fm), 'model must be a subset of full model')
})

test_that("ols_gmsep fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_gmsep(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_jp fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_jp(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_pc fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_pc(m), 'Please specify a OLS linear regression model.')
})

test_that("ols_sp fails when model inherits other than 'lm'", {
    y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(ols_sp(m), 'Please specify a OLS linear regression model.')
})