context('influence')

model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that('leverage matches hat from influence.measures', {
	s1 <- round(ols_leverage(model), 4)
	s2 <- unname(round(influence.measures(model)[[1]][, 10], 4))
	expect_equal(s1, s2)
})


test_that('hadi measure matches the textbook example', {
    model <- lm(Nitrogen ~ ComIndl, data = rivers)
    act <- unname(round(ols_hadi(model)$hadi, 2))
    exp <- c(0.06, 0.07, 0.58, 0.77, 2.04, 0.10, 0.60, 0.37, 0.07, 0.08,
    	       0.13, 0.14, 0.15, 0.13, 0.16, 0.09, 0.12, 0.09, 0.19, 0.11)
    expect_equal(act, exp)
})

test_that('predicted rsquared output matches the expected result', {
    model <- lm(y ~ x4, data = cement)
    act <- round(ols_pred_rsq(model), 3)
    expect_equal(act, 0.56)
    
    model <- lm(y ~ x2, data = cement)
    act <- round(ols_pred_rsq(model), 3)
    expect_equal(act, 0.557)
    
    model <- lm(y ~ x1 + x2, data = cement)
    act <- round(ols_pred_rsq(model), 3)
    expect_equal(act, 0.965)
    
    model <- lm(y ~ x1 + x4, data = cement)
    act <- round(ols_pred_rsq(model), 3)
    expect_equal(act, 0.955)
    
    model <- lm(y ~ x1 + x2 + x4, data = cement)
    act <- round(ols_pred_rsq(model), 3)
    expect_equal(act, 0.969)
    
    model <- lm(y ~ x1 + x2 + x3, data = cement)
    act <- round(ols_pred_rsq(model), 3)
    expect_equal(act, 0.967)
    
    model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
    act <- round(ols_pred_rsq(model), 3)
    expect_equal(act, 0.959)
})

