context('info-criteria')

fullmodel   <- lm(y ~ x1 + x2 + x3 + x4, data = cement)

test_that('aic output matches expected result', {

	expect_equal(ols_aic(fullmodel, method = 'R'), round(AIC(fullmodel), 4))
	expect_equal(ols_aic(fullmodel, method = 'STATA'), 63.8367)

	model     <- lm(y ~ x1 + x2 + x4, data = cement)
	expect_equal(ols_aic(model, method = 'R'), round(AIC(model), 4))
	expect_equal(ols_aic(model, method = 'STATA'), 61.8663)

	model     <- lm(y ~ x2 + x4, data = cement)
	expect_equal(ols_aic(model, method = 'R'), round(AIC(model), 4))
	expect_equal(ols_aic(model, method = 'STATA'), 97.5217)


})


test_that('sbc output matches expected result', {

	expect_equal(ols_sbc(fullmodel, method = 'R'), round(BIC(fullmodel), 4))
	expect_equal(ols_sbc(fullmodel, method = 'STATA'), 66.6614)

	model     <- lm(y ~ x1 + x2 + x4, data = cement)
	expect_equal(ols_sbc(model, method = 'R'), round(BIC(model), 4))
	expect_equal(ols_sbc(model, method = 'STATA'), 64.1261)

	model     <- lm(y ~ x2 + x4, data = cement)
	expect_equal(ols_sbc(model, method = 'R'), round(BIC(model), 4))
	expect_equal(ols_sbc(model, method = 'STATA'), 99.2166)


})

test_that('apc output matches expected result', {

	expect_equal(round(ols_apc(fullmodel), 3), 0.04)

	model     <- lm(y ~ x1, data = cement)
	expect_equal(round(ols_apc(model), 3), 0.636)

	model     <- lm(y ~ x4, data = cement)
	expect_equal(round(ols_apc(model), 3), 0.444)

	model     <- lm(y ~ x1 + x2 + x4, data = cement)
	expect_equal(round(ols_apc(model), 3), 0.033)

	model     <- lm(y ~ x1 + x3 + x4, data = cement)
	expect_equal(round(ols_apc(model), 3), 0.035)

	model     <- lm(y ~ x2 + x4, data = cement)
	expect_equal(round(ols_apc(model), 3), 0.512)



})


test_that("mallow's cp output matches expected result", {

	model     <- lm(y ~ x1, data = cement)
	act       <- round(ols_mallows_cp(model, fullmodel), 2)
	exp       <- 202.55
	expect_equal(act, exp)

	model     <- lm(y ~ x4, data = cement)
	act       <- round(ols_mallows_cp(model, fullmodel), 2)
	exp       <- 138.73
	expect_equal(act, exp)

	model     <- lm(y ~ x1 + x2, data = cement)
	act       <- round(ols_mallows_cp(model, fullmodel), 2)
	exp       <- 2.68
	expect_equal(act, exp)

	model     <- lm(y ~ x2 + x4, data = cement)
	act       <- round(ols_mallows_cp(model, fullmodel), 2)
	exp       <- 138.23
	expect_equal(act, exp)

	model     <- lm(y ~ x1 + x2 + x4, data = cement)
	act       <- round(ols_mallows_cp(model, fullmodel), 2)
	exp       <- 3.02
	expect_equal(act, exp)

	model     <- lm(y ~ x1 + x3 + x4, data = cement)
	act       <- round(ols_mallows_cp(model, fullmodel), 2)
	exp       <- 3.50
	expect_equal(act, exp)
})

