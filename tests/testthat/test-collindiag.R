context("coll_diag")

hsb$race_1 <- ifelse(hsb$race == 1, 1, 0)
hsb$race_2 <- ifelse(hsb$race == 2, 1, 0)
hsb$race_3 <- ifelse(hsb$race == 3, 1, 0)
hsb$race_4 <- ifelse(hsb$race == 4, 1, 0)

model <- lm(write ~ read + math + science + race_2 + race_3 + race_4,
            data = hsb)


test_that("output from vif_tol matches expected result", {

	act       <- vif_tol(model)
	Variables <- c("read", "math", "science", "race_2", "race_3", "race_4")
	Tolerance <- c(0.482, 0.469, 0.475, 0.692, 0.602, 0.467)
	VIF       <- c(2.074, 2.132, 2.104, 1.446, 1.662, 2.141)
	exp       <- tibble(Variables, Tolerance, VIF)
	expect_equivalent(act, exp)

})


test_that("output from eigen_cindex matches expected result", {

	act  <- eigen_cindex(model)
	col1 <- c(4.865, 1.002, 1.000, 0.091, 0.018, 0.013, 0.011)
	col2 <- c(1.000, 2.203, 2.205, 7.298, 16.263, 19.583, 21.447)
	col3 <- c(0.000, 0.000, 0.000, 0.010,  0.870,  0.050,  0.070)
	col4 <- c(0.000, 0.000, 0.000, 0.010,  0.240,  0.370,  0.370)
	col5 <- c(0.000, 0.000, 0.000, 0.010,  0.020,  0.020,  0.960)
	col6 <- c(0.000, 0.000, 0.000, 0.010,  0.020,  0.900,  0.060)
	col7 <- c(0.000, 0.000, 0.610, 0.370,  0.000,  0.000,  0.020)
	col8 <- c(0.000, 0.480, 0.010, 0.430,  0.060,  0.010,  0.000)
	col9 <- c(0.000, 0.010, 0.010, 0.960,  0.000,  0.010,  0.000)
	exp  <- data.frame(col1, col2, col3, col4, col5, col6, col7, col8, col9)
	names(exp) <- c("Eigenvalue", "Condition Index", "intercept", "read", "math", "science", "race_2", "race_3")
	expect_equivalent(act, exp)

})


test_that("vif_tol fails when model inherits other than 'lm'", {
		y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(vif_tol(m), 'Please specify a OLS linear regression model.')
})


test_that("eigen_cindex fails when model inherits other than 'lm'", {
		y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(eigen_cindex(m), 'Please specify a OLS linear regression model.')
})


test_that("coll_diag fails when model inherits other than 'lm'", {
		y <- sample(c(1:4), 100, replace = T)
    x <- sample(c(1, 2), 100, replace = T)
    m <- glm(x ~ y)
    expect_error(coll_diag(m), 'Please specify a OLS linear regression model.')
})
