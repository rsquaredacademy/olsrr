model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that("when fitted.values == TRUE, fitted values from the regression\n\tare used for the test", {
  b <- ols_test_score(model)

  expect_equal(round(b$score, 3), 1.269)
  expect_equal(round(b$p, 3), 0.26)
  expect_equal(b$df, 1)
  expect_true(b$fv)
  expect_false(b$rhs)
  expect_match(b$preds, "fitted values of mpg")
  expect_match(b$resp, "mpg")
})

test_that("when fitted.values == TRUE and rhs == TRUE, predictors from the\n\tmodel are used for the test", {
  b <- ols_test_score(model, rhs = TRUE)

  expect_equal(round(b$score, 3), 2.516)
  expect_equal(round(b$p, 3), 0.774)
  expect_equal(b$df, 5)
  expect_false(b$fv)
  expect_true(b$rhs)
  expect_equal(b$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
  expect_match(b$resp, "mpg")
})


test_that("when vars != NULL, variables specified from the are\n\tused for the test", {
  b <- ols_test_score(model, vars = c("disp", "hp"))

  expect_equal(round(b$score, 3), 0.969)
  expect_equal(round(b$p, 3), 0.616)
  expect_equal(b$df, 2)
  expect_false(b$fv)
  expect_false(b$rhs)
  expect_equal(b$preds, c("disp", "hp"), ignore_attr = TRUE)
  expect_match(b$resp, "mpg")
})


test_that("when vars != NULL and rhs == TRUE, predictors in the model are\n\tused for the test", {
  b <- ols_test_score(model, rhs = TRUE, vars = c("disp", "hp"))

  expect_equal(round(b$score, 3), 2.516)
  expect_equal(round(b$p, 3), 0.774)
  expect_equal(b$df, 5)
  expect_false(b$fv)
  expect_true(b$rhs)
  expect_equal(b$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
  expect_match(b$resp, "mpg")
})


