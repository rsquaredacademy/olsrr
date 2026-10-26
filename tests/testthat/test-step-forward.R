test_that("forward selection output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_forward_p(model)
  expect_equal(k$metrics$step, 1:3)
  expect_equal(k$metrics$variable, c("x4", "x1", "x2"), ignore_attr = TRUE)

  k1 <- ols_step_forward_p(model, include = c("x3"))
  expect_equal(k1$metrics$step, 1:3)
  expect_equal(k1$metrics$variable, c("x3", "x4", "x1"), ignore_attr = TRUE)

  k2 <- ols_step_forward_p(model, exclude = c("x4"))
  expect_equal(k2$metrics$step, 1:3)
  expect_equal(k2$metrics$variable, c("x2", "x1", "x3"), ignore_attr = TRUE)

  k3 <- ols_step_forward_p(model, include = c(3))
  expect_equal(k3$metrics$step, 1:3)
  expect_equal(k3$metrics$variable, c("x3", "x4", "x1"), ignore_attr = TRUE)

  k4 <- ols_step_forward_p(model, exclude = c(4))
  expect_equal(k4$metrics$step, 1:3)
  expect_equal(k4$metrics$variable, c("x2", "x1", "x3"), ignore_attr = TRUE)
})

test_that("forward hierarchical selection output matches the expected result", {
  model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
  k <- ols_step_forward_p(model, 0.1, hierarchical = TRUE)
  expect_equal(k$metrics$step, 1:4)
  expect_equal(k$metrics$variable, c("bcs", "alc_heavy", "pindex", "enzyme_test"), ignore_attr = TRUE)
})

test_that("forward hierarchical selection stops after a specific number of steps", {
  model <- lm(y ~ bcs + alc_heavy + pindex + enzyme_test + liver_test + age + gender + alc_mod, data = surgical)
  k <- ols_step_forward_p(model, 0.1, hierarchical = TRUE, steps = 2)
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("bcs", "alc_heavy"), ignore_attr = TRUE)
})

test_that("stepwise forward regression stops after a specific number of steps", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_forward_p(model, steps = 2)
  expect_equal(k$metrics$step, 1:2)
  expect_equal(k$metrics$variable, c("x4", "x1"), ignore_attr = TRUE)
})


test_that("step_forward_p returns the appropriate error", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_error(ols_step_forward_p(model, include = c("dis")), "dis not part of the model and hence cannot be forcibly included. Please verify the variable names.")
  expect_error(ols_step_forward_p(model, exclude = c("hps")), "hps not part of the model and hence cannot be forcibly excluded. Please verify the variable names.")
  expect_error(ols_step_forward_p(model, include = c(5)), "Index of variable to be included should be between 1 and 4.")
  expect_error(ols_step_forward_p(model, exclude = c(5)), "Index of variable to be excluded should be between 1 and 4.")
})
test_that("forward selection refits transformed responses safely (audit F1)", {
  model <- lm(log(mpg) ~ disp + hp + wt, data = mtcars)
  k <- ols_step_forward_p(model)

  expect_s3_class(k$model, "lm")
  expect_equal(coef(k$model), coef(lm(formula(k$model), data = mtcars)))
  expect_equal(k$model$df.residual, lm(formula(k$model), data = mtcars)$df.residual)
})

test_that("printing stepwise results with transformed terms works (audit F1)", {
  # the printed Beta of `term` in the final model output table
  beta_from_print <- function(out, term) {
    lines <- trimws(out)
    line  <- lines[startsWith(lines, term)][1]
    rest  <- substring(line, nchar(term) + 1)
    as.numeric(regmatches(rest, regexpr("-?[0-9]+\\.[0-9]+", rest)))
  }

  # the transformed term ends up in the final model of every procedure
  models <- list(
    "log"    = log(mpg) ~ disp + hp + wt + qsec,
    "factor" = mpg ~ factor(cyl) + hp + wt + qsec,
    "poly"   = mpg ~ poly(disp, 2) + hp + wt + qsec,
    "I"      = mpg ~ I(disp/100) + hp + qsec
  )

  procedures <- c(
    "ols_step_forward_p", "ols_step_backward_p", "ols_step_both_p",
    "ols_step_forward_aic", "ols_step_backward_aic", "ols_step_both_aic",
    "ols_step_forward_sbc", "ols_step_backward_sbc", "ols_step_both_sbc",
    "ols_step_forward_sbic", "ols_step_backward_sbic", "ols_step_both_sbic",
    "ols_step_forward_r2", "ols_step_backward_r2", "ols_step_both_r2",
    "ols_step_forward_adj_r2", "ols_step_backward_adj_r2", "ols_step_both_adj_r2"
  )

  for (term in names(models)) {
    model <- lm(models[[term]], data = mtcars)

    for (procedure in procedures) {
      k <- do.call(procedure, list(model))
      expect_no_error(capture.output(print(k)))
    }
  }

  # the printed coefficients are the estimates of the final model
  k        <- ols_step_forward_p(lm(models[["log"]], data = mtcars))
  out      <- capture.output(print(k))
  ref      <- lm(formula(k$model), data = mtcars)
  coefname <- names(coef(ref))[2]

  expect_equal(
    beta_from_print(out, coefname),
    unname(round(coef(ref)[coefname], 3)),
    tolerance = 1e-6
  )
})
