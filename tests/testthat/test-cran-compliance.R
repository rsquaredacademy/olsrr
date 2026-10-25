make_noise_model <- function(p, n = 40, seed = 1) {
  set.seed(seed)
  d <- as.data.frame(matrix(rnorm(n * p), n, p))
  names(d) <- paste0("x", seq_len(p))
  d$y <- rnorm(n)
  lm(y ~ ., data = d)
}

test_that("all possible regression counts the models fitted when max_order is set", {
  # 22 predictors and max_order = 2 fit sum(choose(22, 1:2)) = 253 models
  model <- make_noise_model(22)

  expect_no_error(k <- ols_step_all_possible(model, max_order = 2))
  expect_equal(nrow(k$result), sum(choose(22, seq_len(2))))

  # 18 predictors and max_order = 1 fit 18 models
  model <- make_noise_model(18)

  expect_no_warning(k <- ols_step_all_possible(model, max_order = 1))
  expect_equal(nrow(k$result), 18)
})

test_that("all possible regression warns above 2^15 - 1 models", {
  model <- make_noise_model(16)

  expect_warning(
    olsrr:::check_force(model, force = FALSE, max_order = NULL),
    "Selecting among 65535 models may take significant time.",
    fixed = TRUE
  )
})

test_that("all possible regression stops above 2^20 - 1 models without force", {
  model <- make_noise_model(21)

  expect_error(
    olsrr:::check_force(model, force = FALSE, max_order = NULL),
    "Combinatorial subset search involving 2097151 models is prohibited without force = TRUE due to exponential computational scaling.",
    fixed = TRUE
  )
})

test_that("force = TRUE allows a search involving more than 2^20 - 1 models", {
  model <- make_noise_model(21)

  expect_warning(
    olsrr:::check_force(model, force = TRUE, max_order = NULL),
    "Selecting among 2097151 models may take significant time.",
    fixed = TRUE
  )

  k <- ols_step_all_possible(model, max_order = 1)

  expect_s3_class(k, "ols_step_all_possible")
  expect_equal(nrow(k$result), 21)
})

test_that("ols_launch_app() explains how to install a missing xplorerr", {
  local_mocked_bindings(xplorerr_available = function() FALSE)

  expect_error(ols_launch_app(), "install.packages", fixed = TRUE)
})

test_that("model checks accept lm subclasses", {
  model <- lm(mpg ~ disp + hp, data = mtcars)
  class(model) <- c("foo", "lm")

  expect_s3_class(ols_test_normality(model), "ols_test_normality")
  expect_s3_class(ols_step_all_possible_betas(model), "data.frame")
})

test_that("there is no network access at attach", {
  expect_false(exists(".onAttach", envir = asNamespace("olsrr"), inherits = FALSE))
})
