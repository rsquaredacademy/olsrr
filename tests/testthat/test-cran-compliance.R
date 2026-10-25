make_noise_model <- function(p, n = 40, seed = 1) {
  set.seed(seed)
  d <- as.data.frame(matrix(rnorm(n * p), n, p))
  names(d) <- paste0("x", seq_len(p))
  d$y <- rnorm(n)
  lm(y ~ ., data = d)
}

test_that("all possible regression warns above 15 predictors", {
  model <- make_noise_model(16)

  expect_warning(
    ols_step_all_possible(model, max_order = 1),
    "Selecting among > 15 predictors involves fitting 2^p models. This may take significant time.",
    fixed = TRUE
  )
})

test_that("all possible regression stops above 20 predictors without force", {
  model <- make_noise_model(21)

  expect_error(
    ols_step_all_possible(model, max_order = 1),
    "Combinatorial subset search with p > 20 predictors is prohibited without force = TRUE due to exponential computational scaling.",
    fixed = TRUE
  )
})

test_that("force = TRUE allows more than 20 predictors", {
  model <- make_noise_model(21)

  expect_warning(
    k <- ols_step_all_possible(model, max_order = 1, force = TRUE),
    "Selecting among > 15 predictors involves fitting 2^p models. This may take significant time.",
    fixed = TRUE
  )

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
