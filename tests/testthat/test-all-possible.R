# Combination-allocation counters. trace() evaluates its exit expression in the
# traced function's own environment, so the counters live in the global
# environment, which is on that function's search path.
start_combn_trace <- function() {
  assign(".ofx2_combn", c(calls = 0, bytes = 0), envir = globalenv())
  trace(utils::combn, exit = quote({
    .ofx2_combn <<- .ofx2_combn + c(1, as.numeric(object.size(returnValue())))
  }), print = FALSE)
}

combn_trace <- function() {
  get(".ofx2_combn", envir = globalenv())
}

stop_combn_trace <- function() {
  untrace(utils::combn)
  rm(".ofx2_combn", envir = globalenv())
}

test_that("all possible regression output matches the expected result", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  k <- ols_step_all_possible(model)
  pred_exp <- c(
    "x4", "x2", "x1", "x3", "x1 x2",
    "x1 x4", "x3 x4", "x2 x3", "x2 x4", "x1 x3",
    "x1 x2 x4", "x1 x2 x3", "x1 x3 x4", "x2 x3 x4",
    "x1 x2 x3 x4"
  )
  expect_equal(k$result$mindex, c(1:15))
  expect_equal(k$result$predictors, pred_exp, ignore_attr = TRUE)

  k2 <- ols_step_all_possible(model, max_order = 3)
  preds_selected <- c(
    "x4", "x2", "x1", "x3", "x1 x2",
    "x1 x4", "x3 x4", "x2 x3", "x2 x4", "x1 x3",
    "x1 x2 x4", "x1 x2 x3", "x1 x3 x4", "x2 x3 x4"
  )
  expect_equal(k2$result$mindex, c(1:14))
  expect_equal(k2$result$predictors, preds_selected, ignore_attr = TRUE)
})

test_that("a bounded search enumerates only the requested subset orders (M3)", {
  set.seed(5)
  d <- as.data.frame(matrix(rnorm(40 * 16), 40))
  names(d) <- paste0("x", seq_len(16))
  d$y <- rnorm(40)
  m <- lm(y ~ ., d)

  start_combn_trace()
  on.exit(stop_combn_trace(), add = TRUE)

  ap <- ols_step_all_possible(m, max_order = 1)
  expect_equal(nrow(ap$result), 16)
  expect_setequal(ap$result$predictors, names(d)[1:16])
  expect_equal(combn_trace()[["calls"]], 1)   # one order, not sixteen
  expect_lt(combn_trace()[["bytes"]], 100000)

  # the values are still those of the equivalent single-predictor fits
  expect_equal(ap$result$rsquare[ap$result$predictors == "x1"],
               summary(lm(y ~ x1, d))$r.squared)

  start_combn_trace()
  ap2 <- ols_step_all_possible(m, max_order = 2)
  expect_equal(nrow(ap2$result), 16 + choose(16, 2))
  expect_equal(combn_trace()[["calls"]], 2)
  expect_lt(combn_trace()[["bytes"]], 100000)

  start_combn_trace()
  bs <- ols_step_best_subset(m, max_order = 2)
  expect_equal(nrow(bs$metrics), 2)
  expect_equal(combn_trace()[["calls"]], 2)

  # a bound larger than the model is rejected before any allocation
  start_combn_trace()
  expect_error(ols_step_best_subset(lm(y ~ x1 + x2, d), max_order = 5),
               "Maximum subset order")
  expect_equal(combn_trace()[["calls"]], 0)
})

test_that("bounded searches stay cheap for many predictors (M3)", {
  set.seed(5)
  d <- as.data.frame(matrix(rnorm(40 * 22), 40))
  d$y <- rnorm(40)

  start_combn_trace()
  on.exit(stop_combn_trace(), add = TRUE)

  ap <- ols_step_all_possible(lm(y ~ ., d), max_order = 2)
  expect_equal(nrow(ap$result), 22 + choose(22, 2))
  expect_equal(combn_trace()[["calls"]], 2)
  expect_lt(combn_trace()[["bytes"]], 100000)
})

test_that("all possible regression throws error messages", {
  model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
  error_msg <- "Maximum subset order should be less than or equal to the number of predictors in the specified model."
  expect_error(ols_step_all_possible(model, max_order = 5), error_msg)
})


test_that("output from all subsets regression is as expected", {
  model <- lm(mpg ~ disp + hp, data = mtcars)
  expect_snapshot(ols_step_all_possible(model))
})

test_that("all possible regression betas are as expected", {
  
  model   <- lm(mpg ~ disp + hp + wt, data = mtcars)
  k       <- ols_step_all_possible_betas(model)
  out     <- lapply(split(k$beta, k$predictor), mean)
  actual  <- data.frame(predictor = names(out), beta = as.numeric(out))

  predictor <- c("(Intercept)", "disp", "hp", "wt")
  beta <- c(33.85901073, -0.02255579, -0.03899945, -4.09350456)

  expected <- data.frame(predictor, beta)
  expect_equal(actual$predictor, expected$predictor, ignore_attr = TRUE)
  expect_equal(actual$beta, expected$beta, ignore_attr = TRUE)
})


test_that("all possible regression refits transformed responses safely (audit F1)", {
  model <- lm(log(mpg) ~ disp + hp, data = mtcars)
  k <- ols_step_all_possible(model)

  expect_equal(nrow(k$result), 3)
  expect_equal(sort(k$result$rsquare)[3],
               summary(lm(log(mpg) ~ disp + hp, data = mtcars))$r.squared)
})

test_that("all-possible routines respect a no-intercept model (H7)", {
  m <- lm(mpg ~ 0 + wt + hp + disp, mtcars)

  # candidate models keep the input model definition, so the single-predictor
  # model has the uncentred R-squared of a no-intercept fit
  ap  <- ols_step_all_possible(m)
  row <- ap$result[ap$result$predictors == "wt", ]
  expect_equal(row$rsquare, summary(lm(mpg ~ 0 + wt, mtcars))$r.squared)

  bs   <- ols_step_best_subset(m)
  brow <- bs$metrics[bs$metrics$predictors == "wt", ]
  expect_equal(brow$rsquare, summary(lm(mpg ~ 0 + wt, mtcars))$r.squared)

  # the intercept model keeps the centred R-squared
  apc  <- ols_step_all_possible(lm(mpg ~ wt + hp + disp, mtcars))
  rowc <- apc$result[apc$result$predictors == "wt", ]
  expect_equal(rowc$rsquare, summary(lm(mpg ~ wt, mtcars))$r.squared)
  expect_false(isTRUE(all.equal(row$rsquare, rowc$rsquare)))

  # the beta table is not defined without an intercept row to index by
  expect_error(
    ols_step_all_possible_betas(m),
    "requires a model with an intercept"
  )
})

test_that("the beta table rejects weighted models instead of dropping weights (H7)", {
  d <- mtcars
  d$w <- seq_len(nrow(d)) / 8
  m <- lm(mpg ~ wt + hp, d, weights = w)

  expect_error(
    ols_step_all_possible_betas(m),
    "weighted models are not supported"
  )

  # unit weights are the unweighted fit and stay accepted
  mu <- lm(mpg ~ wt + hp, mtcars, weights = rep(1, 32))
  b  <- ols_step_all_possible_betas(mu)
  full <- b[b$model == max(b$model), ]

  expect_equal(full$predictor, names(coef(mu)), ignore_attr = TRUE)
  expect_equal(full$beta, unname(coef(mu)), ignore_attr = TRUE)

  # the unweighted table's full model is still the input fit
  bu <- ols_step_all_possible_betas(lm(mpg ~ wt + hp, mtcars))
  expect_equal(bu$beta[bu$model == max(bu$model)], unname(coef(lm(mpg ~ wt + hp, mtcars))),
               ignore_attr = TRUE)
})
