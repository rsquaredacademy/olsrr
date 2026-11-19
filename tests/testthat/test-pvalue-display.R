expect_pvalue_bound <- function(expr, count = 1L) {
  out <- capture.output(force(expr))
  expect_equal(sum(grepl("< 0.0001", out, fixed = TRUE)), count)
  expect_false(any(grepl("0.0000", out, fixed = TRUE)))
  invisible(out)
}

test_that("p-value display handles the resolution boundary, zero and missing values", {
  p <- c(0, 1e-300, 0.000049, 0.00005, 0.0000999, 0.0001,
         0.00015, 0.123456, 1, NA_real_, NaN)
  before <- p
  expect_identical(
    format_pvalue(p),
    c(rep("< 0.0001", 5), "0.0001", "0.0001", "0.1235", "1.0000", "NA", "NaN")
  )
  expect_identical(p, before)
})

test_that("normality prints tiny p-values as bounds without changing results", {
  set.seed(2)
  x <- rexp(200)
  w <- capture_warnings(result <- ols_test_normality(x))
  expect_match(w, "p-value is smaller than 7.37e-10")
  before <- result
  out <- expect_pvalue_bound(print(result), 4L)
  expect_identical(result, before)
  expect_identical(result$cramer$p.value, 7.37e-10)
  expect_identical(result$shapiro$p.value, shapiro.test(x)$p.value)
  expect_identical(result$kolmogorv$p.value, nortest::lillie.test(x)$p.value)
  expect_identical(result$anderson$p.value, nortest::ad.test(x)$p.value)
  rule <- nchar(out[1])
  expect_true(all(nchar(out) %in% c(rule, rule + 1L)))
})

test_that("regression prints bounds in ANOVA and coefficient tables", {
  result <- ols_regress(lm(mpg ~ disp + hp + wt, mtcars))
  result$p <- 1e-12
  result$pvalues[] <- 1e-12
  before <- result
  out <- expect_pvalue_bound(print(result), 5L)
  expect_identical(result, before)
  # Every coefficient row ends at the same column.
  start <- grep("Parameter Estimates", out)
  rows <- out[seq.int(start + 4L, length(out) - 1L)]
  expect_length(unique(nchar(rows)), 1L)
})

test_that("ANOVA prints a bound without changing its numeric p-value", {
  result <- ols_anova(lm(mpg ~ disp + hp + wt, mtcars))
  result$p <- 1e-12
  before <- result
  expect_pvalue_bound(print(result))
  expect_identical(result, before)
})

test_that("F, score and Bartlett test summaries print tiny p-value bounds", {
  model <- lm(mpg ~ disp + hp + wt, mtcars)
  results <- list(
    f = ols_test_f(model),
    score = ols_test_score(model),
    bartlett = ols_test_bartlett(mtcars, "mpg", "disp")
  )
  for (name in names(results)) {
    result <- results[[name]]
    field <- if (name == "bartlett") "pval" else "p"
    result[[field]] <- 1e-12
    before <- result
    expect_pvalue_bound(print(result))
    expect_identical(result, before)
  }
})

test_that("every Breusch-Pagan print branch bounds tiny p-values", {
  model <- lm(mpg ~ disp + hp + wt + drat + qsec, mtcars)
  cases <- list(
    list(), list(rhs = TRUE), list(rhs = TRUE, multiple = TRUE),
    list(fitted.values = FALSE, rhs = TRUE, multiple = TRUE),
    list(rhs = TRUE, multiple = TRUE, p.adj = "bonferroni"),
    list(rhs = TRUE, multiple = TRUE, p.adj = "holm"),
    list(rhs = TRUE, multiple = TRUE, p.adj = "sidak"),
    list(vars = "disp"), list(multiple = TRUE, vars = c("disp", "hp")),
    list(multiple = TRUE, vars = c("disp", "hp"), p.adj = "bonferroni"),
    list(multiple = TRUE, vars = c("disp", "hp"), p.adj = "sidak"),
    list(multiple = TRUE, vars = c("disp", "hp"), p.adj = "holm"),
    list(multiple = TRUE, vars = "disp")
  )
  for (args in cases) {
    result <- do.call(ols_test_breusch_pagan, c(list(model), args))
    result$p[] <- 1e-12
    before <- result
    expect_pvalue_bound(print(result), length(result$p))
    expect_identical(result, before)
  }
})

test_that("pure-error ANOVA bounds both regression and lack-of-fit p-values", {
  result <- ols_pure_error_anova(lm(mpg ~ disp, mtcars))
  result$pr <- result$pl <- 1e-12
  before <- result
  expect_pvalue_bound(print(result), 2L)
  expect_identical(result, before)
})

test_that("stepwise selection and significance tables bound tiny p-values", {
  d <- data.frame(predictor = c("tiny", "ordinary"), p = c(1e-12, 0.1234),
                  rsq = c(0.7, 0.6), arsq = c(0.6, 0.5), aic = c(10, 20))
  before <- d
  out <- expect_pvalue_bound(
    ols_stepwise_table(d, d$predictor, d$p, d$rsq, d$arsq, d$aic)
  )
  expect_true(any(grepl("0.1234", out, fixed = TRUE)))
  expect_identical(d, before)
  out <- expect_pvalue_bound(ols_stepwise_table_p(d[1:2], d$predictor, d$p))
  expect_true(any(grepl("0.1234", out, fixed = TRUE)))
  expect_identical(d, before)
})
