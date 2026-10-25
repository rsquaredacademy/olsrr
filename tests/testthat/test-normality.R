test_that("normality tests skip Shapiro-Wilk for samples larger than 5000", {
  model <- lm(y ~ ., data = stepdata)

  w <- capture_warnings(nt <- ols_test_normality(model))

  expect_length(w, 1L)
  expect_match(
    w,
    "Shapiro-Wilk test skipped: it requires between 3 and 5000 observations \\(n = 20000\\)\\."
  )
  expect_true(is.na(nt$shapiro$statistic))
  expect_true(is.na(nt$shapiro$p.value))
  expect_s3_class(nt$shapiro, "htest")
  expect_true(all(c("statistic", "p.value") %in% names(nt$shapiro)))

  r       <- residuals(model)
  ks_ref  <- ks.test(r, "pnorm", mean(r), sd(r))
  cvm_ref <- nortest::cvm.test(r)
  ad_ref  <- nortest::ad.test(r)

  expect_equal(unname(nt$kolmogorv$statistic), unname(ks_ref$statistic))
  expect_equal(nt$kolmogorv$p.value, ks_ref$p.value)
  # nortest::cvm.test(r)$statistic and $p.value
  expect_equal(unname(nt$cramer$statistic), 0.0543500297504443)
  expect_equal(nt$cramer$p.value, 0.44914910398614)
  expect_equal(unname(nt$cramer$statistic), unname(cvm_ref$statistic))
  expect_equal(nt$cramer$p.value, cvm_ref$p.value)
  expect_equal(unname(nt$anderson$statistic), unname(ad_ref$statistic))
  expect_equal(nt$anderson$p.value, ad_ref$p.value)

  # the table shows NA for the skipped test and every line keeps the rule width
  out  <- capture.output(print(nt))
  rule <- grep("^-+$", out, value = TRUE)
  expect_equal(nchar(out), rep(nchar(rule[1]), length(out)))
  expect_true(any(grepl("^\\s*Shapiro-Wilk\\s+NA\\s+NA\\s*$", out)))
})

test_that("normality tests skip Anderson-Darling and Cramer-von Mises below 8", {
  y <- c(1, 2, 3, 4, 5, 6)

  w <- capture_warnings(nt <- ols_test_normality(y))

  expect_length(w, 1L)
  expect_match(
    w,
    "Anderson-Darling test skipped: it requires at least 8 observations \\(n = 6\\)\\."
  )
  expect_match(
    w,
    "Cramer-von Mises test skipped: it requires at least 8 observations \\(n = 6\\)\\."
  )
  expect_true(is.na(nt$anderson$statistic))
  expect_true(is.na(nt$anderson$p.value))
  expect_true(is.na(nt$cramer$statistic))
  expect_true(is.na(nt$cramer$p.value))

  ks_ref  <- ks.test(y, "pnorm", mean(y), sd(y))
  sw_ref  <- shapiro.test(y)

  expect_equal(unname(nt$kolmogorv$statistic), unname(ks_ref$statistic))
  expect_equal(nt$kolmogorv$p.value, ks_ref$p.value)
  expect_equal(unname(nt$shapiro$statistic), unname(sw_ref$statistic))
  expect_equal(nt$shapiro$p.value, sw_ref$p.value)
})

test_that("normality tests error when the sample is too small for every test", {
  expect_error(
    ols_test_normality(c(1, 2)),
    "at least 3 observations are required"
  )
})

test_that("Cramer-von Mises test compares the data with the normal distribution", {
  set.seed(1)
  x <- rnorm(200, 50, 10)

  nt  <- ols_test_normality(x)
  ref <- nortest::cvm.test(x)

  # nortest::cvm.test(x)$statistic and $p.value
  expect_equal(unname(nt$cramer$statistic), 0.0559153804861608)
  expect_equal(nt$cramer$p.value, 0.426465381171239)
  expect_equal(unname(nt$cramer$statistic), unname(ref$statistic))
  expect_equal(nt$cramer$p.value, ref$p.value)
  expect_match(nt$cramer$method, "Cramer-von Mises normality test")
  expect_gt(nt$cramer$p.value, 0.05)
})

test_that("Cramer-von Mises test rejects clearly non-normal data", {
  set.seed(2)
  x <- rexp(200)

  # nortest::cvm.test() caps the p-value at 7.37e-10 and warns about it
  w <- capture_warnings(nt <- ols_test_normality(x))
  expect_match(w, "p-value is smaller than 7.37e-10")

  ref <- suppressWarnings(nortest::cvm.test(x))

  # nortest::cvm.test(x)$statistic and $p.value
  expect_equal(unname(nt$cramer$statistic), 1.40372310452857)
  expect_equal(nt$cramer$p.value, 7.37e-10)
  expect_equal(unname(nt$cramer$statistic), unname(ref$statistic))
  expect_equal(nt$cramer$p.value, ref$p.value)
  expect_lt(nt$cramer$p.value, 0.05)
})

test_that("Cramer-von Mises test for lm uses the residuals and the normal null", {
  model <- lm(mpg ~ disp + hp + wt, mtcars)
  r     <- residuals(model)

  nt  <- ols_test_normality(model)
  ref <- nortest::cvm.test(r)

  # nortest::cvm.test(residuals(model))$statistic and $p.value
  expect_equal(unname(nt$cramer$statistic), 0.0830502390092736)
  expect_equal(nt$cramer$p.value, 0.182035579731195)
  expect_equal(unname(nt$cramer$statistic), unname(ref$statistic))
  expect_equal(nt$cramer$p.value, ref$p.value)
  expect_match(nt$cramer$method, "Cramer-von Mises normality test")
})

test_that("Cramer-von Mises test is deterministic", {
  set.seed(42)
  x <- rnorm(150)

  # nothing may depend on the RNG state: no seed between the two calls
  first  <- ols_test_normality(x)$cramer
  second <- ols_test_normality(x)$cramer

  expect_identical(first$statistic, second$statistic)
  expect_identical(first$p.value, second$p.value)
})
