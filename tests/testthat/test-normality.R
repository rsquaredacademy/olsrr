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
  cvm_ref <- goftest::cvm.test(r)
  ad_ref  <- nortest::ad.test(r)

  expect_equal(unname(nt$kolmogorv$statistic), unname(ks_ref$statistic))
  expect_equal(nt$kolmogorv$p.value, ks_ref$p.value)
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

test_that("normality tests skip Anderson-Darling for samples smaller than 8", {
  y <- c(1, 2, 3, 4, 5, 6)

  w <- capture_warnings(nt <- ols_test_normality(y))

  expect_length(w, 1L)
  expect_match(
    w,
    "Anderson-Darling test skipped: it requires at least 8 observations \\(n = 6\\)\\."
  )
  expect_true(is.na(nt$anderson$statistic))
  expect_true(is.na(nt$anderson$p.value))

  ks_ref  <- ks.test(y, "pnorm", mean(y), sd(y))
  cvm_ref <- goftest::cvm.test(y)
  sw_ref  <- shapiro.test(y)

  expect_equal(unname(nt$kolmogorv$statistic), unname(ks_ref$statistic))
  expect_equal(nt$kolmogorv$p.value, ks_ref$p.value)
  expect_equal(unname(nt$cramer$statistic), unname(cvm_ref$statistic))
  expect_equal(nt$cramer$p.value, cvm_ref$p.value)
  expect_equal(unname(nt$shapiro$statistic), unname(sw_ref$statistic))
  expect_equal(nt$shapiro$p.value, sw_ref$p.value)
})

test_that("normality tests error when the sample is too small for every test", {
  expect_error(
    ols_test_normality(c(1, 2)),
    "at least 3 observations are required"
  )
})
