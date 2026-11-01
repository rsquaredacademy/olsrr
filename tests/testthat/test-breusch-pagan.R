model <- lm(mpg ~ disp + hp + wt + drat + qsec, data = mtcars)

test_that("when fitted.values == TRUE, fitted values from the regression\n\tare used for the test", {
  k <- ols_test_breusch_pagan(model)

  expect_equal(round(k$bp, 3), 1.256)
  expect_equal(round(k$p, 3), 0.263)

  expect_true(k$fv)
  expect_false(k$rhs)
  expect_false(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})

test_that("when rhs == TRUE, predictors from the regression\n\tare used for the test", {
  k <- ols_test_breusch_pagan(model, rhs = TRUE)

  expect_equal(round(k$bp, 3), 2.489)
  expect_equal(round(k$p, 3), 0.778)

  expect_true(k$fv)
  expect_true(k$rhs)
  expect_false(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})

test_that("when rhs == TRUE and multiple == TRUE, multiple p values are returned", {
  k <- ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)

  expect_equal(round(k$bp, 4), c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890), ignore_attr = TRUE)
  expect_equal(round(k$p, 4), c(0.3365, 0.3817, 0.3787, 0.3786, 0.2560, 0.7781), ignore_attr = TRUE)

  expect_true(k$fv)
  expect_true(k$rhs)
  expect_true(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})

test_that("when fitted.value == FALSE and rhs == TRUE and multiple == TRUE, multiple p values are returned", {
  k <- ols_test_breusch_pagan(model, fitted.values = FALSE, rhs = TRUE, multiple = TRUE)

  expect_equal(round(k$bp, 4), c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890), ignore_attr = TRUE)
  expect_equal(round(k$p, 4), c(0.3365, 0.3817, 0.3787, 0.3786, 0.2560, 0.7781), ignore_attr = TRUE)

  expect_false(k$fv)
  expect_true(k$rhs)
  expect_true(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'bonferroni'
	bonferroni adjusted p values are returned", {
  k <- ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = "bonferroni")

  expect_equal(round(k$bp, 4), c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890), ignore_attr = TRUE)
  expect_equal(round(k$p, 4), c(1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 0.7781), ignore_attr = TRUE)

  expect_true(k$fv)
  expect_true(k$rhs)
  expect_true(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "bonferroni")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'holm',
	bonferroni adjusted p values are returned", {
  k <- ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = "holm")

  expect_equal(round(k$bp, 4), c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890), ignore_attr = TRUE)
  expect_equal(round(k$p, 4), c(1.0000, 0.3817, 0.7574, 1.0000, 1.0000, 0.7781), ignore_attr = TRUE)

  expect_true(k$fv)
  expect_true(k$rhs)
  expect_true(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "holm")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'sidak',
	bonferroni adjusted p values are returned", {
  k <- ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = "sidak")

  expect_equal(round(k$bp, 4), c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890), ignore_attr = TRUE)
  expect_equal(round(k$p, 4), c(0.8714, 0.9096, 0.9074, 0.9074, 0.7720, 0.7781), ignore_attr = TRUE)

  expect_true(k$fv)
  expect_true(k$rhs)
  expect_true(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "sidak")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})

test_that("when vars != NA, variables specified are used for the test", {
  k <- ols_test_breusch_pagan(model, vars = c("disp"))

  expect_equal(round(k$bp, 4), 0.9237)
  expect_equal(round(k$p, 4), 0.3365)

  expect_false(k$fv)
  expect_false(k$rhs)
  expect_false(k$multiple)

  expect_match(k$vars, "disp")
  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})

test_that("when rhs == TRUE and vars != NA, variables specified\n\tused for the test", {
  k <- ols_test_breusch_pagan(model, vars = c("disp", "hp"), rhs = TRUE)

  expect_equal(round(k$bp, 4), 2.489)
  expect_equal(round(k$p, 4), 0.7781)

  expect_false(k$fv)
  expect_true(k$rhs)
  expect_false(k$multiple)

  expect_equal(k$vars, c("disp", "hp"), ignore_attr = TRUE)

  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when rhs == FALSE, multiple == TRUE and vars != NA,\n\tvariables specified are used for the test", {
  k <- ols_test_breusch_pagan(model, multiple = TRUE, rhs = FALSE, vars = c("disp", "hp"))

  expect_equal(round(k$bp, 4), c(0.9237, 0.7652, 0.9588), ignore_attr = TRUE)
  expect_equal(round(k$p, 4), c(0.3365, 0.3817, 0.6192), ignore_attr = TRUE)

  expect_false(k$fv)
  expect_false(k$rhs)
  expect_true(k$multiple)

  expect_equal(k$vars, c("disp", "hp"), ignore_attr = TRUE)

  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when multiple == TRUE and vars != NA and p.adj == 'bonferroni',
	variables specified are used for the test", {
  k <- ols_test_breusch_pagan(
    model, multiple = TRUE, vars = c("disp", "hp"),
    p.adj = "bonferroni"
  )

  expect_equal(round(k$bp, 4), c(0.9237, 0.7652, 0.9588), ignore_attr = TRUE)
  expect_equal(round(k$p, 4), c(0.6730, 0.7634, 0.6192), ignore_attr = TRUE)

  expect_false(k$fv)
  expect_false(k$rhs)
  expect_true(k$multiple)

  expect_equal(k$vars, c("disp", "hp"), ignore_attr = TRUE)

  expect_match(k$padj, "bonferroni")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when multiple == TRUE and vars != NA and p.adj == 'sidak',
	variables specified are used for the test", {
  k <- ols_test_breusch_pagan(model, multiple = TRUE, vars = c("disp", "hp"), p.adj = "sidak")


  expect_equal(round(k$bp, 4), c(0.9237, 0.7652, 0.9588), ignore_attr = TRUE)
  expect_equal(round(k$p, 4), c(0.5598, 0.6177, 0.6192), ignore_attr = TRUE)

  expect_false(k$fv)
  expect_false(k$rhs)
  expect_true(k$multiple)

  expect_equal(k$vars, c("disp", "hp"), ignore_attr = TRUE)

  expect_match(k$padj, "sidak")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})


test_that("when multiple == TRUE and vars != NA and p.adj == 'holm',
	variables specified are used for the test", {
  k <- ols_test_breusch_pagan(
    model, multiple = TRUE,
    vars = c("disp", "hp"), p.adj = "holm"
  )

  expect_equal(round(k$bp, 4), c(0.9237, 0.7652, 0.9588), ignore_attr = TRUE)
  expect_equal(round(k$p, 4), c(0.6730, 0.3817, 0.6192), ignore_attr = TRUE)

  expect_false(k$fv)
  expect_false(k$rhs)
  expect_true(k$multiple)

  expect_equal(k$vars, c("disp", "hp"), ignore_attr = TRUE)

  expect_match(k$padj, "holm")
  expect_match(k$resp, "mpg")
  expect_equal(k$preds, c("disp", "hp", "wt", "drat", "qsec"), ignore_attr = TRUE)
})

test_that("vars = NULL uses the fitted values test", {
  k <- ols_test_breusch_pagan(model, vars = NULL)
  b <- ols_test_breusch_pagan(model)

  expect_equal(k$bp, b$bp)
  expect_equal(k$df, b$df)
  expect_equal(k$p, b$p)
  expect_true(k$fv)
})




test_that("fitted.values = FALSE without vars or rhs throws an error", {
  err_msg <- "Specify `vars`, or set `fitted.values = TRUE` or `rhs = TRUE`."
  expect_error(ols_test_breusch_pagan(model, fitted.values = FALSE, vars = NULL), err_msg)
  expect_error(ols_test_breusch_pagan(model, fitted.values = FALSE, vars = NA), err_msg)
  expect_error(ols_test_breusch_pagan(model, fitted.values = FALSE, vars = character(0)), err_msg)
  expect_error(ols_test_breusch_pagan(model, fitted.values = FALSE), err_msg)

  expect_no_error(ols_test_breusch_pagan(model, fitted.values = FALSE, rhs = TRUE))
  expect_no_error(ols_test_breusch_pagan(model))
})

test_that("na.exclude and na.omit fits give the same Breusch-Pagan test (M5)", {
  d <- mtcars
  d$mpg[1] <- NA
  me <- lm(mpg ~ disp + hp + wt, d, na.action = na.exclude)
  mo <- lm(mpg ~ disp + hp + wt, d, na.action = na.omit)

  expect_equal(ols_test_breusch_pagan(me)$bp, ols_test_breusch_pagan(mo)$bp)
  expect_equal(ols_test_breusch_pagan(me)$p, ols_test_breusch_pagan(mo)$p)
  expect_equal(ols_test_breusch_pagan(me)$bp, 0.5508428, tolerance = 1e-6)

  expect_equal(
    ols_test_breusch_pagan(me, rhs = TRUE)$bp,
    ols_test_breusch_pagan(mo, rhs = TRUE)$bp
  )
  expect_equal(
    ols_test_breusch_pagan(me, vars = c("disp", "hp"))$bp,
    ols_test_breusch_pagan(mo, vars = c("disp", "hp"))$bp
  )
  expect_equal(
    ols_test_breusch_pagan(me, rhs = TRUE, multiple = TRUE)$p,
    ols_test_breusch_pagan(mo, rhs = TRUE, multiple = TRUE)$p
  )
})

test_that("aliased predictors do not inflate the auxiliary degrees of freedom (H8)", {
  d <- mtcars
  d$hp2 <- 2 * d$hp
  m <- lm(mpg ~ wt + hp + hp2, d)

  b <- ols_test_breusch_pagan(m, vars = c("hp", "hp2"))
  expect_equal(b$df, 1)
  expect_equal(b$p, pchisq(b$bp, 1, lower.tail = FALSE))
  expect_equal(b$p, 0.3115296, tolerance = 1e-6)

  # duplicate and overlapping selections are de-duplicated
  b1 <- ols_test_breusch_pagan(m, vars = "hp")
  b2 <- ols_test_breusch_pagan(m, vars = c("hp", "hp"))
  expect_equal(b2$bp, b1$bp)
  expect_equal(b2$df, 1)

  mf <- lm(mpg ~ factor(cyl) + hp, mtcars)
  bf <- ols_test_breusch_pagan(mf, vars = c("factor(cyl)", "factor(cyl)6"))
  expect_equal(bf$df, 2)
  expect_equal(bf$p, pchisq(bf$bp, 2, lower.tail = FALSE))

  # the printed simultaneous row reports the tested dimension
  out  <- capture.output(print(ols_test_breusch_pagan(m, vars = c("hp", "hp2"), multiple = TRUE)))
  sim  <- grep("simultaneous", out, value = TRUE)
  expect_length(sim, 1)
  expect_equal(as.integer(strsplit(trimws(sim), "[[:space:]]+")[[1]][3]), 1L)
})

test_that("an auxiliary design with no tested direction is rejected (H8)", {
  d <- transform(mtcars, z = 1)
  m <- lm(mpg ~ wt + hp + z, d)

  # the audit case: a constant `vars` column supplied no direction beyond the
  # auxiliary intercept, and a zero-degrees-of-freedom tail reported p = 0
  expect_error(
    ols_test_breusch_pagan(m, vars = "z"),
    "no tested direction"
  )

  # every one-at-a-time row needs a direction of its own
  expect_error(
    ols_test_breusch_pagan(m, vars = c("wt", "z"), multiple = TRUE),
    "no tested direction"
  )

  # the non-degenerate neighbours are unchanged
  b <- ols_test_breusch_pagan(m, rhs = TRUE)
  expect_equal(b$df, 2)
  expect_equal(b$p, pchisq(b$bp, 2, lower.tail = FALSE))
  expect_equal(b$bp, 1.0267659, tolerance = 1e-6)

  bz <- ols_test_breusch_pagan(m, vars = "wt")
  expect_equal(bz$df, 1)
  expect_equal(bz$p, pchisq(bz$bp, 1, lower.tail = FALSE))

  # an intercept-only model has no direction to test against its fitted values
  expect_error(
    ols_test_breusch_pagan(lm(mpg ~ 1, mtcars)),
    "no tested direction"
  )
})
