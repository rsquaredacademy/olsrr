context("bp_test")

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
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
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
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})

test_that("when rhs == TRUE and multiple == TRUE, multiple p values are\n\treturned", {
  k <- ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)

  expect_equivalent(round(k$bp, 4), c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890))
  expect_equivalent(round(k$p, 4), c(0.3365, 0.3817, 0.3787, 0.3786, 0.2560, 0.7781))

  expect_true(k$fv)
  expect_true(k$rhs)
  expect_true(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'bonferroni'
	bonferroni adjusted p values are returned", {
  k <- ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = "bonferroni")

  expect_equivalent(round(k$bp, 4), c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890))
  expect_equivalent(round(k$p, 4), c(1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 0.7781))

  expect_true(k$fv)
  expect_true(k$rhs)
  expect_true(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "bonferroni")
  expect_match(k$resp, "mpg")
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'holm',
	bonferroni adjusted p values are returned", {
  k <- ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = "holm")

  expect_equivalent(round(k$bp, 4), c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890))
  expect_equivalent(round(k$p, 4), c(1.0000, 0.3817, 0.7574, 1.0000, 1.0000, 0.7781))

  expect_true(k$fv)
  expect_true(k$rhs)
  expect_true(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "holm")
  expect_match(k$resp, "mpg")
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})


test_that("when rhs == TRUE, multiple == TRUE and p.adj == 'sidak',
	bonferroni adjusted p values are returned", {
  k <- ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = "sidak")

  expect_equivalent(round(k$bp, 4), c(0.9237, 0.7652, 0.7749, 0.7751, 1.2903, 2.4890))
  expect_equivalent(round(k$p, 4), c(0.8714, 0.9096, 0.9074, 0.9074, 0.7720, 0.7781))

  expect_true(k$fv)
  expect_true(k$rhs)
  expect_true(k$multiple)

  expect_output(k$vars, NA)

  expect_match(k$padj, "sidak")
  expect_match(k$resp, "mpg")
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
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
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})

test_that("when rhs == TRUE and vars != NA, variables specified\n\tused for the test", {
  k <- ols_test_breusch_pagan(model, vars = c("disp", "hp"), rhs = TRUE)

  expect_equal(round(k$bp, 4), 2.489)
  expect_equal(round(k$p, 4), 0.7781)

  expect_false(k$fv)
  expect_true(k$rhs)
  expect_false(k$multiple)

  expect_equivalent(k$vars, c("disp", "hp"))

  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})


test_that("when rhs == FALSE, multiple == TRUE and vars != NA,\n\tvariables specified are used for the test", {
  k <- ols_test_breusch_pagan(model, multiple = TRUE, rhs = FALSE, vars = c("disp", "hp"))

  expect_equivalent(round(k$bp, 4), c(0.9237, 0.7652, 0.9588))
  expect_equivalent(round(k$p, 4), c(0.3365, 0.3817, 0.6192))

  expect_false(k$fv)
  expect_false(k$rhs)
  expect_true(k$multiple)

  expect_equivalent(k$vars, c("disp", "hp"))

  expect_match(k$padj, "none")
  expect_match(k$resp, "mpg")
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})


test_that("when multiple == TRUE and vars != NA and p.adj == 'bonferroni',
	variables specified are used for the test", {
  k <- ols_test_breusch_pagan(
    model, multiple = TRUE, vars = c("disp", "hp"),
    p.adj = "bonferroni"
  )

  expect_equivalent(round(k$bp, 4), c(0.9237, 0.7652, 0.9588))
  expect_equivalent(round(k$p, 4), c(0.6730, 0.7634, 0.6192))

  expect_false(k$fv)
  expect_false(k$rhs)
  expect_true(k$multiple)

  expect_equivalent(k$vars, c("disp", "hp"))

  expect_match(k$padj, "bonferroni")
  expect_match(k$resp, "mpg")
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})


test_that("when multiple == TRUE and vars != NA and p.adj == 'sidak',
	variables specified are used for the test", {
  k <- ols_test_breusch_pagan(model, multiple = TRUE, vars = c("disp", "hp"), p.adj = "sidak")


  expect_equivalent(round(k$bp, 4), c(0.9237, 0.7652, 0.9588))
  expect_equivalent(round(k$p, 4), c(0.5598, 0.6177, 0.6192))

  expect_false(k$fv)
  expect_false(k$rhs)
  expect_true(k$multiple)

  expect_equivalent(k$vars, c("disp", "hp"))

  expect_match(k$padj, "sidak")
  expect_match(k$resp, "mpg")
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})


test_that("when multiple == TRUE and vars != NA and p.adj == 'holm',
	variables specified are used for the test", {
  k <- ols_test_breusch_pagan(
    model, multiple = TRUE,
    vars = c("disp", "hp"), p.adj = "holm"
  )

  expect_equivalent(round(k$bp, 4), c(0.9237, 0.7652, 0.9588))
  expect_equivalent(round(k$p, 4), c(0.6730, 0.3817, 0.6192))

  expect_false(k$fv)
  expect_false(k$rhs)
  expect_true(k$multiple)

  expect_equivalent(k$vars, c("disp", "hp"))

  expect_match(k$padj, "holm")
  expect_match(k$resp, "mpg")
  expect_equivalent(k$preds, c("disp", "hp", "wt", "drat", "qsec"))
})



