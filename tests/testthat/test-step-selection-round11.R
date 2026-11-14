round11_selectors <- c(
  "ols_step_forward_aic", "ols_step_backward_aic", "ols_step_both_aic",
  "ols_step_forward_p", "ols_step_backward_p", "ols_step_both_p"
)

test_that("attributed responses preserve exact offset fits and predictions (P02)", {
  d <- auto
  m <- lm(mpg ~ weight + length + displacement, data = d, offset = displacement / 10)
  for (labelled in c(FALSE, TRUE)) {
    if (labelled) {
      d$mpg <- structure(as.numeric(d$mpg), label = "Mileage", labels = c(low = 12),
                         class = c("haven_labelled", "numeric"))
      m <- lm(mpg ~ weight + length + displacement, data = d, offset = displacement / 10)
    }
    for (nm in round11_selectors) {
      fm <- suppressMessages(get(nm)(m))$model
      ref <- lm(formula(fm), data = d, offset = displacement / 10)
      nd <- d[1:7, ]
      nd$displacement <- nd$displacement + 100
      expect_identical(unname(as.numeric(coef(fm))), unname(as.numeric(coef(ref))))
      expect_identical(unname(as.numeric(fm$fitted.values)), unname(as.numeric(ref$fitted.values)))
      expect_identical(predict(fm, newdata = nd), predict(ref, newdata = nd))
    }
  }
})

round11_tie_data <- function() {
  set.seed(123)
  n <- 100
  y <- scale(rnorm(n))
  e1 <- scale(resid(lm(rnorm(n) ~ y)))
  e2 <- scale(resid(lm(rnorm(n) ~ y + e1)))
  r <- 0.35
  x1 <- as.numeric(r * y + sqrt(1 - r^2) * e1)
  a <- 0.9 * sqrt(1 - r^2)
  b <- sqrt(1 - r^2 - a^2)
  x2 <- as.numeric(r * y + a * e1 + b * e2)
  data.frame(y = as.numeric(y), x1 = x1, x2 = x2, x3 = rnorm(n))
}

# Rebuild every recorded step independently, including locked terms.
round11_check_history <- function(k, d, initial = character(), direction = "forward") {
  terms <- initial
  expect_identical(k$metrics$step, seq_len(nrow(k$metrics)))
  for (i in seq_len(nrow(k$metrics))) {
    variable <- k$metrics$variable[i]
    removal <- direction == "backward" ||
      (direction == "both" && k$metrics$method[i] == "removal")
    before <- terms
    terms <- if (removal) setdiff(terms, variable) else c(terms, variable)
    expect_equal(length(setdiff(union(before, terms), intersect(before, terms))), 1L)
    ref <- lm(reformulate(terms, response = names(d)[1]), data = d)
    expect_equal(k$metrics$r2[i], summary(ref)$r.squared)
    expect_equal(k$metrics$adj_r2[i], summary(ref)$adj.r.squared)
    expect_equal(k$metrics$aic[i], ols_aic(ref))
    expect_equal(k$metrics$sbc[i], ols_sbc(ref))
    expect_equal(k$metrics$sbic[i], ols_sbic(ref, k$others$full_model))
    if ("rmse" %in% names(k$metrics)) {
      expect_equal(k$metrics$rmse[i], sqrt(deviance(ref) / df.residual(ref)))
      expect_equal(k$metrics$mallows_cp[i], ols_mallows_cp(ref, k$others$full_model))
    }
    if (!is.null(k$others$beta_pval)) {
      history <- k$others$beta_pval
      history <- history[history$model == i, ]
      expect_identical(history$predictor, names(coef(ref)))
      expect_equal(history$beta, unname(coef(ref)))
      expect_equal(history$pval, unname(coef(summary(ref))[, "Pr(>|t|)"]))
    }
  }
  expect_identical(attr(terms(k$model), "term.labels"), terms)
}

test_that("forward entry tests one tied candidate at a time (P03)", {
  d <- round11_tie_data()
  m <- lm(y ~ x1 + x2 + x3, data = d)
  expect_identical(car::Anova(lm(y ~ x1, d))$`F value`[1],
                   car::Anova(lm(y ~ x2, d))$`F value`[1])
  k <- ols_step_forward_p(m, p_val = 0.001)
  expect_identical(k$metrics$variable, "x1")
  expect_gt(coef(summary(lm(y ~ x1 + x2, d)))["x2", "Pr(>|t|)"], 0.001)
  round11_check_history(k, d)
  k <- ols_step_both_p(m, p_enter = 0.001)
  expect_identical(k$metrics$variable, "x1")
  round11_check_history(k, d, direction = "both")
})

test_that("tied removals and criterion selectors have one accurate row per step (P03)", {
  # Walsh columns give exactly tied, independent x1/x2 contributions.
  d <- expand.grid(x1 = c(-1, 1), x2 = c(-1, 1), z = c(-1, 1), e = c(-1, 1))
  d <- data.frame(y = d$z * 10 + d$e * 3 + (d$x1 + d$x2) / 4, d[1:3])
  m <- lm(y ~ x1 + x2 + z, d)
  f <- car::Anova(m)$`F value`
  expect_identical(f[1], f[2])
  expect_identical(ols_aic(lm(y ~ x2 + z, d)), ols_aic(lm(y ~ x1 + z, d)))
  k <- ols_step_backward_p(m, p_val = 0.05)
  expect_identical(k$metrics$variable, c("x1", "x2"))
  round11_check_history(k, d, c("x1", "x2", "z"), "backward")
  for (nm in round11_selectors[1:3]) {
    direction <- if (grepl("backward", nm)) "backward" else if (grepl("both", nm)) "both" else "forward"
    k <- suppressMessages(get(nm)(m))
    round11_check_history(k, d, if (direction == "backward") c("x1", "x2", "z") else character(), direction)
  }
  # Identical columns give an exact entry tie for the AIC selectors.
  d <- data.frame(y = mtcars$mpg, x1 = mtcars$wt, x2 = mtcars$wt, z = mtcars$hp)
  m <- lm(y ~ x1 + x2 + z, d)
  for (nm in c("ols_step_forward_aic", "ols_step_both_aic")) {
    k <- get(nm)(m)
    expect_identical(k$metrics$variable[1], "x1")
    round11_check_history(k, d, direction = if (grepl("both", nm)) "both" else "forward")
  }
  # Each generic criterion uses the same single-candidate tie path.
  for (direction in c("forward", "backward", "both")) {
    for (criterion in c("sbc", "sbic", "r2", "adj_r2")) {
      k <- suppressMessages(get(paste0("ols_step_", direction, "_", criterion))(m))
      round11_check_history(k, d, if (direction == "backward") c("x1", "x2", "z") else character(), direction)
    }
  }
})

test_that("aliased candidates end selection without an empty-index crash (P04)", {
  d <- mtcars
  d$x1 <- d$wt
  d$x2 <- -d$wt
  m <- lm(mpg ~ x1 + x2 + hp, data = d)
  for (nm in c("ols_step_forward_p", "ols_step_both_p")) {
    expect_no_warning(k <- get(nm)(m))
    expect_equal(coef(k$model), coef(lm(mpg ~ x1 + hp, d)))
    expect_false("x2" %in% k$metrics$variable)
  }
  d$x1 <- d$x2 <- 1
  m <- lm(mpg ~ x1 + x2, d)
  for (nm in c("ols_step_forward_p", "ols_step_both_p")) {
    expect_error(get(nm)(m), "None of the variables satisfy the criteria for entering the model", fixed = TRUE)
  }
  expect_no_warning(k <- ols_step_backward_p(m))
  expect_equal(nrow(k$metrics), 0L)
  for (nm in c("ols_step_forward_p", "ols_step_backward_p")) {
    expect_no_warning(k <- get(nm)(m, hierarchical = TRUE))
    expect_equal(nrow(k$metrics), 0L)
  }
})

test_that("F ranking uses p then candidate order and ignores missing tests (P03/P04)", {
  expect_identical(ols_step_p_index(c(5, 5, 4), c(.2, .1, .01)), 2L)
  expect_identical(ols_step_p_index(c(5, 5, 4), c(.1, .1, .01)), 1L)
  expect_identical(ols_step_p_index(c(2, 2, 4), c(.4, .3, .01), FALSE), 2L)
  expect_identical(ols_step_p_index(c(NA, 5), c(NA, .1)), 2L)
  expect_identical(ols_step_p_index(c(NA, NA), c(NA, NA)), NA_integer_)
  expect_identical(ols_step_p_index(c(Inf, 5), c(0, .1)), 1L)
  expect_identical(ols_threshold(c(2, 2, NA), "aic"), 1L)
  expect_identical(ols_threshold(c(.5, .5, NA), "rsq"), 1L)
})

test_that("forced predictors are initialized one term per recorded step (P03)", {
  d <- data.frame(y = mtcars$mpg, wt = mtcars$wt, hp = mtcars$hp, qsec = mtcars$qsec)
  m <- lm(y ~ wt + hp + qsec, d)
  k <- ols_step_forward_p(m, include = "qsec")
  expect_identical(k$metrics$variable[1], "qsec")
  round11_check_history(k, d)
})

test_that("both-direction histories name the removed term after locked terms (P03)", {
  d <- stepdata
  m <- lm(y ~ ., d)
  k <- ols_step_both_p(m, include = "x1")
  expect_identical(k$metrics$variable[k$metrics$method == "removal"], "x6")
  round11_check_history(k, d, "x1", "both")
})

test_that("numeric offset identity still rejects cancellation mismatches (P02)", {
  d <- mtcars
  d$a <- d$b <- d$c <- d$o <- 0
  d$a[17] <- -1e15
  d$b[17] <- 1e15
  d$c[17] <- -4.687491
  d$o[17] <- 16.088774
  m <- lm(mpg ~ wt * hp + offset(a) + offset(c) + offset(b), d, offset = o)
  # Reordering the formula offsets changes floating-point cancellation.
  # The stored frozen offset must win; a different fit must never escape.
  m$terms <- terms(mpg ~ wt * hp + offset(a) + offset(b) + offset(c), data = d)
  expect_error(ols_step_final_model(m, "mpg", c("wt", "hp", "wt:hp"), d),
               "cannot reproduce the frozen-offset selection fit exactly", fixed = TRUE)
})
