offset_selectors <- c(
  "ols_step_forward_aic", "ols_step_backward_aic", "ols_step_both_aic",
  "ols_step_forward_p", "ols_step_backward_p", "ols_step_both_p"
)

test_that("returned offset models predict from the original call (P01)", {
  d_train <- mtcars[1:25, ]
  d_test <- mtcars[26:32, ]
  d_same <- d_train
  d_same$disp <- d_same$disp + 1000
  d_same$wt <- d_same$wt * 2

  models <- list(
    lm(mpg ~ wt + hp + qsec, data = d_train, offset = disp * 0.01),
    lm(mpg ~ wt + hp + qsec + offset(log(wt)), data = d_train),
    lm(mpg ~ wt + hp + qsec + offset(log(wt)), data = d_train,
       offset = disp * 0.01)
  )
  for (m in models) {
    for (nm in offset_selectors) {
      fm <- suppressMessages(get(nm)(m))$model
      selected <- attr(terms(fm), "term.labels")
      off <- as.list(attr(terms(m), "variables"))[-1][attr(terms(m), "offset")]
      rhs <- c(selected, vapply(off, function(x) paste(deparse(x), collapse = ""), character(1)))
      ref_call <- m$call
      ref_call$formula <- reformulate(rhs, response = "mpg")
      ref <- eval(ref_call)

      expect_length(predict(fm, newdata = d_test), 7L)
      expect_identical(predict(fm, newdata = d_test), predict(ref, newdata = d_test))
      expect_identical(predict(fm, newdata = d_same), predict(ref, newdata = d_same))
      expect_identical(coef(fm), coef(ref))
      expect_identical(fm$fitted.values, ref$fitted.values)
      expect_identical(fm$offset, model.offset(m$model))
      expect_true(identical(fm$call[[1]], quote(lm)) ||
                    identical(fm$call[[1]], quote(stats::lm)))
      expect_identical(fm$call$data, quote(d_train))
      expect_identical(fm$call$offset, m$call$offset)
      expect_length(attr(terms(fm), "offset"), length(off))
      printed <- capture.output(print(fm))
      expect_lt(length(printed), 20L)
      expect_false(any(grepl("function|list\\(|c\\(", printed)))
    }
  }
})

test_that("returned offset calls retain subsets, NA actions and resolved contrasts (P01)", {
  d <- transform(mtcars, g = factor(cyl), h = factor(am), w = 1)
  d$mpg[3] <- NA
  m <- lm(mpg ~ g + h + wt + hp + offset(log(wt)), data = d,
          offset = disp * 0.01, subset = hp > 100, weights = w,
          na.action = na.exclude, contrasts = list(g = "contr.sum", h = "contr.helmert"))
  old <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(old), add = TRUE)
  for (nm in offset_selectors) {
    fm <- suppressMessages(get(nm)(m, include = c("g", "wt"), exclude = c("h", "hp")))$model
    ref <- lm(mpg ~ g + wt + offset(log(wt)), data = d, offset = disp * 0.01,
              subset = hp > 100, weights = w, na.action = na.exclude,
              contrasts = list(g = contr.sum(levels(d$g))))
    for (arg in c("data", "offset", "subset", "weights", "na.action")) {
      expect_identical(fm$call[[arg]], m$call[[arg]])
    }
    expect_named(fm$call$contrasts, "g")
    expect_true(is.matrix(fm$call$contrasts$g))
    expect_identical(coef(fm), coef(ref))
    expect_identical(fm$fitted.values, ref$fitted.values)
    newdata <- transform(mtcars, g = factor(cyl), h = factor(am))
    expect_identical(predict(fm, newdata = newdata), predict(ref, newdata = newdata))
  }
})

test_that("offset final models must reproduce the frozen selection fit exactly (P01)", {
  d <- mtcars
  d$qsec[3] <- NA
  m <- lm(mpg ~ wt + hp + qsec, data = d, offset = disp * 0.01)
  # Dropping qsec would restore a row that selection did not use. Refuse the
  # changed fit rather than returning a model with different coefficients.
  for (nm in offset_selectors) {
    expect_error(suppressMessages(get(nm)(m, include = c("wt", "hp"), exclude = "qsec")),
                 "cannot reproduce the frozen-offset selection fit exactly", fixed = TRUE)
  }
})
