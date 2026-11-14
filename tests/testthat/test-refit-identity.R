refit_selectors <- c(
  "ols_step_forward_aic", "ols_step_backward_aic", "ols_step_both_aic",
  "ols_step_forward_p", "ols_step_backward_p", "ols_step_both_p"
)

test_that("custom named contrasts reject before refitting (O01)", {
  # model.matrix() looks up character contrast specifications on the search path.
  had_contrast <- exists("mycon", envir = globalenv(), inherits = FALSE)
  if (had_contrast) old_contrast <- get("mycon", envir = globalenv())
  on.exit(if (had_contrast) {
    assign("mycon", old_contrast, envir = globalenv())
  } else {
    rm("mycon", envir = globalenv())
  }, add = TRUE)
  old_options <- options(contrasts = c("mycon", "contr.poly"))
  on.exit(options(old_options), add = TRUE)
  msg <- "can't verify the custom contrast 'mycon'"

  for (stored_x in c(FALSE, TRUE)) {
    for (missing_data in c(FALSE, TRUE)) {
      for (via_options in c(FALSE, TRUE)) {
        assign("mycon", stats::contr.treatment, envir = globalenv())
        e <- new.env(parent = globalenv())
        e$d <- transform(mtcars, g = factor(cyl))
        e$stored_x <- stored_x
        m <- if (via_options) {
          evalq(lm(mpg ~ g * hp, d, x = stored_x), e)
        } else {
          evalq(lm(mpg ~ g * hp, d, x = stored_x, contrasts = list(g = "mycon")), e)
        }
        original <- model.matrix(m)
        assign("mycon", function(n, contrasts = TRUE, sparse = FALSE) {
          z <- stats::contr.sum(n, contrasts = contrasts, sparse = sparse)
          colnames(z) <- c("6", "8")
          z
        }, envir = globalenv())
        if (!stored_x) {
          expect_identical(colnames(model.matrix(m)), colnames(original))
          expect_false(identical(model.matrix(m), original))
        }
        if (missing_data) rm("d", envir = e)
        before <- serialize(m, NULL)
        expect_error(ols_regress(m), msg, fixed = TRUE)
        for (nm in refit_selectors) {
          expect_error(suppressMessages(get(nm)(m, include = c("g", "hp", "g:hp"))),
                       msg, fixed = TRUE)
        }
        expect_identical(serialize(m, NULL), before)
      }
    }
  }
})

test_that("base named contrasts use stats and explicit matrices stay fixed (O01)", {
  old_options <- options(contrasts = c("contr.treatment", "contr.poly"))
  on.exit(options(old_options), add = TRUE)
  base_names <- c("contr.treatment", "contr.sum", "contr.helmert", "contr.poly", "contr.SAS")
  saved <- lapply(base_names, function(nm) {
    if (exists(nm, envir = globalenv(), inherits = FALSE)) get(nm, envir = globalenv())
  })
  on.exit(for (i in seq_along(base_names)) {
    if (is.null(saved[[i]])) {
      rm(list = base_names[i], envir = globalenv())
    } else {
      assign(base_names[i], saved[[i]], envir = globalenv())
    }
  }, add = TRUE)
  d <- transform(mtcars, g = factor(cyl))
  for (nm in base_names) {
    con <- getExportedValue("stats", nm)(levels(d$g))
    m <- lm(mpg ~ g * hp, d, contrasts = list(g = nm))
    mx <- lm(mpg ~ g * hp, d, contrasts = list(g = nm), x = TRUE)
    explicit <- lm(mpg ~ g * hp, d, contrasts = list(g = con))
    expected <- ols_regress(explicit)$sbetas
    options(contrasts = c("contr.sum", "contr.poly"))
    # A same-named function on the search path must never be evaluated.
    assign(nm, function(...) {
      stop("search-path contrast was called")
    }, envir = globalenv())
    expect_identical(ols_regress(m)$sbetas, expected)
    expect_identical(ols_regress(mx)$sbetas, expected)
    expect_identical(ols_regress(explicit)$sbetas, expected)
    for (fn in refit_selectors) {
      k <- suppressMessages(get(fn)(m, include = c("g", "hp", "g:hp")))
      expect_identical(coef(k$model), coef(m))
      kx <- suppressMessages(get(fn)(mx, include = c("g", "hp", "g:hp")))
      expect_identical(coef(kx$model), coef(mx))
    }
  }
})

test_that("refits preserve the frozen total under offset cancellation (O02)", {
  d <- mtcars
  d$a <- d$b <- d$c <- d$o <- 0
  d$a[17] <- -1e15
  d$b[17] <- 1e15
  d$c[17] <- -4.687491
  d$o[17] <- 16.088774
  m <- lm(mpg ~ wt * hp + offset(a) + offset(c) + offset(b), d, offset = o)
  expect_identical(model.offset(model.frame(m))[17], 11.5)
  before <- m

  ds <- transform(d, wt = as.numeric(scale(wt)), hp = as.numeric(scale(hp)))
  ref <- lm(mpg ~ wt * hp, ds, offset = model.offset(model.frame(m)))
  expected <- coef(ref)[-1] * apply(model.matrix(ref)[, -1, drop = FALSE], 2, sd) /
    sd(m$model$mpg)
  expect_identical(ols_regress(m)$sbetas, expected)
  for (nm in refit_selectors) {
    k <- suppressMessages(get(nm)(m, include = c("wt", "hp", "wt:hp")))
    expect_identical(k$model$offset, model.offset(model.frame(m)))
    expect_identical(coef(k$model), coef(m))
  }
  expect_identical(m, before)

  # Nearby case: cancelling formula offsets without a call offset.
  mf <- lm(mpg ~ wt * hp + offset(a) + offset(c) + offset(b), d)
  rf <- lm(mpg ~ wt * hp, ds, offset = model.offset(model.frame(mf)))
  expected <- coef(rf)[-1] * apply(model.matrix(rf)[, -1, drop = FALSE], 2, sd) /
    sd(mf$model$mpg)
  expect_identical(ols_regress(mf)$sbetas, expected)
  for (nm in refit_selectors) {
    k <- suppressMessages(get(nm)(mf, include = c("wt", "hp", "wt:hp")))
    expect_identical(k$model$offset, model.offset(model.frame(mf)))
    expect_identical(coef(k$model), coef(mf))
  }

  # A returned offset fit must validate again on its fitted rows, including a
  # subset/na.exclude fit whose frozen offset is shorter than the call data.
  d$mpg[3] <- NA
  ms <- lm(mpg ~ wt * hp + offset(qsec), d, offset = drat,
           subset = hp > 100, na.action = na.exclude)
  for (nm in refit_selectors) {
    k <- suppressMessages(get(nm)(ms, include = c("wt", "hp", "wt:hp")))
    expect_identical(k$model$offset, model.offset(model.frame(ms)))
    expect_identical(coef(k$model), coef(ms))
    expect_identical(ols_regress(k$model, iterm = TRUE)$sbetas, ols_regress(ms)$sbetas)
    expect_length(attr(terms(k$model), "offset"), 0L)
  }
})
