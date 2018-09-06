context("utility")

test_that("output from text formatting matches the expected result", {
  expect_equivalent(fg(3, 10), "         3")
  expect_equivalent(fw(3, 10), "         3")
  expect_equivalent(fs(), "  ")
  expect_equivalent(fl(3, 10), "3         ")
  expect_equivalent(fc(3, 10), "    3     ")
  expect_equivalent(formatter_t(3, 10), "         3")
  expect_equivalent(formatter_n(3.7589, 10), "  3.7589  ")
  expect_equivalent(format_cil(3, 10), "  3.000   ")
  expect_equivalent(format_ciu(3, 10), "  3.000   ")
  expect_equivalent(formats_t(), "  ")
})

test_that("output from l matches the expected result", {
  expect_equal(l(deparse(substitute(mtcars$mpg))), "mpg")
  expect_equal(l(deparse(substitute(mpg))), "mpg")
  expect_equal(l(deparse(substitute(mtcars@mpg))), "mtcars@mpg")
})

test_that("output from cpout matches the expected result", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- cpout(model)
  expect_equivalent(k$e, residuals(model))
  expect_equivalent(k$mc, model$coefficients[-1])
  expect_equivalent(as.data.frame(k$data), model.frame(model)[-1])
  expect_equal(k$lmc, length(model$coefficients[-1]))
  expect_equivalent(k$nam, names(model.frame(model)[-1]))
  expect_equivalent(k$indvar, names(model.frame(model)[1]))
})

test_that("output from cpdata matches the expected result", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- cpout(model)
  actual <- cpdata(mtcars, k$mc, k$e, 2)
  expect_equivalent(
    actual[[1]],
    c(
      6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8, 8,
      8, 8, 4, 4, 4, 8, 6, 8, 4
    )
  )
  expect_equivalent(actual[[2]], c(
    -2.9203498, -2.0330346, -2.3992081,
    0.6259587, 0.3873286, -1.3037619,
    -1.2315502, 1.2850510, 0.3095797, -1.4791933, -2.8791933, 0.9161240,
    0.6330370, -1.2929796, -0.6093035, 0.2660135, 4.4825264, 5.5492117,
    -0.4647590, 5.5258185, -2.8364793, -2.5541073, -3.7860046, -2.1732729,
    2.2677477, -0.4740448, -0.9675111, 3.2790314, -2.2139222, -0.8851877,
    2.2406110, -2.1513153
  ))
})

test_that("output from cdplot matches the expected output", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_prep_cdplot_data(model)
  expect_equal(round(k$maxx, 3), 0.398)
  expect_equal(k$ts, 0.125)
})

test_that("output from dpred matches the expected output", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_prep_dsrvf_data(model)
  expect_equal(round(k$cmaxx, 3), 3.516)
  expect_equal(round(k$cminx, 3), -2.44)
})


test_that("output from ka matches the expected result", {
  expect_equal(round(ka(10, 0.69, 30), 3), -0.326)
})

test_that("output from corrout matches the expected output", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- corrout(model)
  expect_equal(round(k, 3), 0.960)
})

test_that("output from obspred matches the expected output", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- obspred(model)
  expect_equal(mean(k[[1]]), mean(k[[2]]))
})

test_that("output from hadio matches the expected output", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- hadio(model, 1)
  expect_equal(round(mean(k), 3), 0.412)
})

test_that("output from rsdata matches the expected output", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_prep_rfsplot_rsdata(model)
  expect_equal(round(mean(k$x), 3), 0.516)
  expect_equal(round(max(k$y), 3), 5.688)
})

test_that("output from fmdata matches the expected output", {

  skip_on_cran()

  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_prep_rfsplot_fmdata(model)
  expect_equal(round(mean(k$x), 3), 0.517)
  expect_equal(round(max(k$y), 3), 10.635)
})

test_that("output from rvspdata matches the expected output", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- rvspdata(model)
  expect_equal(round(mean(k$predicted), 3), 20.091)
  expect_equal(round(median(k$resid), 3), -0.506)
})

test_that("output from rvsrdata matches the expected output", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_prep_rvsrplot_data(model)
  expect_equal(k$np, 4)
  expect_equivalent(k$pnames, c("disp", "hp", "wt", "drat"))
  expect_equal(round(mean(k$dat[[1]]), 3), 230.722)
})

test_that("output from srdata matches the expected result", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_prep_srplot_data(model)
  expect_equal(k$cmaxx, 3)
  expect_equal(k$cminx, -3)
  expect_equivalent(k$nseq, c(-1, -2))
  expect_equivalent(k$pseq, c(1, 2))
})

test_that("output from histdata matches the expected result", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- histdata(model)
  expect_equal(round(k$maxx, 3), 6.688)
  expect_equal(round(k$minx, 3), -4.508)
})


test_that("output from rstudlev matches the expected result", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  k <- ols_prep_rstudlev_data(model)
  expect_equal(round(k$lev_thrsh, 3), 0.312)
  expect_equal(round(k$minx, 3), 0.046)
  expect_equal(round(k$miny, 3), -4.44)
  expect_equal(round(k$maxx, 3), 0.508)
  expect_equal(round(k$maxy, 3), 5.516)
})

test_that("output from corrout matches the expected result", {
  model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
  expect_equal(round(corrout(model), 3), 0.96)
})

test_that("output from advarx matches the expected output", {
  actual <- ols_prep_regress_x(mtcars, 1)

  expected <- lm(mpg ~., data = mtcars) %>%
    residuals() %>%
    unname()

  expect_equivalent(actual, expected)
})
