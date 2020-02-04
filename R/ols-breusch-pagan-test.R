#' Breusch pagan test
#'
#' @description
#' Test for constant variance. It assumes that the error terms are normally
#' distributed.
#'
#' @param model An object of class \code{lm}.
#' @param fitted.values Logical; if TRUE, use fitted values of regression model.
#' @param rhs Logical; if TRUE, specifies that tests for heteroskedasticity be
#' performed for the right-hand-side (explanatory) variables of the fitted
#' regression model.
#' @param multiple Logical; if TRUE, specifies that multiple testing be performed.
#' @param p.adj Adjustment for p value, the following options are available:
#' bonferroni, holm, sidak and none.
#' @param vars Variables to be used for heteroskedasticity test.
#'
#' @details
#' Breusch Pagan Test was introduced by Trevor Breusch and Adrian Pagan in 1979.
#' It is used to test for heteroskedasticity in a linear regression model.
#' It test whether variance of errors from a regression is dependent on the
#' values of a independent variable.
#'
#' \itemize{
#' \item Null Hypothesis: Equal/constant variances
#' \item Alternative Hypothesis: Unequal/non-constant variances
#' }
#'
#' Computation
#'
#' \itemize{
#'   \item Fit a regression model
#'   \item Regress the squared residuals from the above model on the independent variables
#'   \item Compute \eqn{nR^2}. It follows a chi square distribution with p -1 degrees of
#'         freedom, where p is the number of independent variables, n is the sample size and
#' 				 \eqn{R^2} is the coefficient of determination from the regression in step 2.
#' }
#'
#' @return \code{ols_test_breusch_pagan} returns an object of class \code{"ols_test_breusch_pagan"}.
#' An object of class \code{"ols_test_breusch_pagan"} is a list containing the
#' following components:
#'
#' \item{bp}{breusch pagan statistic}
#' \item{p}{p-value of \code{bp}}
#' \item{fv}{fitted values of the regression model}
#' \item{rhs}{names of explanatory variables of fitted regression model}
#' \item{multiple}{logical value indicating if multiple tests should be performed}
#' \item{padj}{adjusted p values}
#' \item{vars}{variables to be used for heteroskedasticity test}
#' \item{resp}{response variable}
#' \item{preds}{predictors}
#'
#' @references
#' T.S. Breusch & A.R. Pagan (1979), A Simple Test for Heteroscedasticity and
#' Random Coefficient Variation. Econometrica 47, 1287–1294
#'
#' Cook, R. D.; Weisberg, S. (1983). "Diagnostics for Heteroskedasticity in Regression". Biometrika. 70 (1): 1–10.
#'
#' @section Deprecated Function:
#' \code{ols_bp_test()} has been deprecated. Instead use \code{ols_test_breusch_pagan()}.
#'
#' @family heteroskedasticity tests
#'
#' @examples
#' # model
#' model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
#'
#' # use fitted values of the model
#' ols_test_breusch_pagan(model)
#'
#' # use independent variables of the model
#' ols_test_breusch_pagan(model, rhs = TRUE)
#'
#' # use independent variables of the model and perform multiple tests
#' ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)
#'
#' # bonferroni p value adjustment
#' ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'bonferroni')
#'
#' # sidak p value adjustment
#' ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'sidak')
#'
#' # holm's p value adjustment
#' ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'holm')
#'
#' @importFrom stats anova
#'
#' @export
#'
ols_test_breusch_pagan <- function(model, fitted.values = TRUE, rhs = FALSE, multiple = FALSE,
                        p.adj = c("none", "bonferroni", "sidak", "holm"), vars = NA) UseMethod("ols_test_breusch_pagan")

#' @export
#'
ols_test_breusch_pagan.default <- function(model, fitted.values = TRUE, rhs = FALSE, multiple = FALSE,
                                p.adj = c("none", "bonferroni", "sidak", "holm"), vars = NA) {

  check_model(model)
  check_logic(fitted.values)
  check_logic(rhs)
  check_logic(multiple)

  suppressWarnings(
    if (!is.na(vars[1])) {
      check_modelvars(model, vars)
      fitted.values <- FALSE
    }
  )

  method <- match.arg(p.adj)
  p.adj <- match.arg(p.adj)
  check_options(p.adj)
  l      <- ols_prep_avplot_data(model)
  n      <- nrow(l)

  response <-
    l %>%
    names() %>%
    extract(1)

  predictors <-
    l %>%
    names() %>%
    extract(-1)

  if (fitted.values) {
    vars <- NA

    if (rhs) {
      if (multiple) {

        start <- bp_case_one(l, model)
        loops <- bp_case_loop(start$np, start$nam, start$l)
        inter <- bp_case_inter(start$l, start$np, loops$tstat)
        bp    <- inter$bp
        p     <- bp_case_adj(method, loops$pvals, start$np, inter$ps)

      } else {
        result <- bp_case_2(l, model)
        bp     <- result$bp
        p      <- pchisq(bp, df = result$df, lower.tail = FALSE)
      }
    } else {
      bp <- bp_case_3(model)
      p  <- pchisq(bp, df = 1, lower.tail = FALSE)
    }
  } else {
    if (multiple) {
      if (rhs) {
        start <- bp_case_one(l, model)
        loops <- bp_case_loop(start$np, start$nam, start$l)
        inter <- bp_case_inter(start$l, start$np, loops$tstat)
        bp    <- inter$bp
        p     <- bp_case_adj(method, loops$pvals, start$np, inter$ps)

      } else {
        len_vars <- length(vars)

        if (len_vars > 1) {
          start   <- bp_case_one(l, model)
          len_var <- length(vars)
          loops   <- bp_case_loop(len_var, vars, start$l)
          inter   <- bp_case_5_inter(l, model, vars, loops$tstat)
          bp      <- inter$bp
          p       <- bp_case_adj(method, loops$pvals, inter$np, inter$ps)
        } else {
          result <- bp_case_7(l, model, vars)
          bp     <- result$bp
          p      <- pchisq(bp, df = result$df, lower.tail = FALSE)
        }
      }
    } else {
      if (rhs) {
        result <- bp_case_6(l, model)
        bp     <- result$bp
        p      <- pchisq(bp, df = result$df, lower.tail = FALSE)
      } else {
        result <- bp_case_7(l, model, vars)
        bp     <- result$bp
        p      <- pchisq(bp, df = result$df, lower.tail = FALSE)
      }
    }
  }

  out <- list(
    bp       = bp,
    p        = p,
    fv       = fitted.values,
    rhs      = rhs,
    multiple = multiple,
    padj     = method,
    vars     = vars,
    resp     = response,
    preds    = predictors
  )

  class(out) <- "ols_test_breusch_pagan"

  return(out)
}

#' @export
#' @rdname ols_test_breusch_pagan
#' @usage NULL
#'
ols_bp_test <- function(model, fitted.values = TRUE, rhs = FALSE, multiple = FALSE,
                        p.adj = c("none", "bonferroni", "sidak", "holm"), vars = NA) {
  .Deprecated("ols_test_breusch_pagan()")
}


#' @export
#'
print.ols_test_breusch_pagan <- function(x, ...) {
  print_bp_test(x)
}

#' @description
#' Computes breusch pagan statistics and degrees of freedom when:
#' * fit = TRUE
#' * rhs = TRUE
#' * multiple = TRUE
#'
#' @param l A \code{data.frame} created using `avplots_data()`.
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
bp_case_2 <- function(l, model) {

  n         <- model_rows(model)
  var_resid <- residual_var(model, n)
  ind       <- ind_bp(model, var_resid)

  df <-
    l %>%
    select(-1) %>%
    ncol()

  l %<>%
    select(-1) %>%
    bind_cols(ind)

  bp <- bp_model(l)

  list(bp = bp, df = df)

}

#' @description
#' Computes breusch pagan statistics and degrees of freedom when:
#' * fit = TRUE
#' * rhs = FALSE
#'
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
bp_case_3 <- function(model) {

  `Sum Sq`     <- NULL
  pred         <- fitted(model)
  scaled_resid <- resid_scaled(model, pred)

  lm(scaled_resid ~ pred) %>%
    anova() %>%
    use_series(`Sum Sq`) %>%
    extract(1) %>%
    divide_by(2)

}

#' @description
#' Computes breusch pagan statistics and degrees of freedom when:
#' * fit = FALSE
#' * rhs = TRUE
#' * multiple = FALSE
#'
#' @param l A \code{data.frame} created using `avplots_data()`.
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
bp_case_6 <- function(l, model) {

  n         <- nrow(l)
  var_resid <- residual_var(model, n)
  ind       <- ind_bp(model, var_resid)

  np <-
    l %>%
    names() %>%
    extract(-1) %>%
    length()

  bp <-
    l %>%
    bind_cols(ind) %>%
    select(-1) %>%
    bp_model()

  list(bp = bp, df = np)

}

#' @description
#' Computes breusch pagan statistics and degrees of freedom when:
#' * fit = FALSE
#' * rhs = FALSE
#' * multiple = FALSE
#'
#' @importFrom rlang !!! syms
#' @importFrom magrittr subtract
#'
#' @param l A \code{data.frame} created using `avplots_data()`.
#' @param model An object of class \code{lm}.
#' @param vars Variables to be used for the test.
#'
#' @noRd
#'
bp_case_7 <- function(l, model, vars) {

  n         <- nrow(l)
  var_resid <- residual_var(model, n)
  ind       <- ind_bp(model, var_resid)

  l %<>%
    select(!!! syms(vars)) %>%
    bind_cols(ind)

  bp <- bp_model(l)

  nd <-
    l %>%
    ncol() %>%
    subtract(1)

  list(bp = bp, df = nd)

}

#' @description Fit model using the columns in the data set.
#'
#' @param l A \code{data.frame} created using `avplots_data()`.
#'
#' @noRd
#'
bp_model <- function(l) {

  l %>%
    lm(ind ~ ., data = .) %>%
    bp_fit()

}

bp_fit <- function(l) {

  l %>%
    fitted() %>%
    raise_to_power(2) %>%
    sum() %>%
    divide_by(2)

}

ind_bp <- function(model, var_resid) {

  model %>%
    residuals() %>%
    raise_to_power(2) %>%
    divide_by(var_resid) %>%
    subtract(1) %>%
    data.frame() %>%
    set_colnames("ind")

}

#' @description
#' Computes breusch pagan statistics and degrees of freedom when:
#' * fit = TRUE
#' * rhs = TRUE
#' * multiple = TRUE
#'
#' @param l A \code{data.frame} created using `avplots_data()`.
#' @param model An object of class \code{lm}.
#'
#' @noRd
#'
bp_case_one <- function(l, model) {

  nam <-
    l %>%
    names() %>%
    extract(-1)

  np        <- length(nam)
  n         <- nrow(l)
  var_resid <- residual_var(model, n)
  ind       <- ind_bp(model, var_resid)

  l %<>%
    bind_cols(ind)

  list(np = np, nam = nam, l = l)

}

#' @description
#' Computes breusch pagan statistic and p values when multiple = TRUE
#'
#' @noRd
#'
bp_case_loop <- function(np, nam, l) {

  tstat <- c()
  pvals <- c()

  for (i in seq_len(np)) {

    form <- as.formula(paste("ind ~", nam[i]))

    tstat[i] <-
      l %>%
      lm(form, data = .) %>%
      bp_fit()

    pvals[i] <- pchisq(tstat[i], df = 1, lower.tail = FALSE)

  }

  list(tstat = tstat, pvals = pvals)

}

#' @description
#' Computes breusch pagan statistic and p values when multiple = FALSE.
#'
#' @noRd
#'
bp_case_inter <- function(l, np, tstat) {

  comp <-
    l %>%
    select(-1) %>%
    lm(ind ~ ., data = .) %>%
    bp_fit()

  ps <- pchisq(comp, df = np, lower.tail = FALSE)

  bp <- c(tstat, comp)

  list(bp = bp, ps = ps)

}

#' @description Computes adjusted p values.
#'
#' @noRd
#'
bp_case_adj <- function(method, pvals, np, ps) {

  if (method == "bonferroni") {

    bpvals <- pmin(1, pvals * np)
    p      <- c(bpvals, ps)

  } else if (method == "sidak") {

    spvals <- pmin(1, 1 - (1 - pvals) ^ np)
    p      <- c(spvals, ps)

  } else if (method == "holm") {

    j <-
      pvals %>%
      length() %>%
      seq_len(.) %>%
      rev()

    h <-
      pvals %>%
      order() %>%
      order()

    pvals_sort <-
      pvals %>%
      sort() %>%
      multiply_by(j)

    pholms <-
      pmin(1, pvals_sort) %>%
      extract(h)

    p <- c(pholms, ps)

  } else {

    p <- c(pvals, ps)

  }

  return(p)

}

#' @description Computes breusch pagan statistic and p values when:
#' * fit = FALSE
#' * rhs = FALSE
#' * multiple = FALSE
#'
#' @noRd
#'
bp_case_5_inter <- function(l, model, vars, tstat) {

  n         <- nrow(l)
  var_resid <- residual_var(model, n)
  ind       <- ind_bp(model, var_resid)

  l %<>%
    select(-1) %>%
    select(!!! syms(vars)) %>%
    bind_cols(ind)

  np <-
    l %>%
    ncol() %>%
    subtract(1)

  ps <-
    l %>%
    bp_model() %>%
    pchisq(df = np, lower.tail = FALSE)

  bp <- c(tstat, bp_model(l))

  list(bp = bp, ps = ps, np = np)

}
