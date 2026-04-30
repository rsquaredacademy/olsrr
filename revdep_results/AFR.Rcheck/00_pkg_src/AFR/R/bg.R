#' @title Breusch-Godfrey test [BG test]
#' @description
#' BG test is used to test for autocorrelation in the errors of a regression model
#' @param model is a (generalized)linear regression model
#' @param order integer. maximal order of serial correlation to be tested.
#' @param order.by Either a vector z or a formula with a single explanatory variable like ~ z
#' @param type the type of test statistic to be returned
#' @param data an optional data frame containing the variables in the model
#' @param fill starting values for the lagged residuals in the auxiliary regression. By default 0 but can also be set to NA.
#' @references Mitchel, D. and Zeileis, A. Published 2021-11-07. lmtest package
#' @import stats
#' @importFrom cli console_width
#' @examples
#' model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
#' bg(model)
#' @export

bg<-
  function (model, order = 1, order.by = NULL, type = c("Chisq",
                                                          "F"), data = list(), fill = 0)
  {
    dname <- paste(deparse(substitute(model)))
    if (!inherits(model, "formula")) {
      X <- if (is.matrix(model$x))
        model$x
      else model.matrix(terms(model), model.frame(model))
      y <- if (is.vector(model$y))
        model$y
      else model.response(model.frame(model))
    }
    else {
      mf <- model.frame(model, data = data)
      y <- model.response(mf)
      X <- model.matrix(model, data = data)
    }
    if (!is.null(order.by)) {
      if (inherits(order.by, "formula")) {
        z <- model.matrix(order.by, data = data)
        z <- as.vector(z[, ncol(z)])
      }
      else {
        z <- order.by
      }
      X <- as.matrix(X[order(z), ])
      y <- y[order(z)]
    }
    n <- nrow(X)
    k <- ncol(X)
    order <- 1:order
    m <- length(order)
    resi <- lm.fit(X, y)$residuals
    Z <- sapply(order, function(x) c(rep(fill, length.out = x),
                                     resi[1:(n - x)]))
    if (any(na <- !complete.cases(Z))) {
      X <- X[!na, , drop = FALSE]
      Z <- Z[!na, , drop = FALSE]
      y <- y[!na]
      resi <- resi[!na]
      n <- nrow(X)
    }
    auxfit <- lm.fit(cbind(X, Z), resi)
    cf <- auxfit$coefficients
    vc <- chol2inv(auxfit$qr$qr) * sum(auxfit$residuals^2)/auxfit$df.residual
    names(cf) <- colnames(vc) <- rownames(vc) <- c(colnames(X),
                                                   paste("lag(resid)", order, sep = "_"))
    switch(match.arg(type), Chisq = {
      bg <- n * sum(auxfit$fitted.values^2)/sum(resi^2)
      p.val <- pchisq(bg, m, lower.tail = FALSE)
      df <- m
      names(df) <- "df"
    }, F = {
      uresi <- auxfit$residuals
      bg <- ((sum(resi^2) - sum(uresi^2))/m)/(sum(uresi^2)/(n -
                                                              k - m))
      df <- c(m, n - k - m)
      names(df) <- c("df1", "df2")
      p.val <- pf(bg, df1 = df[1], df2 = df[2], lower.tail = FALSE)
    })

    #print
    a <- c("LM test", "p-value")
    b <- c(round(bg, 3), round(p.val,3))
    w1 <- max(nchar(a))
    w2 <- max(nchar(b))
    w3 <- console_width()
    w <- sum(w1, w2, 7)
    n <- length(b)

    alternative <-NULL
    cat(format(as.character(paste("Breusch-Godfrey test for serial correlation of order up to", max(order))), width=w3, justify="centre"), "\n\n")

    if (p.val>=0.05)
      cat(paste("Residuals are not autocorrelated.", "\n"))
    else
      cat(paste("Residuals are autocorrelated.", "\n"))

    cat(rep("-", w), sep = "", "\n")
    for (i in seq(n)) {
      cat(fl(a[i], w1),fsp(),fsp(), fg(b[i], w2), "\n")
    }
    cat(rep("-", w), sep = "", "\n")

  }

