#' @title Godfrey-Quandt test
#' @description
#'  Godfrey-Quandt test is used to test against heteroskedasticity of a time-series
#' @param model is a (generalized)linear regression model
#' @param point numerical. If point is smaller than 1 it is interpreted as percentages of data
#' @param fraction numerical. The number of central observations to be omitted.
#' @param alternative a character string specifying the alternative hypothesis.
#' @param order.by Either a vector z or a formula with a single explanatory variable like ~ z
#' @param data an optional data frame containing the variables in the model.
#' @importFrom lmtest gqtest
#' @importFrom cli console_width
#' @examples
#' model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
#' gq(model)
#' @references Torsten, H., Zeileis, A., Farebrother, Richard W., Cummins, C., Millo, G., Mitchell, D., lmtest package
#' Wang, B., 2014, bstats package
#' @export

gq<-
  function (model, point = 0.5, fraction = 0, alternative = c("greater",
                                                              "two.sided", "less"), order.by = NULL, data = list())
  {
    dname <- paste(deparse(substitute(model)))
    alternative <- match.arg(alternative)
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
    k <- ncol(X)
    n <- nrow(X)
    if (point > 1) {
      if (fraction < 1)
        fraction <- floor(fraction * n)
      point1 <- point - ceiling(fraction/2)
      point2 <- point + ceiling(fraction/2 + 0.01)
    }
    else {
      if (fraction >= 1)
        fraction <- fraction/n
      point1 <- floor((point - fraction/2) * n)
      point2 <- ceiling((point + fraction/2) * n + 0.01)
    }
    if (point2 > n - k + 1 | point1 < k)
      stop("inadmissable breakpoint/too many central observations omitted")
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
    rss1 <- sum(lm.fit(as.matrix(X[1:point1, ]), y[1:point1])$residuals^2)
    rss2 <- sum(lm.fit(as.matrix(X[point2:n, ]), y[point2:n])$residuals^2)
    mss <- c(rss1/(point1 - k), rss2/(n - point2 + 1 - k))
    gq <- mss[2]/mss[1]
    df <- c(n - point2 + 1 - k, point1 - k)
    PVAL <- switch(alternative,two.sided = (2 * min(pf(gq, df[1],
                                                       df[2]), pf(gq, df[1], df[2], lower.tail = FALSE))), less = pf(gq,
                                                                                                                     df[1], df[2]), greater = pf(gq, df[1], df[2], lower.tail = FALSE))


    #print
    a <- c("GQ", "p-value", "df1", "df2")
    b <- c(round(gq, 3), round(PVAL,3), df[1], df[2])
    w1 <- max(nchar(a))
    w2 <- max(nchar(b))
    w3 <- console_width()
    w <- sum(w1, w2, 7)
    n <- length(b)


    cat(format(as.character("Goldfeld-Quandt test"), width=w3, justify="centre"), "\n")
    if (PVAL>=0.05)
      cat(paste("Homoskedasticity presents.", "Please use other tests additionally.", "In case of opposite results study the case further.", sep="\n", "\n"))
    else
      cat(paste("Heteroskedasticity presents.", "Please use other tests additionally.", "In case of opposite results study the case further.", sep="\n", "\n"))

    cat(rep("-", w), sep = "", "\n")
    for (i in seq(n)) {
      cat(fl(a[i], w1),fsp(),fsp(), fg(b[i], w2), "\n")
    }
    cat(rep("-", w), sep = "", "\n")

}

