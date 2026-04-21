#' @title Breusch-Pagan test
#' @description
#' Breusch-Pagan test is used to test against heteroskedasticity of a time-series
#' @param model is a (generalized)linear regression model
#' @param varformula a formula describing only the potential explanatory variables for the variance (no dependent variable needed). By default the same explanatory variables are taken as in the main regression model.
#' @param studentize logical. If set to TRUE Koenker's studentized version of the test statistic will be used.
#' @param data an optional data frame containing the variables in the model
#' @import stats
#' @importFrom cli console_width
#' @examples
#' model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
#' bp(model)
#' @references Torsten, H., Zeileis, A., Farebrother, Richard W., Cummins, C., Millo, G., Mitchell, D., lmtest package
#' Wang, B., 2014, bstats package
#' @export

bp<-function (model, varformula = NULL, studentize = TRUE, data = list())
  {
    dname <- paste(deparse(substitute(model)))
    if (!inherits(model, "model")) {
      X <- if (is.matrix(model$x))
        model$x
      else model.matrix(terms(model), model.frame(model))
      y <- if (is.vector(model$y))
        model$y
      else model.response(model.frame(model))
      Z <- if (is.null(varformula))
        X
      else model.matrix(varformula, data = data)
    }
    else {
      mf <- model.frame(model, data = data)
      y <- model.response(mf)
      X <- model.matrix(model, data = data)
      Z <- if (is.null(varformula))
        X
      else model.matrix(varformula, data = data)
    }
    if (!(all(c(row.names(X) %in% row.names(Z), row.names(Z) %in%
                row.names(X))))) {
      allnames <- row.names(X)[row.names(X) %in% row.names(Z)]
      X <- X[allnames, ]
      Z <- Z[allnames, ]
      y <- y[allnames]
    }
    if (ncol(Z) < 2)
      stop("the auxiliary variance regression requires at least an intercept and a regressor")
    k <- ncol(X)
    n <- nrow(X)
    resi <- lm.fit(X, y)$residuals
    sigma2 <- sum(resi^2)/n
    if (studentize) {
      w <- resi^2 - sigma2
      aux <- lm.fit(Z, w)
      bp <- n * sum(aux$fitted.values^2)/sum(w^2)
      method <- "studentized Breusch-Pagan test"
    }
    else {
      f <- resi^2/sigma2 - 1
      aux <- lm.fit(Z, f)
      bp <- 0.5 * sum(aux$fitted.values^2)
      method <- "Breusch-Pagan test"
    }
    #message1<-cat("Homoskedasticity presents.", "\n", "Please use other tests additionally.In case of opposite results study the case further.")
    #message2<-cat("Heteroskedasticity presents.", "\n", "Please use others tests additionally.In case of opposite results study the case further.")
    df <- c(df = aux$rank - 1)
    PVAL<-pchisq(bp, df, lower.tail = FALSE)

    #print
    a <- c("BP", "p-value")
    b <- c(round(bp, 3), round(PVAL,3))
    w1 <- max(nchar(a))
    w2 <- max(nchar(b))
    w3 <- console_width()
    w <- sum(w1, w2, 7)
    n <- length(b)


    cat(format(as.character("Breusch-Pagan test"), width=w3, justify="centre"), "\n")
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

