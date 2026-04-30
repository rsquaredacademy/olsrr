#' @title Hodrick-Prescott filter for time series data
#' @description
#' Hodrick-Prescott filter is a data smoothing technique that removes trending in time series data frame
#' @param x time-series vector
#' @param type character, indicating the filter type
#' @param freq integer
#' @param drift logical
#' @import stats
#' @examples
#' data(macroKZ)
#' HP(macroKZ[,2])
#' @rdname HP
#' @export

HP<-
  function (x, freq = NULL, type = c("lambda", "frequency"), drift = FALSE)
  {
    if (is.null(drift))
      drift <- FALSE
    xname = deparse(substitute(x))
    type = match.arg(type)
    if (is.null(type))
      type <- "lambda"
    if (is.ts(x)) {
      tsp.x <- tsp(x)
      frq.x <- frequency(x)
      if (type == "lambda") {
        if (is.null(freq)) {
          if (frq.x == 1)
            lambda = 6
          if (frq.x == 4)
            lambda = 1600
          if (frq.x == 12)
            lambda = 129600
        }
        else lambda = freq
      }
    }
    else {
      if (type == "lambda") {
        if (is.null(freq))
          stop("freq is NULL")
        else lambda = freq
      }
    }
    if (type == "frequency") {
      if (is.null(freq))
        stop("freq is NULL")
      else lambda = (2 * sin(pi/freq))^-4
    }

    undrift <- function(x) {
      n <- nrow(x)
      X <- cbind(rep(1, n), 1:n)
      b <- solve(t(X) %*% X) %*% t(X) %*% x
      x - X %*% b
    }

    xo = x
    x = as.matrix(x)
    if (drift)
      x = undrift(x)
    n = length(x)
    imat = diag(n)
    Ln = rbind(matrix(0, 1, n), diag(1, n - 1, n))
    Ln = (imat - Ln) %*% (imat - Ln)
    Q = t(Ln[3:n, ])
    SIGMA.R = t(Q) %*% Q
    SIGMA.n = diag(n - 2)
    g = t(Q) %*% as.matrix(x)
    b = solve(SIGMA.n + lambda * SIGMA.R, g)
    x.cycle = c(lambda * Q %*% b)
    x.trend = x - x.cycle
    if (is.ts(xo)) {
      tsp.x = tsp(xo)
      x.cycle = ts(x.cycle, start = tsp.x[1], frequency = tsp.x[3])
      x.trend = ts(x.trend, start = tsp.x[1], frequency = tsp.x[3])
      x = ts(x, start = tsp.x[1], frequency = tsp.x[3])
    }
    A = lambda * Q %*% solve(SIGMA.n + lambda * SIGMA.R) %*%
      t(Q)
    res <- list(cycle = x.cycle, trend = x.trend, fmatrix = A,
                title = "Hodrick-Prescott Filter", xname = xname, call = as.call(match.call()),
                type = type, lambda = lambda, method = "hpfilter", x = x)
    return(structure(res, class = "mFilter"))
  }
