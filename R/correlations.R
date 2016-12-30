#' @importFrom stats cor
#' @title Part and Partial Correlations
#' @description Zero-order, part and partial correlations
#' @param model an object of class \code{lm}
#' @return \code{correlations} returns an object of class \code{"correlations"}.
#' An object of class \code{"correlations"} is a data frame containing the
#' following components:
#'
#' \item{Zero-order}{zero order correlations}
#' \item{Partial}{partial correlations}
#' \item{Part}{part correlations}
#' @export
#'
correlations <- function(model) UseMethod('correlations')

#' @export
#'
correlations.default <- function(model) {

    if (!all(class(model) == 'lm')) {
        stop('Please specify a OLS linear regression model.', call. = FALSE)
    }

    mdata     <- model.frame(model)
    mdata2    <- as.data.frame(lapply(mdata, as.numeric))
    cor_mdata <- cor(mdata2)[-1, 1]
    r1        <- summary(model)$r.squared
    n         <- ncol(mdata2)
    r2        <- c()

    for (i in 2:n) {
        dat   <- mdata2[, c(-1, -i)]
        model <- lm(mdata2[[1]] ~ ., data = dat)
        k     <- summary(model)
        out   <- k$r.squared
        r2    <- c(r2, out)
    }

    ksign <- as.vector(sign(cor_mdata))
    n2    <- n - 1
    parts <- c()

    for (i in seq_len(n2)) {
        part  <- ksign[i] * sqrt(r1 - r2[i])
        parts <- c(parts, part)
    }

    partials <- c()

    for (i in seq_len(n2)) {
        partial  <- parts[i] / sqrt(1 - r2[i])
        partials <- c(partials, partial)
    }

    result        <- data.frame(cor_mdata, partials, parts)
    names(result) <- c("Zero-order", "Partial", "Part")
    out           <- t(apply(result, 1, round, 3))
    class(out)    <- 'correlations'

    return(out)
}

#' @export
#'
print.correlations <- function(x, ...) {
    print_correlations(x)
}
