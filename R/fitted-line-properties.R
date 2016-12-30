# # 1. sum of residuals is zero
# round(sum(model$residual)) == 0
#
# # 2. sum of observed values == sum of fitted values
# sum(model.response(model.frame(model))) == sum(fitted.values(model))
#
# # 3. mean(observed values) == mean(fitted values)
# mean(model.response(model.frame(model))) == mean(fitted.values(model))
#
# # 4. sum of weighted residuals is 0; consider the values of the predictors to
# # be the weight
# round(sum(model.frame(model)[2] * model$residuals)) == 0
#
# # 5. sum of weighted residuals is zero (where weights are fitted values)
# round(sum(fitted.values(model) * residuals(model))) == 0

#' @title Regression Line
#' @description regression line always passes through xbar and ybar
#' @param response response variable
#' @param predictor explanatory variable
#' @return \code{reg_line} returns  a list containing the
#' following components:
#'
#' \item{mean_pred}{mean of \code{predictor}}
#' \item{mean_resp}{mean of \code{response}}
#' \item{model}{an object of class \code{lm}}
#' @export
#'
reg_line <- function(response, predictor) {

    resp        <- l(deparse(substitute(response)))
    preds       <- l(deparse(substitute(predictor)))
    m_predictor <- round(mean(predictor), 2)
    m_response  <- round(mean(response), 2)

    plot(predictor, response, type = 'n', xlab = preds, ylab = resp,
        main = "Regression Line",
        sub = paste0("Mean(", resp, "): ", m_predictor, "          Mean(", preds, "): ", m_response))
    points(predictor, response)

    model <- lm(response ~ predictor)
    abline(model)
    points(m_predictor, m_response, col = 'red', pch = 5, cex = 1.5)

    z <- list(mean_pred = m_predictor,
              mean_resp = m_response,
              model     = model)
}

#' @importFrom stats predict qf
#' @title Confidence Band Regression Line
#' @description confidence band for regression line
#' @param model an object of class \code{lm}
#' @param conf.level confidence level
#' @return \code{conf_band} returns  a data frame
#'
conf_band <- function(model, conf.level = 0.95) {

    if (!inherits(model, 'lm')) {
      stop('Please specify a linear regression model.', call. = FALSE)
    }

    if (!is.numeric(conf.level)) {
      stop('conf.level must be numeric.', call. = FALSE)
    }

    if ((conf.level < 0) | (conf.level > 1)) {
      stop('conf.level must be between 0 and 1.', call. = FALSE)
    }

	  m    <- model.frame(model)[2]
    yhat <- predict(model, newdata = m, se.fit = T)
    n    <- nrow(m)
    w    <- sqrt(2 * qf(conf.level, 2, n - 2))
    b_l  <- list()
    b_u  <- list()

    for (i in seq_len(n)) {
        b_l[[i]] <- yhat$fit[[i]] - w * yhat$se.fit[[i]]
        b_u[[i]] <- yhat$fit[[i]] + w * yhat$se.fit[[i]]
    }

    dname  <- names(m)
    lower  <- unlist(b_l)
    upper  <- unlist(b_u)
    result <- data.frame(lower, upper)
    out    <- cbind(m, result)

    return(out)
}

#' @importFrom graphics lines
#' @title Confidence Interval Regression Line
#' @description Confidence interval for a simple linear regression line
#' @param model a simple linear regression model
#' @param conf.level confidence level for the interval
#' @return \code{reg_line} returns  a list containing the
#' following components:
#'
#' \item{conf_band}{confidence band for the regression line}
#' @export
#'
conf_line <- function(model, conf.level = 0.95) {

    if (!inherits(model, 'lm')) {
      stop('Please specify a linear regression model.', call. = FALSE)
    }

    if (!is.numeric(conf.level)) {
      stop('conf.level must be numeric.', call. = FALSE)
    }

    if ((conf.level < 0) | (conf.level > 1)) {
      stop('conf.level must be between 0 and 1.', call. = FALSE)
    }

    ho  <- conf_band(model, conf.level = 0.9)
    ho  <- ho[order(ho[[1]]), ]
    nam <- names(model.frame(model))

    plot(ho[[1]], ho$upper, ylim = c(min(ho$lower), max(ho$upper)), type = "n",
    	xlab = nam[2], ylab = nam[1], main = "Confidence Band: Regression Line")
    lines(ho[[1]], ho$lower, type = "l", lty = 2, col = "red")
    lines(ho[[1]], ho$upper, type = "l", lty = 2, col = "red")
    abline(model)

    z <- list(conf_band = ho)
}
