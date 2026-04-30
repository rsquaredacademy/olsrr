#' Main function for fitting the fixed effect linear model
#'
#' Fit a fixed effect linear model via profile likelihood.
#'
#' @param formula a two-sided formula object describing the model to be fitted,
#' with the response variable on the left of a ~ operator and covariates on the right,
#' separated by + operators. The fixed effect of the provider identifier is specified using \code{id()}.
#' @param data a data frame containing the variables named in the `formula`,
#' or the columns specified by `Y.char`, `Z.char`, and `ProvID.char`.
#' @param Y.char a character string specifying the column name of the response variable in the `data`.
#' @param Z.char a character vector specifying the column names of the covariates in the `data`.
#' @param ProvID.char a character string specifying the column name of the provider identifier in the `data`.
#' @param Y a numeric vector representing the response variable.
#' @param Z a matrix or data frame representing the covariates, which can include both numeric and categorical variables.
#' @param ProvID a numeric vector representing the provider identifier.
#' @param option.gamma.var a character string specifying the method to calculate the variance of provider effects \code{gamma},
#' must be \code{"full"} or \code{"simplified"}. You can specify just the initial letter.
#' \itemize{
#'    \item{\code{"simplified"}} (default) calculating the simplified variance of provider effects assuming regression coefficients are known.
#'    This approach is suitable for large datasets where the results of the full and simplified methods are similar,
#'    or when the full method may become unstable due to complex settings.
#'    \item{\code{"full"}} considering the correlation between provider effects and regression coefficients.
#' }
#'
#' @return A list of objects with S3 class \code{"linear_fe"}:
#' \item{coefficient}{a list containing the estimated coefficients:
#'   \code{beta}, the fixed effects for each predictor, and \code{gamma}, the effect for each provider.}
#' \item{variance}{a list containing the variance estimates:
#'   \code{beta}, the variance-covariance matrix of the predictor coefficients, and \code{gamma}, the variance of the provider effects.}
#' \item{sigma}{the residual standard error.}
#' \item{fitted}{the fitted values of each individual.}
#' \item{observation}{the original response of each individual.}
#' \item{residuals}{the residuals of each individual, that is response minus fitted values.}
#' \item{linear_pred}{the linear predictor of each individual.}
#' \item{data_include}{the data used to fit the model, sorted by the provider identifier.
#' For categorical covariates, this includes the dummy variables created for
#' all categories except the reference level.}
#' \item{char_list}{a list of the character vectors representing the column names for
#' the response variable, covariates, and provider identifier.
#' For categorical variables, the names reflect the dummy variables created for each category.}
#' \item{Loglkd}{log likelihood.}
#' \item{AIC}{Akaike information criterion.}
#' \item{BIC}{Bayesian information criterion.}
#'
#' @details
#' This function is used to fit a fixed effect linear model of the form:
#' \deqn{Y_{ij} = \gamma_i + \mathbf{Z}_{ij}^\top\boldsymbol\beta + \epsilon_{ij}}
#' where \eqn{Y_{ij}} is the continuous outcome for individual \eqn{j} in provider \eqn{i}, \eqn{\gamma_i} is the provider-specific effect,
#' \eqn{\mathbf{Z}_{ij}} are the covariates, and \eqn{\boldsymbol\beta} is the vector of coefficients for the covariates.
#'
#' The function accepts three different input formats:
#' a formula and dataset, where the formula is of the form \code{response ~ covariates + id(provider)}, with \code{provider} representing the provider identifier;
#' a dataset along with the column names of the response, covariates, and provider identifier;
#' or the outcome vector \eqn{\boldsymbol{Y}}, the covariate matrix or data frame \eqn{\mathbf{Z}}, and the provider identifier vector.
#'
#' If issues arise during model fitting, consider using the \code{data_check} function to perform a data quality check,
#' which can help identify missing values, low variation in covariates, high-pairwise correlation, and multicollinearity.
#' For datasets with missing values, this function automatically removes observations (rows) with any missing values before fitting the model.
#'
#' @seealso \code{\link{data_check}}
#'
#' @importFrom Matrix bdiag
#' @importFrom stats complete.cases terms model.matrix reformulate as.formula
#'
#' @export
#'
#' @examples
#' data(ExampleDataLinear)
#' outcome <- ExampleDataLinear$Y
#' covar <- ExampleDataLinear$Z
#' ProvID <- ExampleDataLinear$ProvID
#' data <- data.frame(outcome, ProvID, covar)
#' covar.char <- colnames(covar)
#' outcome.char <- colnames(data)[1]
#' ProvID.char <- colnames(data)[2]
#' formula <- as.formula(paste("outcome ~", paste(covar.char, collapse = " + "), "+ id(ProvID)"))
#'
#' # Fit fixed linear effect model using three input formats
#' fit_fe1 <- linear_fe(Y = outcome, Z = covar, ProvID = ProvID)
#' fit_fe2 <- linear_fe(data = data, Y.char = outcome.char,
#' Z.char = covar.char, ProvID.char = ProvID.char)
#' fit_fe3 <- linear_fe(formula, data)
#'
#' @references
#' Hsiao, C. (2022). Analysis of panel data (No. 64). Cambridge university press.
#' \cr
#'

linear_fe <- function(formula = NULL, data = NULL,
                      Y = NULL, Z = NULL, ProvID = NULL,
                      Y.char = NULL, Z.char = NULL, ProvID.char = NULL,
                      option.gamma.var = "simplified"){
  if (!is.null(formula) && !is.null(data)) {
    message("Input format: formula and data.")

    formula_terms <- terms(formula)
    Y.char <- as.character(attr(formula_terms, "variables"))[2]
    predictors <- attr(formula_terms, "term.labels")

    ProvID.char <- gsub(".*id\\(([^)]+)\\).*", "\\1", predictors[grepl("id\\(", predictors)])
    Z.char <- predictors[!grepl("id\\(", predictors)]

    if (!all(c(Y.char, Z.char, ProvID.char) %in% colnames(data)))
      stop("Formula contains variables not in the data or is incorrectly structured.", call.=F)

    data <- data[,c(Y.char, ProvID.char, Z.char)]
    data <- data[complete.cases(data), ] # Remove rows with missing values

    #mf <- model.frame(formula, data)
    #Y <- model.response(mf)
    Y <- data[,Y.char, drop = F]
    Z <- model.matrix(reformulate(Z.char), data)[, -1, drop = F]
    # Z <- model.matrix(~ data[[predictors]] - 1)
    ProvID <- data[,ProvID.char, drop = F]
  }
  else if (!is.null(data) && !is.null(Y.char) && !is.null(Z.char) && !is.null(ProvID.char)) {
    message("Input format: data, Y.char, Z.char, and ProvID.char.")

    if (!all(c(Y.char, Z.char, ProvID.char) %in% colnames(data)))
      stop("Some of the specified columns are not in the data!", call.=FALSE)

    data <- data[,c(Y.char, ProvID.char, Z.char)]
    data <- data[complete.cases(data), ] # Remove rows with missing values

    Y <- data[, Y.char]
    Z <- model.matrix(reformulate(Z.char), data)[, -1, drop = FALSE]
    ProvID <- data[, ProvID.char, drop = F]
  }
  else if (!is.null(Y) && !is.null(Z) && !is.null(ProvID)) {
    message("Input format: Y, Z, and ProvID.")

    if (length(Y) != length(ProvID) | (length(ProvID) != nrow(Z))) {
      stop("Dimensions of the input data do not match!!", call.=F)
    }

    data <- data.frame(Y, ProvID, Z)
    data <- data[complete.cases(data), ] # Remove rows with missing values
    Y.char <- colnames(data)[1]
    ProvID.char <- colnames(data)[2]
    Z.char <- colnames(Z)
    Y <- data[, Y.char]
    ProvID <- data[, ProvID.char]
    Z <- model.matrix(reformulate(Z.char), data)[, -1, drop = FALSE]
  }
  else {
    stop("Insufficient or incompatible arguments provided. Please provide either (1) formula and data, (2) data, Y.char, Z.char, and ProvID.char, or (3) Y, Z, and ProvID.", call.=FALSE)
  }

  data <- data.frame(Y, ProvID, Z)
  data <- data[complete.cases(data), ] # Remove rows with missing values
  Y.char <- colnames(data)[1]
  ProvID.char <- colnames(data)[2]
  Z.char <- colnames(Z)
  data <- data[order(factor(data[,ProvID.char])),]

  n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)
  m <- length(n.prov) # number of providers
  n <- sum(n.prov) # number of observations
  p <- length(Z.char) # number of covariates

  Z <- as.matrix(data[,Z.char], drop = F)
  Y <- as.matrix(data[, Y.char, drop = F])
  ProvID <- as.matrix(data[, ProvID.char, drop = F])

  Q <- lapply(n.prov, function(n) diag(n)-matrix(1, nrow = n, ncol = n)/n)

  # Coefficients
  beta <- matrix(solve(t(Z)%*%bdiag(Q)%*%Z)%*%t(Z)%*%bdiag(Q)%*%Y, ncol = 1)
  colnames(beta) <- "beta"
  rownames(beta) <- Z.char

  y_bar <- sapply(split(data[,Y.char], data[,ProvID.char]),mean)
  Z_bar <- t(matrix(sapply(split(data[,Z.char, drop = FALSE], data[,ProvID.char]), colMeans),
                    ncol=length(y_bar), nrow = length(beta)))
  gamma.prov <- as.matrix(y_bar - Z_bar %*% beta)
  colnames(gamma.prov) <- "gamma"
  rownames(gamma.prov)<- names(n.prov)

  coefficient <- list()
  coefficient$beta <- beta
  coefficient$gamma <- gamma.prov

  # Prediction
  linear_pred <- Z %*% beta
  colnames(linear_pred) <- "Linear Predictor"
  rownames(linear_pred) <- seq_len(nrow(linear_pred))
  gamma.obs <- rep(gamma.prov, n.prov)
  pred <- gamma.obs + linear_pred
  colnames(pred) <- "Prediction"
  rownames(pred) <- seq_len(nrow(pred))
  residuals <- matrix(Y - pred, ncol = 1)
  colnames(residuals) <- "Residuals"
  rownames(residuals) <- seq_len(nrow(residuals))
  SSR <- sum(residuals^2)
  sigma_hat_sq <- sum(residuals^2)/(n - m - p)

  # Variance
  varcov_beta <- matrix(sigma_hat_sq * solve(t(Z)%*%bdiag(Q)%*%Z), ncol = p, nrow = p)
  rownames(varcov_beta) <- Z.char
  colnames(varcov_beta) <- Z.char

  if (option.gamma.var == "full" | option.gamma.var == "f") {
    var_gamma <- matrix(sigma_hat_sq*(1/n.prov + diag(Z_bar%*%solve(t(Z)%*%bdiag(Q)%*%Z)%*%t(Z_bar))), ncol = 1)
    attr(var_gamma, "description")<- "full"
  }
  else if (option.gamma.var == "simplified" | option.gamma.var == "s") {
    var_gamma <- matrix(sigma_hat_sq/n.prov, ncol = 1)
    attr(var_gamma, "description")<- "simplified"
  }
  else stop("Argument 'option.gamma.var' should be 'full' or 'simplified'.")
  rownames(var_gamma) <- names(n.prov)
  colnames(var_gamma) <- "Variance.Gamma"

  variance <- list()
  variance$beta <- varcov_beta
  variance$gamma <- var_gamma

  # AIC and BIC
  log_likelihood <- - (n/2) * log(2*pi) - (n/2) * log(sum(residuals^2)/n) - (SSR/(2*sum(residuals^2)/n))
  AIC <- -2*log_likelihood + 2*(m+p+1)
  BIC <- -2*log_likelihood + (m+p+1) * log(n)

  char_list <- list(Y.char = Y.char,
                    ProvID.char = ProvID.char,
                    Z.char = Z.char)

  result <- structure(list(coefficient = coefficient,
                           variance = variance,
                           sigma = sqrt(sigma_hat_sq),
                           fitted = pred,
                           observation = Y,
                           residuals = residuals,
                           linear_pred = linear_pred,
                           Loglkd = log_likelihood,
                           AIC = AIC,
                           BIC = BIC
                           ),
                      class = "linear_fe")  #define a list for prediction

  result$data_include <- data
  result$char_list <- char_list

  return(result)
}














