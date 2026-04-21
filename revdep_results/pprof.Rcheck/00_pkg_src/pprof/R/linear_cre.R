#' Main Function for fitting correlated random effect linear model
#'
#' Fit a correlated  random effect linear model via \code{\link[lme4]{lmer}} from the \code{lme4} package.
#'
#' @param data a data frame containing all variables.
#' @param Y.char a character string specifying the column name of the response variable in the `data`.
#' @param wb.char a character vector specifying covariates to be decomposed into
#'   within (\code{*_within}) and between (\code{*_bar}) components.
#' @param other.char a character vector specifying additional covariates to include in the model without decomposition.
#' @param ProvID.char a character string specifying the column name of the provider identifier in the `data`.
#' @param \dots additional arguments passed to \code{\link[lme4]{lmer}} for further customization.
#'
#' @return A list of objects with S3 class \code{"linear_cre"}:
#' \item{coefficient}{a list containing the estimated coefficients:
#'   \code{FE}, the fixed effects for each predictor and the intercept, and \code{RE}, the random effects for each provider.}
#' \item{variance}{a list containing the variance estimates:
#'   \code{FE}, the variance-covariance matrix of the fixed effect coefficients, and \code{RE}, the variance of the random effects.}
#' \item{sigma}{the residual standard error.}
#' \item{fitted}{the fitted values of each individual.}
#' \item{observation}{the original response of each individual.}
#' \item{residuals}{the residuals of each individual, that is response minus fitted values.}
#' \item{linear_pred}{the linear predictor of each individual.}
#' \item{data_include}{the processed data used to fit the model, sorted by the provider identifier.
#' This includes the within-group (\code{*_within}) and between-group (\code{*_bar}) components for variables specified in \code{wb.char}.
#' For categorical covariates, it includes the dummy variables created for
#' all categories except the reference level.}
#' \item{char_list}{a list of the character vectors representing the column names for
#' the response variable, covariates, and provider identifier.
#' For categorical variables, the names reflect the dummy variables created for each category.}
#' \item{Loglkd}{the log-likelihood.}
#' \item{AIC}{Akaike information criterion.}
#' \item{BIC}{Bayesian information criterion.}
#'
#' @details
#' Fit a correlated random effect linear model using \code{\link[lme4]{lmer}} with a Mundlak
#' within–between decomposition for selected covariates. For each
#' decomposed covariate \eqn{Z_k}, the function constructs
#' \eqn{Z_{k,\mathrm{bar},i} = \frac{1}{n_i}\sum_j Z_{k,ij}} (the group mean, "between")
#' and \eqn{Z_{k,\mathrm{within},ij} = Z_{k,ij} - Z_{k,\mathrm{bar},i}} (the within-group deviation),
#' and estimates
#' \deqn{Y_{ij} = \mu + \alpha_i + \sum_k \beta_{k,W} Z_{k,\mathrm{within},ij}
#'       + \sum_k \beta_{k,B} Z_{k,\mathrm{bar},i}
#'       + \mathbf{X}_{ij}^\top\gamma + \varepsilon_{ij},}
#' where \eqn{\alpha_i \sim \mathcal{N}(0,\sigma_\alpha^2)} is a random intercept.
#'
#' The function creates, for every name in \code{wb.char}, two columns:
#' \code{<var>_bar} (group mean within \code{ProvID.char}) and
#' \code{<var>_within} (observation minus its group mean).
#' The fitted model is:
#' \preformatted{
#'   Y ~ <all *_within> + <all *_bar> + <other.char> + (1 | ProvID)
#' }
#'
#' In addition to these input formats, all arguments from the \code{\link[lme4]{lmer}} function can be modified via \code{\dots},
#' allowing for customization of model fitting options such as controlling the optimization method or adjusting convergence criteria.
#' By default, the model is fitted using REML (restricted maximum likelihood).
#'
#' If issues arise during model fitting, consider using the \code{data_check} function to perform a data quality check,
#' which can help identify missing values, low variation in covariates, high-pairwise correlation, and multicollinearity.
#' For datasets with missing values, this function automatically removes observations (rows) with any missing values before fitting the model.
#'
#' @seealso \code{\link{data_check}}
#'
#' @importFrom lme4 lmer fixef ranef
#' @importFrom stats complete.cases as.formula model.matrix fitted residuals logLik
#' @importFrom dplyr group_by mutate ungroup across
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @examples
#' data(ExampleDataLinear)
#' outcome <- ExampleDataLinear$Y
#' covar <- ExampleDataLinear$Z
#' ProvID <- ExampleDataLinear$ProvID
#' data <- data.frame(outcome, ProvID, covar)
#' outcome.char <- colnames(data)[1]
#' ProvID.char <- colnames(data)[2]
#' wb.char <- c("z1", "z2")
#' other.char <- c("z3", "z4", "z5")
#'
#' # Fit a correlated random effect linear model
#' fit_cre <- linear_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
#' wb.char = wb.char, other.char = other.char)
#'
#' @references
#' Bates D, Maechler M, Bolker B, Walker S (2015). \emph{Fitting Linear Mixed-Effects Models Using lme4}.
#' Journal of Statistical Software, 67(1), 1-48.
#' \cr


linear_cre <- function(data, Y.char, wb.char, other.char = NULL, ProvID.char, ...) {
  needed <- c(Y.char, ProvID.char, wb.char, if (!is.null(other.char)) other.char)
  if (!all(needed %in% colnames(data))) {
    stop("Some specified columns are not in `data`.", call. = FALSE)
  }

  df_cre <- data %>%
    group_by(.data[[ProvID.char]]) %>%
    mutate(
      across(all_of(wb.char),
             ~ mean(.x, na.rm = TRUE),
             .names = "{.col}_bar"),
      across(all_of(wb.char),
             ~ .x - mean(.x, na.rm = TRUE),
             .names = "{.col}_within")
    ) %>%
    ungroup()

  within_terms  <- paste0(wb.char, "_within")
  between_terms <- paste0(wb.char, "_bar")
  rhs_terms     <- c(within_terms, between_terms, other.char)

  df_use <- df_cre[, c(Y.char, ProvID.char, rhs_terms), drop = FALSE]
  df_use <- df_use[complete.cases(df_use), ] # Remove rows with missing values
  df_use <- df_use[order(factor(df_use[[ProvID.char]])), ]

  f <- as.formula(paste(Y.char, "~", paste(rhs_terms, collapse = " + "), "+ (1 |", ProvID.char, ")"))
  model <- lmer(f, data = df_use, ...)

  X.model <- model.matrix(model)
  Y <- as.matrix(df_use[, Y.char, drop = F])
  ProvID <- as.matrix(df_use[, ProvID.char, drop = F])
  data <- as.data.frame(cbind(Y, ProvID, X.model))

  n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)
  m <- length(n.prov) # number of providers
  n <- sum(n.prov) # number of observations

  # Coefficients
  FE_coefficient <- matrix(fixef(model))
  colnames(FE_coefficient) <- "Coefficient"
  rownames(FE_coefficient) <- names(fixef(model))

  RE_coefficient <- as.matrix(ranef(model)[[ProvID.char]], ncol = 1)
  colnames(RE_coefficient) <- "alpha"
  rownames(RE_coefficient) <- names(n.prov)

  coefficient <- list()
  coefficient$FE <- FE_coefficient
  coefficient$RE <- RE_coefficient

  # Variance
  sum <- summary(model)
  var_alpha <- matrix(as.data.frame(sum$varcor)[1,"sdcor"]^2)
  colnames(var_alpha) <- "Variance.Alpha"
  rownames(var_alpha) <- "ProvID"

  varcov_FE <- matrix(sum$vcov, ncol = length(FE_coefficient))
  colnames(varcov_FE) <- colnames(sum$vcov)
  rownames(varcov_FE) <- rownames(sum$vcov)

  variance <- list()
  variance$alpha <- var_alpha
  variance$FE <- varcov_FE

  sigma <- sum$sigma

  # prediction
  linear_pred <- X.model %*% FE_coefficient
  colnames(linear_pred) <- "Fixed Fitted"
  rownames(linear_pred) <- seq_len(nrow(linear_pred))

  pred <- matrix(fitted(model), ncol = 1)
  colnames(pred) <- "Prediction"
  rownames(pred) <- seq_len(nrow(pred))

  res <- matrix(residuals(model), ncol = 1)
  colnames(res) <- "Residuals"
  rownames(res) <- seq_len(nrow(res))

  # AIC and BIC
  log_likelihood <- logLik(model)
  AIC <- AIC(model)
  BIC <- BIC(model)

  char_list <- list(Y.char = Y.char,
                    ProvID.char = ProvID.char,
                    within_terms = within_terms,
                    between_terms = between_terms,
                    other.vars = other.char)

  result <- structure(list(coefficient = coefficient,
                           variance = variance,
                           sigma = sigma,
                           fitted = pred,
                           observation = Y,
                           residuals = res,
                           linear_pred = linear_pred,
                           Loglkd = log_likelihood,
                           AIC = AIC,
                           BIC = BIC),
                      class = "linear_cre")  # define a list for prediction

  result$data_include <- data
  result$char_list <- char_list

  attr(result, "model") <- model

  return(result)
}


#' @noRd
#' @exportS3Method print linear_cre
print.linear_cre <- function(x, ...) {
  x2 <- x
  attr(x2, "model") <- NULL
  base::print.default(x2, ...)
  invisible(x)
}
