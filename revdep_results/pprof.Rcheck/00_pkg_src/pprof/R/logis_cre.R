#' Main Function for fitting correlated random effect logistic model
#'
#' Fit a correlated random effect logistic model via \code{\link[lme4]{glmer}} from the \code{lme4} package.
#'
#' @param data a data frame containing all variables.
#' @param Y.char a character string specifying the column name of the response variable in the `data`.
#' @param wb.char a character vector specifying covariates to be decomposed into
#'   within (\code{*_within}) and between (\code{*_bar}) components.
#' @param other.char a character vector specifying additional covariates to include in the model without decomposition.
#' @param ProvID.char a character string specifying the column name of the provider identifier in the `data`.
#' @param \dots additional arguments passed to \code{\link[lme4]{glmer}} for further customization.
#'
#' @return A list of objects with S3 class \code{"logis_cre"}:
#' \item{coefficient}{a list containing the estimated coefficients:
#'   \code{FE}, the fixed effects for each predictor and the intercept, and \code{RE}, the random effects for each provider.}
#' \item{variance}{a list containing the variance estimates:
#'   \code{FE}, the variance-covariance matrix of the fixed effect coefficients, and \code{RE}, the variance of the random effects.}
#' \item{fitted}{the fitted values of each individual.}
#' \item{observation}{the original response of each individual.}
#' \item{linear_pred}{the linear predictor of each individual.}
#' \item{data_include}{the processed data used to fit the model, sorted by the provider identifier.
#' This includes the within-group (\code{*_within}) and between-group (\code{*_bar}) components for variables specified in \code{wb.char}.
#' For categorical covariates, it includes the dummy variables created for
#' all categories except the reference level.}
#' \item{char_list}{a list of the character vectors representing the column names for
#' the response variable, provider identifier, the generated names for covariates with decomposition (\code{wb.char},
#' and covariates included without decomposition (\code{other.char}).
#' For categorical variables, the names reflect the dummy variables created for each category.}
#' \item{Loglkd}{the log-likelihood.}
#' \item{AIC}{Akaike information criterion.}
#' \item{BIC}{Bayesian information criterion.}
#'
#' @details
#' Fit a correlated random effect logistic model using \code{\link[lme4]{glmer}} with a Mundlak
#' within–between decomposition for selected covariates. For each
#' decomposed covariate \eqn{Z_k}, the function constructs
#' \eqn{Z_{k,\mathrm{bar},i} = \frac{1}{n_i}\sum_j Z_{k,ij}} (the group mean, "between")
#' and \eqn{Z_{k,\mathrm{within},ij} = Z_{k,ij} - Z_{k,\mathrm{bar},i}} (the within-group deviation),
#' and estimates the model
#' \deqn{\mathrm{logit}\left(P(Y_{ij}=1)\right) = \mu + \alpha_i + \sum_k \beta_{k,W} Z_{k,\mathrm{within},ij}
#'   + \sum_k \beta_{k,B} Z_{k,\mathrm{bar},i} + \mathbf{X}_{ij}^\top\gamma,}
#' where \eqn{\alpha_i \sim \mathcal{N}(0,\sigma_\alpha^2)} is a random intercept.
#'
#' The function creates, for every name in \code{wb.char}, two columns:
#' \code{<var>_bar} (group mean within \code{ProvID.char}) and
#' \code{<var>_within} (observation minus its group mean).
#' The fitted model formula is:
#' \preformatted{
#'   Y ~ <all *_within> + <all *_bar> + <other.char> + (1 | ProvID)
#' }
#' This model is fitted using \code{glmer} with \code{family = binomial(link = "logit")}.
#'
#' In addition to these input formats, all arguments from the \code{\link[lme4]{glmer}} function can be modified via \code{\dots},
#' allowing for customization of model fitting options such as controlling the optimization method or adjusting convergence criteria.
#'
#' If issues arise during model fitting, consider using the \code{data_check} function to perform a data quality check.
#' For datasets with missing values, this function automatically removes observations (rows) with any missing values before fitting the model.
#'
#' @seealso \code{\link{data_check}}
#'
#' @importFrom lme4 glmer fixef ranef
#' @importFrom stats complete.cases as.formula model.matrix fitted logLik
#' @importFrom dplyr group_by mutate ungroup across
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @examples
#' data(ExampleDataBinary)
#' outcome <- ExampleDataBinary$Y
#' covar <- ExampleDataBinary$Z
#' ProvID <- ExampleDataBinary$ProvID
#' data <- data.frame(outcome, ProvID, covar)
#' outcome.char <- colnames(data)[1]
#' ProvID.char <- colnames(data)[2]
#' wb.char <- c("z1", "z2")
#' other.char <- c("z3", "z4", "z5")
#'
#' # Fit a correlated random effect linear model
#' fit_cre <- logis_cre(data = data, Y.char = outcome.char, ProvID.char = ProvID.char,
#' wb.char = wb.char, other.char = other.char)
#'
#' @references
#' Bates D, Maechler M, Bolker B, Walker S (2015). \emph{Fitting Linear Mixed-Effects Models Using lme4}.
#' Journal of Statistical Software, 67(1), 1-48.
#' \cr

logis_cre <- function(data, Y.char, wb.char, other.char = NULL, ProvID.char, ...) {
  needed <- c(Y.char, ProvID.char, wb.char, if (!is.null(other.char)) other.char)
  if (!all(needed %in% colnames(data))) {
    stop("Some specified columns are not in `data`.", call. = FALSE)
  }

  df_cre <- data %>%
    group_by(.data[[ProvID.char]]) %>%
    mutate(
      across(all_of(wb.char),
             ~ mean(.x, na.rm = TRUE),
             .names = "{.col}_bar"),      # Between-subject effect
      across(all_of(wb.char),
             ~ .x - mean(.x, na.rm = TRUE),
             .names = "{.col}_within")    # Within-subject effect
    ) %>%
    ungroup()

  within_terms  <- paste0(wb.char, "_within")
  between_terms <- paste0(wb.char, "_bar")
  rhs_terms     <- c(within_terms, between_terms, other.char)

  f <- as.formula(paste(Y.char, "~", paste(rhs_terms, collapse = " + "), "+ (1 |", ProvID.char, ")"))

  df_use <- df_cre[, c(Y.char, ProvID.char, rhs_terms), drop = FALSE]
  df_use <- df_use[complete.cases(df_use), ]
  df_use <- df_use[order(factor(df_use[[ProvID.char]])), ]

  model <- glmer(f, data = df_use, family = binomial(link = "logit"), ...)

  X.model <- model.matrix(model)
  Y <- df_use[, Y.char, drop = FALSE]
  ProvID <- df_use[, ProvID.char, drop = FALSE]
  data <- as.data.frame(cbind(Y, ProvID, X.model))

  n.prov <- sapply(split(data[[Y.char]], data[[ProvID.char]]), length)
  m <- length(n.prov) # number of providers
  n <- sum(n.prov)   # number of observations

  # Coefficients
  FE_coefficient <- matrix(fixef(model))
  colnames(FE_coefficient) <- "Coefficient"
  rownames(FE_coefficient) <- names(fixef(model))

  RE_coefficient <- as.matrix(ranef(model)[[ProvID.char]], ncol = 1)
  colnames(RE_coefficient) <- "alpha"
  rownames(RE_coefficient) <- rownames(ranef(model)[[ProvID.char]])

  coefficient <- list()
  coefficient$FE <- FE_coefficient
  coefficient$RE <- RE_coefficient

  # Variance Components
  sum_model <- summary(model)
  var_alpha <- matrix(as.data.frame(sum_model$varcor)[1, "vcov"])
  colnames(var_alpha) <- "Variance.Alpha"
  rownames(var_alpha) <- ProvID.char

  varcov_FE <- as.matrix(sum_model$vcov)
  colnames(varcov_FE) <- colnames(sum_model$vcov)
  rownames(varcov_FE) <- rownames(sum_model$vcov)

  variance <- list()
  variance$alpha <- var_alpha
  variance$FE <- varcov_FE

  # Predictions
  # Linear predictors on the logit scale (fixed effects only)
  linear_pred_link <- X.model %*% FE_coefficient
  colnames(linear_pred_link) <- "Fixed Fitted"

  # Fitted values (probabilities) including random effects
  pred_prob <- matrix(fitted(model), ncol = 1)
  colnames(pred_prob) <- "Prediction"

  # Residuals (defaults to deviance residuals for glmer)
  # res <- matrix(residuals(model), ncol = 1)
  # colnames(res) <- "Residuals"

  # Model Fit Statistics
  log_likelihood <- logLik(model)
  AIC <- AIC(model)
  BIC <- BIC(model)

  # 7. Structure and Return Results
  char_list <- list(Y.char = Y.char,
                    ProvID.char = ProvID.char,
                    within_terms = within_terms,
                    between_terms = between_terms,
                    other.vars = other.char)

  result <- structure(list(coefficient = coefficient,
                           variance = variance,
                           fitted = pred_prob,
                           observation = Y,
                           linear_pred = linear_pred_link,
                           Loglkd = log_likelihood,
                           AIC = AIC,
                           BIC = BIC,
                           data_include = data,
                           char_list = char_list),
                      class = "logis_cre")

  attr(result, "model") <- model

  return(result)
}


#' @noRd
#' @exportS3Method print linear_re
print.logis_cre <- function(x, ...) {
  x2 <- x
  attr(x2, "model") <- NULL
  base::print.default(x2, ...)
  invisible(x)
}
