#' Main Function for fitting the random effect linear model
#'
#' Fit a random effect linear model via \code{\link[lme4]{lmer}} from the \code{lme4} package.
#'
#' @param formula a two-sided formula object describing the model to be fitted,
#' with the response variable on the left of a ~ operator and covariates on the right,
#' separated by + operators. The random effect of the provider identifier is specified using \code{(1 | )}.
#' @param data a data frame containing the variables named in the `formula`,
#' or the columns specified by `Y.char`, `Z.char`, and `ProvID.char`.
#' @param Y.char a character string specifying the column name of the response variable in the `data`.
#' @param Z.char a character vector specifying the column names of the covariates in the `data`.
#' @param ProvID.char a character string specifying the column name of the provider identifier in the `data`.
#' @param Y a numeric vector representing the response variable.
#' @param Z a matrix or data frame representing the covariates, which can include both numeric and categorical variables.
#' @param ProvID a numeric vector representing the provider identifier.
#' @param \dots additional arguments passed to \code{\link[lme4]{lmer}} for further customization.
#'
#' @return A list of objects with S3 class \code{"linear_re"}:
#' \item{coefficient}{a list containing the estimated coefficients:
#'   \code{FE}, the fixed effects for each predictor and the intercept, and \code{RE}, the random effects for each provider.}
#' \item{variance}{a list containing the variance estimates:
#'   \code{FE}, the variance-covariance matrix of the fixed effect coefficients, and \code{RE}, the variance of the random effects.}
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
#' \item{Loglkd}{the log-likelihood.}
#' \item{AIC}{Akaike information criterion.}
#' \item{BIC}{Bayesian information criterion.}
#'
#' @details
#' This function is used to fit a random effect linear model of the form:
#' \deqn{Y_{ij} = \mu + \alpha_i + \mathbf{Z}_{ij}^\top\boldsymbol\beta + \epsilon_{ij}}
#' where \eqn{Y_{ij}} is the continuous outcome for individual \eqn{j} in provider \eqn{i},
#' \eqn{\mu} is the overall intercept, \eqn{\alpha_i} is the random effect for provider \eqn{i},
#' \eqn{\mathbf{Z}_{ij}} are the covariates, and \eqn{\boldsymbol\beta} is the vector of coefficients for the covariates.
#'
#' The model is fitted by overloading the \code{\link[lme4]{lmer}} function from the \code{lme4} package.
#' Three different input formats are accepted:
#' a formula and dataset, where the formula is of the form \code{response ~ covariates + (1 | provider)}, with \code{provider} representing the provider identifier;
#' a dataset along with the column names of the response, covariates, and provider identifier;
#' or the outcome vector \eqn{\boldsymbol{Y}}, the covariate matrix or data frame \eqn{\mathbf{Z}}, and the provider identifier vector.
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
#' formula <- as.formula(paste("outcome ~", paste(covar.char, collapse = " + "), "+ (1|ProvID)"))
#'
#' # Fit random effect linear model using three input formats
#' fit_re1 <- linear_re(Y = outcome, Z = covar, ProvID = ProvID)
#' fit_re2 <- linear_re(data = data, Y.char = outcome.char,
#' Z.char = covar.char, ProvID.char = ProvID.char)
#' fit_re3 <- linear_re(formula, data)
#'
#' @references
#' Bates D, Maechler M, Bolker B, Walker S (2015). \emph{Fitting Linear Mixed-Effects Models Using lme4}.
#' Journal of Statistical Software, 67(1), 1-48.
#' \cr

linear_re <- function(formula = NULL, data = NULL,
                      Y = NULL, Z = NULL, ProvID = NULL,
                      Y.char = NULL, Z.char = NULL, ProvID.char = NULL, ...) {
  if (!is.null(formula) && !is.null(data)) {
    message("Input format: formula and data.")

    terms <- terms(formula)
    response <- as.character(attr(terms, "variables"))[2]
    predictors <- attr(terms, "term.labels")

    RE_term <- predictors[grepl("\\|", predictors)]
    id_var <- trimws(gsub(".*\\|", "", RE_term))

    Y.char <- response
    ProvID.char <- id_var
    Z.char <- predictors[!grepl("\\|", predictors)]
    if (!all(c(Y.char, Z.char, ProvID.char) %in% colnames(data)))
      stop("Formula contains variables not in the data or is incorrectly structured.", call.=F)

    data <- data[,c(Y.char, ProvID.char, Z.char)]
    data <- data[complete.cases(data), ] # Remove rows with missing values

    data <- data[order(factor(data[[id_var]])),]
    # Y <- data[, Y.char, drop = F]
    # Z <- as.matrix(data[, Z.char, drop = F])
    # ProvID <- data[, ProvID.char, drop = F]
    fit_re <- lmer(formula, data, ...)
  }
  else if (!is.null(data) && !is.null(Y.char) && !is.null(Z.char) && !is.null(ProvID.char)) {
    message("Input format: data, Y.char, Z.char, and ProvID.char.")

    if (!all(c(Y.char, Z.char, ProvID.char) %in% colnames(data)))
      stop("Some of the specified columns are not in the data!", call.=FALSE)

    data <- data[,c(Y.char, ProvID.char, Z.char)]
    data <- data[complete.cases(data), ] # Remove rows with missing values
    data <- data[order(factor(data[, ProvID.char])),]
    # Y <- data[, Y.char, drop = F]
    # Z <- as.matrix(data[, Z.char, drop = F])
    # ProvID <- data[, ProvID.char, drop = F]

    formula <- as.formula(paste(Y.char, "~ (1|", ProvID.char, ") +", paste(Z.char, collapse = " + ")))
    fit_re <- lmer(formula, data, ...)
  }
  else if (!is.null(Y) && !is.null(Z) && !is.null(ProvID)) {
    message("Input format: Y, Z, and ProvID.")

    if (length(Y) != length(ProvID) | length(ProvID) != nrow(Z)){
      stop("Dimensions of the input data do not match!!", call.=F)}

    data <- as.data.frame(cbind(Y, ProvID, Z))
    data <- data[complete.cases(data), ] # Remove rows with missing values
    Y.char <- colnames(data)[1]
    ProvID.char <- colnames(data)[2]
    Z.char <- colnames(Z)
    data <- data[order(factor(data[,ProvID.char])),]

    # Z <- as.matrix(data[,Z.char], drop = F)
    # Y <- as.matrix(data[, Y.char, drop = F])
    # ProvID <- as.matrix(data[, ProvID.char, drop = F])

    formula <- as.formula(paste(Y.char, "~ (1|", ProvID.char, ")+", paste0(Z.char, collapse = "+")))

    fit_re <- lmer(formula, data, ...)
  }

  X.model <- model.matrix(fit_re)
  Y <- as.matrix(data[, Y.char, drop = F])
  ProvID <- as.matrix(data[, ProvID.char, drop = F])
  data <- as.data.frame(cbind(Y, ProvID, X.model))

  n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)
  m <- length(n.prov) # number of providers
  n <- sum(n.prov) # number of observations
  # p <- length(Z.char) # number of covariates

  # Coefficients
  FE_coefficient <- matrix(fixef(fit_re))
  colnames(FE_coefficient) <- "Coefficient"
  rownames(FE_coefficient) <- names(fixef(fit_re))

  RE_coefficient <- as.matrix(ranef(fit_re)[[ProvID.char]], ncol = 1)
  colnames(RE_coefficient) <- "alpha"
  rownames(RE_coefficient) <- names(n.prov)

  coefficient <- list()
  coefficient$FE <- FE_coefficient
  coefficient$RE <- RE_coefficient

  # Variance
  sum <- summary(fit_re)
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

  pred <- matrix(fitted(fit_re), ncol = 1)
  colnames(pred) <- "Prediction"
  rownames(pred) <- seq_len(nrow(pred))

  res <- matrix(residuals(fit_re), ncol = 1)
  colnames(res) <- "Residuals"
  rownames(res) <- seq_len(nrow(res))

  # AIC and BIC
  log_likelihood <- logLik(fit_re)
  AIC <- AIC(fit_re)
  BIC <- BIC(fit_re)

  char_list <- list(Y.char = Y.char,
                    ProvID.char = ProvID.char,
                    Z.char = rownames(FE_coefficient)[2:length(rownames(FE_coefficient))])

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
                      class = "linear_re")  # define a list for prediction

  result$data_include <- data
  result$char_list <- char_list

  attr(result, "model") <- fit_re

  return(result)
}


#' @noRd
#' @exportS3Method print linear_re
print.linear_re <- function(x, ...) {
  x2 <- x
  attr(x2, "model") <- NULL
  base::print.default(x2, ...)
  invisible(x)
}


#===================COMMENT OUT TO USE LATER=========================
# linear_re.nlme <- function(formula = NULL, data = NULL,
#                            Y = NULL, Z = NULL, ID = NULL,
#                            Y.char = NULL, Z.char = NULL, ID.char = NULL, ...) {
#   if (!is.null(formula) && !is.null(data)) {
#     terms <- terms(formula)
#     response <- as.character(attr(terms, "variables"))[2]
#     predictors <- attr(terms, "term.labels")
#
#     RE_term <- predictors[grepl("\\|", predictors)]
#     id_var <- trimws(gsub(".*\\|", "", RE_term))
#
#     Y.char <- response
#     ID.char <- id_var
#     Z.char <- predictors[!grepl("\\|", predictors)]
#     data <- data[order(factor(data[, id_var])),]
#     # Y <- data[, Y.char, drop = F]
#     # Z <- as.matrix(data[, Z.char, drop = F])
#     # ID <- data[, ID.char, drop = F]
#     fe_formula <- reformulate(Z.char, response = Y.char)
#     re_formula <- as.formula(paste("~ 1 |", id_var))
#     fit_re <- lme(fixed = fe_formula, random = re_formula, data = data, ...)
#   }
#   else if (!is.null(data) && !is.null(Y.char) && !is.null(Z.char) && !is.null(ID.char)) {
#     if (!all(c(Y.char, Z.char, ID.char) %in% colnames(data)))
#       stop("Some of the specified columns are not in the data!", call.=FALSE)
#     data <- data[order(factor(data[, ID.char])),]
#     # Y <- data[, Y.char, drop = F]
#     # Z <- as.matrix(data[, Z.char, drop = F])
#     # ID <- data[, ID.char, drop = F]
#
#     fe_formula <- reformulate(Z.char, response = Y.char)
#     re_formula <- as.formula(paste("~ 1 |", ID.char))
#     fit_re <- lme(fixed = fe_formula, random = re_formula, data = data, ...)
#   }
#   else if (!is.null(Y) && !is.null(Z) && !is.null(ID)) {
#     if (length(Y) != length(ID) | length(ID) != nrow(Z)){
#       stop("Dimensions of the input data do not match!!", call.=F)}
#
#     data <- as.data.frame(cbind(Y, ID, Z))
#     Y.char <- colnames(data)[1]
#     ID.char <- colnames(data)[2]
#     Z.char <- colnames(Z)
#     data <- data[order(factor(data[,ID.char])),]
#
#     # Z <- as.matrix(data[,Z.char], drop = F)
#     # Y <- as.matrix(data[, Y.char, drop = F])
#     # ID <- as.matrix(data[, ID.char, drop = F])
#
#     fe_formula <- reformulate(Z.char, response = Y.char)
#     re_formula <- as.formula(paste("~ 1 |", ID.char))
#     fit_re <- lme(fixed = fe_formula, random = re_formula, data = data, ...)
#   }
#
#   X.model <- model.matrix(formula(fit_re), data = data)
#   Y <- as.matrix(data[, Y.char, drop = F])
#   ID <- as.matrix(data[, ID.char, drop = F])
#   data <- as.data.frame(cbind(Y, ID, X.model))
#
#   n.prov <- sapply(split(data[, Y.char], data[, ID.char]), length)
#   m <- length(n.prov) # number of providers
#   n <- sum(n.prov) # number of observations
#   # p <- length(Z.char) # number of covariates
#
#   # Coefficients
#   FE_coefficient <- matrix(fixef(fit_re))
#   colnames(FE_coefficient) <- "Coefficient"
#   rownames(FE_coefficient) <- names(fixef(fit_re))
#
#   RE_coefficient <- as.matrix(ranef(fit_re), ncol = 1)
#   colnames(RE_coefficient) <- "alpha"
#   rownames(RE_coefficient) <- names(n.prov)
#
#   coefficient <- list()
#   coefficient$FE <- FE_coefficient
#   coefficient$RE <- RE_coefficient
#
#   # Variance
#   sum <- summary(fit_re)
#   var_alpha <- matrix(as.numeric(VarCorr(fit_re)[1]))
#   colnames(var_alpha) <- "Variance.Alpha"
#   rownames(var_alpha) <- "ID"
#
#   varcov_FE <- matrix(sum$varFix, ncol = length(FE_coefficient))
#   colnames(varcov_FE) <- colnames(sum$varFix)
#   rownames(varcov_FE) <- rownames(sum$varFix)
#
#   variance <- list()
#   variance$alpha <- var_alpha
#   variance$FE <- varcov_FE
#
#   sigma <- sum$sigma
#
#   # prediction
#   linear_pred <- sum$fitted[, 1, drop = FALSE]
#   colnames(linear_pred) <- "Fixed"
#   pred <- sum$fitted[, 2, drop = FALSE]
#   colnames(pred) <- "prediction"
#   res <- matrix(residuals(fit_re), ncol = 1)
#   colnames(res) <- "Residuals"
#   rownames(res) <- seq_len(nrow(res))
#
#
#   char_list <- list(Y.char = Y.char,
#                     ID.char = ID.char,
#                     Z.char = Z.char)
#
#   result <- structure(list(coefficient = coefficient,
#                            variance = variance,
#                            sigma = sigma,
#                            prediction = pred,
#                            observation = Y,
#                            residuals = res,
#                            linear_pred = linear_pred,
#                            prov = data[, ID.char],
#                            model = fit_re),
#                       class = "linear_re")  #define a list for prediction
#
#   result$data_include <- data
#   result$char_list <- char_list
#
#   return(result)
# }
