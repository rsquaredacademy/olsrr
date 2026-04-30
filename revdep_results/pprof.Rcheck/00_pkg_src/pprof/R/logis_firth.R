#' Main function for fitting the fixed effect logistic model using firth correction
#'
#' Fixed effects (FE) models suffer from separation issues when all outcomes in a cluster are the same,
#' leading to infinite estimates and unreliable inference.
#' Firth’s corrected logistic regression (FLR) overcomes this limitation and
#' outperforms both FE and random effects (RE) models in terms of bias and RMSE.
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
#' @param max.iter maximum iteration number if the stopping criterion specified by `stop` is not satisfied. The default value is 10,000.
#' @param tol tolerance used for stopping the algorithm. See details in `stop` below. The default value is 1e-5.
#' @param bound a positive number to avoid inflation of provider effects. The default value is 10.
#' @param cutoff An integer specifying the minimum number of observations required for providers.
#' Providers with fewer observations than the cutoff will be labeled as \code{"include = 0"} and excluded from model fitting. The default is 10.
#' @param threads a positive integer specifying the number of threads to be used. The default value is 1.
#' @param message a Boolean indicating whether to print the progress of the fitting process. The default is TRUE.
#'
#' @details
#' The function accepts three different input formats:
#' a formula and dataset, where the formula is of the form \code{response ~ covariates + id(provider)}, with \code{provider} representing the provider identifier;
#' a dataset along with the column names of the response, covariates, and provider identifier;
#' or the binary outcome vector \eqn{\boldsymbol{Y}}, the covariate matrix or data frame \eqn{\mathbf{Z}}, and the provider identifier vector.
#'
#' This function utilizes OpenMP for parallel processing. For macOS, to enable multi-threading,
#' users may need to install the OpenMP library (e.g., brew install libomp) or use a supported compiler such as GCC.
#' If OpenMP is not detected during installation, the function will transparently fall back to single-threaded execution.
#'
#' @seealso \code{\link{data_check}}
#'
#' @return A list of objects with S3 class \code{"logis_fe"}:
#' \item{coefficient}{a list containing the estimated coefficients:
#'   \code{beta}, the fixed effects for each predictor, and \code{gamma}, the effect for each provider.}
#' \item{variance}{a list containing the variance estimates:
#'   \code{beta}, the variance-covariance matrix of the predictor coefficients, and \code{gamma}, the variance of the provider effects.}
#' \item{linear_pred}{the linear predictor of each individual.}
#' \item{fitted}{the predicted probability of each observation having a response of 1.}
#' \item{observation}{the original response of each individual.}
#' \item{Loglkd}{the log-likelihood.}
#' \item{AIC}{Akaike info criterion.}
#' \item{BIC}{Bayesian info criterion.}
#' \item{AUC}{area under the ROC curve.}
#' \item{char_list}{a list of the character vectors representing the column names for
#' the response variable, covariates, and provider identifier.
#' For categorical variables, the names reflect the dummy variables created for each category.}
#' \item{data_include}{the data used to fit the model, sorted by the provider identifier.
#' For categorical covariates, this includes the dummy variables created for
#' all categories except the reference level. Additionally, it contains three extra columns:
#' \code{included}, indicating whether the provider is included based on the `cutoff` argument;
#' \code{all.events}, indicating if all observations in the provider are 1;
#' \code{no.events}, indicating if all observations in the provider are 0.}
#'
#' @examples
#' data(ExampleDataBinary)
#' outcome <- ExampleDataBinary$Y
#' covar <- ExampleDataBinary$Z
#' ProvID <- ExampleDataBinary$ProvID
#' data <- data.frame(outcome, ProvID, covar)
#' covar.char <- colnames(covar)
#' outcome.char <- colnames(data)[1]
#' ProvID.char <- colnames(data)[2]
#' formula <- as.formula(paste("outcome ~", paste(covar.char, collapse = " + "), "+ id(ProvID)"))
#'
#' # Fit logistic linear effect model using three input formats
#' fit_fe1 <- logis_firth(Y = outcome, Z = covar, ProvID = ProvID)
#' fit_fe2 <- logis_firth(data = data, Y.char = outcome.char,
#' Z.char = covar.char, ProvID.char = ProvID.char)
#' fit_fe3 <- logis_firth(formula, data)
#'
#' @importFrom Rcpp evalCpp
#' @importFrom pROC auc
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stats complete.cases terms model.matrix reformulate median
#'
#' @references
#' Firth, D. (1993) Bias reduction of maximum likelihood estimates.
#' \emph{Biometrika}, \strong{80(1)}: 27-38.
#' \cr
#'
#' @export
#'
#' @useDynLib pprof, .registration = TRUE
#'
logis_firth <- function(formula = NULL, data = NULL,
                        Y.char = NULL, Z.char = NULL, ProvID.char = NULL,
                        Y = NULL, Z = NULL, ProvID = NULL,
                        max.iter = 1000, tol = 1e-5, bound = 10,
                        cutoff = 10, threads = 1, message = TRUE) {
  if (!is.null(formula) && !is.null(data)) {
    if (message == TRUE) message("Input format: formula and data.")

    formula_terms <- terms(formula)
    Y.char <- as.character(attr(formula_terms, "variables"))[2]
    predictors <- attr(formula_terms, "term.labels")

    ProvID.char <- gsub(".*id\\(([^)]+)\\).*", "\\1", predictors[grepl("id\\(", predictors)])
    Z.char <- predictors[!grepl("id\\(", predictors)]

    if (!all(c(Y.char, Z.char, ProvID.char) %in% colnames(data)))
      stop("Formula contains variables not in the data or is incorrectly structured.", call.=F)

    data <- data[,c(Y.char, ProvID.char, Z.char)]
    data <- data[complete.cases(data), ] # Remove rows with missing values

    Y <- data[,Y.char, drop = F]
    Z <- model.matrix(reformulate(Z.char), data)[, -1, drop = F]
    # Z <- model.matrix(~ data[[predictors]] - 1)
    ProvID <- data[,ProvID.char, drop = F]
  }
  else if (!is.null(data) && !is.null(Y.char) && !is.null(Z.char) && !is.null(ProvID.char)) {
    if (message == TRUE) message("Input format: data, Y.char, Z.char, and ProvID.char.")

    if (!all(c(Y.char, Z.char, ProvID.char) %in% colnames(data)))
      stop("Some of the specified columns are not in the data!", call.=FALSE)

    data <- data[,c(Y.char, ProvID.char, Z.char)]
    data <- data[complete.cases(data), ] # Remove rows with missing values
    Y <- data[, Y.char]
    Z <- model.matrix(reformulate(Z.char), data)[, -1, drop = FALSE]
    ProvID <- data[, ProvID.char, drop = F]
  }
  else if (!is.null(Y) && !is.null(Z) && !is.null(ProvID)) {
    if (message == TRUE) message("Input format: Y, Z, and ProvID.")

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
  Y.char <- colnames(data)[1]
  ProvID.char <- colnames(data)[2]
  Z.char <- colnames(Z)

  data <- data[order(factor(data[,ProvID.char])),] # sort data by provider ID
  prov.size <- as.integer(table(data[,ProvID.char])) # provider sizes
  prov.size.long <- rep(prov.size,prov.size) # provider sizes assigned to patients
  data$included <- 1 * (prov.size.long >= cutoff) # create variable 'included' as an indicator
  if (message == TRUE) warning(sum(prov.size<=cutoff)," out of ",length(prov.size),
                               " providers considered small and filtered out!",immediate.=T,call.=F)

  prov.list <- unique(data[data$included==1,ProvID.char])   # a reduced list of provider IDs
  prov.no.events <-      # providers with no events
    prov.list[sapply(split(data[data$included==1,Y.char], factor(data[data$included==1,ProvID.char])),sum)==0]
  data$no.events <- 0
  data$no.events[data[,ProvID.char]%in%c(prov.no.events)] <- 1
  if (message == TRUE) message(paste(length(prov.no.events),"out of",length(prov.list),
                                     "remaining providers with no events."))
  prov.all.events <-     # providers with all events
    prov.list[sapply(split(1-data[data$included==1,Y.char],factor(data[data$included==1,ProvID.char])),sum)==0]
  data$all.events <- 0
  data$all.events[data[,ProvID.char]%in%c(prov.all.events)] <- 1
  if (message == TRUE) message(paste(length(prov.all.events),"out of",length(prov.list),
                                     "remaining providers with all events."))
  if (message == TRUE) message(paste0("After screening, ", round(sum(data[data$included==1,Y.char])/length(data[data$included==1,Y.char])*100,2),
                                      "% of all records exhibit occurrences of events (Y = 1)"))


  # for the remaining parts, only use the data with "included==1" ("cutoff" of provider size)
  data <- data[data$included==1,]
  n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length) # provider-specific number of discharges
  n.events.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), sum) # provider-specific number of events
  Z <- as.matrix(data[,Z.char])
  gamma.prov <- rep(log(mean(data[,Y.char])/(1-mean(data[,Y.char]))), length(n.prov))
  beta <- rep(0, NCOL(Z))

  n_obs <- nrow(data)

  ls <- logis_firth_prov(as.matrix(data[,Y.char]), Z, n.prov, gamma.prov, beta,
                         n_obs = n_obs, m = length(n.prov), threads = threads, tol = tol,
                         max_iter = max.iter, bound = bound, message = message)



  gamma.prov <- as.numeric(ls$gamma)
  beta <- as.numeric(ls$beta)

  # Coefficient
  beta <- matrix(beta)
  gamma.prov <- matrix(gamma.prov)
  dimnames(beta) <- list(Z.char, "beta")
  dimnames(gamma.prov) <- list(names(n.prov), "gamma")

  # Variance
  var_ls <- logis_fe_var(data[,Y.char], as.matrix(data[,Z.char]), n.prov, gamma.prov, beta)
  varcov_beta <- var_ls$var.beta
  rownames(varcov_beta) <- Z.char
  colnames(varcov_beta) <- Z.char
  var_gamma <- var_ls$var.gamma
  rownames(var_gamma) <- names(n.prov)
  colnames(var_gamma) <- "Variance.Gamma"

  variance <- list()
  variance$beta <- varcov_beta
  variance$gamma <- var_gamma

  # AIC and BIC
  gamma.obs <- rep(gamma.prov, n.prov)
  Loglkd <- sum((gamma.obs+Z%*%beta)*data[,Y.char]-log(1+exp(gamma.obs+Z%*%beta)))
  # neg2Loglkd <- -2*sum((gamma.obs+Z%*%beta)*data[,Y.char]-log(1+exp(gamma.obs+Z%*%beta)))
  neg2Loglkd <- -2 * Loglkd
  AIC <- neg2Loglkd + 2 * (length(gamma.prov)+length(beta))
  BIC <- neg2Loglkd + log(nrow(data)) * (length(gamma.prov)+length(beta))

  # df.prov <- data.frame(Obs_provider = sapply(split(data[,Y.char],data[,ProvID.char]),sum),
  #                       gamma_est = gamma.prov) #original gamma-hat, for internal using
  linear_pred <- Z %*% beta
  colnames(linear_pred) <- "Linear Predictor"
  rownames(linear_pred) <- seq_len(nrow(linear_pred))
  pred <- as.numeric(plogis(gamma.obs + linear_pred))
  fitted <- matrix(pred, ncol = 1)
  colnames(fitted) <- "Predicted Probability"
  rownames(fitted) <- seq_len(nrow(fitted))

  coefficient <- list()
  coefficient$beta <- beta
  coefficient$gamma <- gamma.prov

  char_list <- list(Y.char = Y.char,
                    ProvID.char = ProvID.char,
                    Z.char = Z.char)

  return_ls <- structure(list(coefficient = coefficient,
                              variance = variance,
                              fitted = fitted, #predicted probability
                              observation = data[, Y.char], #patient-level obs
                              linear_pred = linear_pred, #linear predictor
                              Loglkd = Loglkd,
                              AIC = AIC,
                              BIC = BIC
  ),
  class = "logis_fe")
  # if (AUC) {
  #   AUC <- pROC::auc(data[,Y.char], pred)
  #   return_ls$AUC <- AUC[1]
  # }
  #AUC <- pROC::auc(data[,Y.char], pred)
  if(message == TRUE) AUC <- pROC::auc(data[,Y.char], pred)
  else AUC <- suppressMessages(pROC::auc(data[, Y.char], pred))
  return_ls$AUC <- AUC[1]

  # return_ls$df.prov <- df.prov
  return_ls$char_list <- char_list
  return_ls$data_include <- data

  return(return_ls)
}
