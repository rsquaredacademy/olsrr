#' Main function for fitting the fixed effect logistic model
#'
#' Fit a fixed effect logistic model via Serial blockwise inversion Newton (SerBIN) or block ascent Newton (BAN) algorithm.
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
#' @param method a string specifying the algorithm to be used. The default value is "SerBIN".
#'   \itemize{
#'   \item{\code{"SerBIN"}} uses the Serial blockwise inversion Newton algorithm to fit the model (See [Wu et al. (2022)](https://onlinelibrary.wiley.com/doi/full/10.1002/sim.9387)).
#'   \item{\code{"BAN"}} uses the block ascent Newton algorithm to fit the model (See [He et al. (2013)](https://link.springer.com/article/10.1007/s10985-013-9264-6)).
#'   }
#' @param max.iter maximum iteration number if the stopping criterion specified by `stop` is not satisfied. The default value is 10,000.
#' @param tol tolerance used for stopping the algorithm. See details in `stop` below. The default value is 1e-5.
#' @param bound a positive number to avoid inflation of provider effects. The default value is 10.
#' @param cutoff An integer specifying the minimum number of observations required for providers.
#' Providers with fewer observations than the cutoff will be labeled as \code{"include = 0"} and excluded from model fitting. The default is 10.
#' @param backtrack a Boolean indicating whether backtracking line search is implemented. The default is FALSE.
#' @param stop a character string specifying the stopping rule to determine convergence.
#' \itemize{
#' \item{\code{"beta"}} stop the algorithm when the infinity norm of the difference between current and previous beta coefficients is less than the `tol`.
#' \item{\code{"relch"}} stop the algorithm when the \eqn{(loglik(m)-loglik(m-1))/(loglik(m))} (the difference between the log-likelihood of
#' the current iteration and the previous iteration divided by the log-likelihood of the current iteration) is less than the `tol`.
#' \item{\code{"ratch"}} stop the algorithm when \eqn{(loglik(m)-loglik(m-1))/(loglik(m)-loglik(0))} (the difference between the log-likelihood of
#' the current iteration and the previous iteration divided by the difference of the log-likelihood of the current iteration and the initial iteration)
#' is less than the `tol`.
#' \item{\code{"all"}} stop the algorithm when all the stopping rules (`"beta"`, `"relch"`, `"ratch"`) are met.
#' \item{\code{"or"}} stop the algorithm if any one of the rules (`"beta"`, `"relch"`, `"ratch"`) is met.
#' }
#' The default value is `or`. If `iter.max` is achieved, it overrides any stop rule for algorithm termination.
#' @param threads a positive integer specifying the number of threads to be used. The default value is 1.
#' @param message a Boolean indicating whether to print the progress of the fitting process. The default is TRUE.
#'
#' @details
#' The function accepts three different input formats:
#' a formula and dataset, where the formula is of the form \code{response ~ covariates + id(provider)}, with \code{provider} representing the provider identifier;
#' a dataset along with the column names of the response, covariates, and provider identifier;
#' or the binary outcome vector \eqn{\boldsymbol{Y}}, the covariate matrix or data frame \eqn{\mathbf{Z}}, and the provider identifier vector.
#'
#' The default algorithm is based on Serial blockwise inversion Newton (SerBIN) proposed by
#' [Wu et al. (2022)](https://onlinelibrary.wiley.com/doi/full/10.1002/sim.9387),
#' but users can also choose to use the block ascent Newton (BAN) algorithm proposed by
#' [He et al. (2013)](https://link.springer.com/article/10.1007/s10985-013-9264-6) to fit the model.
#' Both methodologies build upon the Newton-Raphson method, yet SerBIN simultaneously updates both the provider effect and covariate coefficient.
#' This concurrent update necessitates the inversion of the whole information matrix at each iteration.
#' In contrast, BAN adopts a two-layer updating approach, where the covariate coefficient is sequentially fixed to update the provider effect,
#' followed by fixing the provider effect to update the covariate coefficient.
#'
#' We suggest using the default `"SerBIN"` option as it typically converges subsequently much faster for most datasets.
#' However, in rare cases where the SerBIN algorithm encounters second-order derivative irreversibility leading to an error,
#' users can consider using the `"BAN"` option as an alternative.
#' For a deeper understanding, please consult the original article for detailed insights.
#'
#' If issues arise during model fitting, consider using the \code{data_check} function to perform a data quality check,
#' which can help identify missing values, low variation in covariates, high-pairwise correlation, and multicollinearity.
#' For datasets with missing values, this function automatically removes observations (rows) with any missing values before fitting the model.
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
#' fit_fe1 <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID)
#' fit_fe2 <- logis_fe(data = data, Y.char = outcome.char,
#' Z.char = covar.char, ProvID.char = ProvID.char)
#' fit_fe3 <- logis_fe(formula, data)
#'
#' @importFrom Rcpp evalCpp
#' @importFrom pROC auc
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stats complete.cases terms model.matrix reformulate median
#'
#' @references
#' He K, Kalbfleisch, J, Li, Y, and et al. (2013) Evaluating hospital readmission rates in dialysis providers; adjusting for hospital effects.
#' \emph{Lifetime Data Analysis}, \strong{19}: 490-512.
#' \cr
#'
#' Wu, W, Yang, Y, Kang, J, He, K. (2022) Improving large-scale estimation and inference for profiling health care providers.
#' \emph{Statistics in Medicine}, \strong{41(15)}: 2840-2853.
#' \cr
#'
#' @export
#'
#' @useDynLib pprof, .registration = TRUE
#'
logis_fe <- function(formula = NULL, data = NULL,
                     Y.char = NULL, Z.char = NULL, ProvID.char = NULL,
                     Y = NULL, Z = NULL, ProvID = NULL,
                     method = "SerBIN", max.iter = 10000, tol = 1e-5, bound = 10,
                     cutoff = 10, backtrack = TRUE, stop = "or", threads = 1, message = TRUE) {
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

  start_time_cpp <- Sys.time()

  if (method == "SerBIN") {
    ls <- logis_BIN_fe_prov(as.matrix(data[,Y.char]), Z, n.prov, gamma.prov, beta,
                            threads = threads, tol, max.iter, bound, message, backtrack, stop)
    gamma.prov <- as.numeric(ls$gamma)
    beta <- as.numeric(ls$beta)
    }

  else if (method == "BAN") {
    ls <- logis_fe_prov(as.matrix(data[,Y.char]), Z, n.prov, gamma.prov, beta,
                        backtrack, max.iter, bound, tol, message, stop)
    gamma.prov <- as.numeric(ls$gamma)
    beta <- as.numeric(ls$beta)
  }
  else {
    stop("Argument 'method' NOT as required!")
  }

  end_time_cpp <- Sys.time()
  cpp_time <- end_time_cpp - start_time_cpp

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

  #return_ls$cpp_time <- cpp_time
  return(return_ls)
}

# logis_fe.fit <- function(Y, Z, ProvID, algorithm = "SerBIN", max.iter = 10000, tol = 1e-5, bound = 10,
#                      backtrack = TRUE, Rcpp = TRUE, AUC = FALSE, message = TRUE, cutoff = 10,
#                      stop = "or", check = FALSE){
#
#   # Check input
#   if (missing(Y) || missing(Z) || missing(ProvID))
#     stop("Arguments 'Y', 'Z', and 'ProvID' are all required!", call.=FALSE)
#
#   if (!is.logical(backtrack)) stop("Argument 'backtrack' NOT as required!", call.=F)
#
#   #check dimensions of the input data
#   if (length(Y) != length(ProvID) | length(ProvID) != nrow(Z)){
#     stop("Dimensions of the input data do not match!!", call.=F)
#   }
#
#   if (check == TRUE)
#     data_check(Y, Z, ProvID)
#
#   # Data Preparation
#   data <- as.data.frame(cbind(Y, ProvID, Z))
#   Y.char <- colnames(data)[1]
#   ProvID.char <- colnames(data)[2]
#   Z.char <- colnames(Z)
#
#   data <- data[order(factor(data[,ProvID.char])),] # sort data by provider ID
#   prov.size <- as.integer(table(data[,ProvID.char])) # provider sizes
#   prov.size.long <- rep(prov.size,prov.size) # provider sizes assigned to patients
#   data$included <- 1 * (prov.size.long > cutoff) # create variable 'included' as an indicator
#   if (message == TRUE) warning(sum(prov.size<=cutoff)," out of ",length(prov.size),
#                                " providers considered small and filtered out!",immediate.=T,call.=F)
#
#   prov.list <- unique(data[data$included==1,ProvID.char])   # a reduced list of provider IDs
#   prov.no.events <-      # providers with no events
#     prov.list[sapply(split(data[data$included==1,Y.char], factor(data[data$included==1,ProvID.char])),sum)==0]
#   data$no.events <- 0
#   data$no.events[data[,ProvID.char]%in%c(prov.no.events)] <- 1
#   if (message == TRUE) message(paste(length(prov.no.events),"out of",length(prov.list),
#                                      "remaining providers with no events."))
#   prov.all.events <-     # providers with all events
#     prov.list[sapply(split(1-data[data$included==1,Y.char],factor(data[data$included==1,ProvID.char])),sum)==0]
#   data$all.events <- 0
#   data$all.events[data[,ProvID.char]%in%c(prov.all.events)] <- 1
#   if (message == TRUE) message(paste(length(prov.all.events),"out of",length(prov.list),
#                                      "remaining providers with all events."))
#   if (message == TRUE) message(paste0("After screening, ", round(sum(data[data$included==1,Y.char])/length(data[data$included==1,Y.char])*100,2),
#                                       "% of all records exhibit occurrences of events (Y = 1)"))
#
#
#   # for the remaining parts, only use the data with "included==1" ("cutoff" of provider size)
#   data <- data[data$included==1,]
#   n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length) # provider-specific number of discharges
#   n.events.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), sum) # provider-specific number of events
#   Z <- as.matrix(data[,Z.char])
#   gamma.prov <- rep(log(mean(data[,Y.char])/(1-mean(data[,Y.char]))), length(n.prov))
#   beta <- rep(0, NCOL(Z))
#
#   Loglkd <- function(gamma.obs, beta) {
#     sum((gamma.obs+Z%*%beta)*data[,Y.char]-log(1+exp(gamma.obs+Z%*%beta)))
#   }
#   loglkd_init = Loglkd(rep(gamma.prov, n.prov), beta)
#
#   # Model
#   if (algorithm == "SerBIN") {
#     if (Rcpp) { #Rcpp always use "backtrack"
#       ls <- logis_BIN_fe_prov(as.matrix(data[,Y.char]),Z,n.prov,gamma.prov,beta,
#                               0,1,tol,max.iter, bound, message, backtrack, stop)
#       gamma.prov <- as.numeric(ls$gamma)
#       beta <- as.numeric(ls$beta)
#     } else {
#       iter <- 0
#       crit <- 100 # initialize stop criterion
#       if (message){
#         message("Implementing SerBIN algorithm for fixed provider effects model ...")
#       }
#
#       if (backtrack){ # initialize parameters for backtracking line search
#         s <- 0.01
#         t <- 0.6
#       }
#
#       while (iter<=max.iter & crit>=tol) {
#         iter <- iter + 1
#         gamma.obs <- rep(gamma.prov, n.prov)
#         loglkd = Loglkd(gamma.obs, beta)
#         p <- c(plogis(gamma.obs+Z%*%beta))
#         pq <- p*(1-p)
#         pq[pq == 0] <- 1e-20
#         score.gamma <- sapply(split(data[,Y.char]-p, data[,ProvID.char]), sum)
#         score.beta <- t(Z)%*%(data[,Y.char]-p)
#         info.gamma.inv <- 1/sapply(split(pq, data[,ProvID.char]),sum) #I_11^(-1)
#         info.betagamma <- sapply(by(pq*Z,data[,ProvID.char],identity),colSums) #I_21
#         info.beta <- t(Z)%*%(pq*Z) #I_22
#         mat.tmp1 <- info.gamma.inv*t(info.betagamma) #J_1^T
#         schur.inv <- solve(info.beta-info.betagamma%*%mat.tmp1) #S^-1
#         mat.tmp2 <- mat.tmp1%*%schur.inv #J_2^T
#
#         d.gamma.prov <- info.gamma.inv*score.gamma +
#           mat.tmp2%*%(t(mat.tmp1)%*%score.gamma-score.beta)
#         d.beta <- -t(mat.tmp2)%*%score.gamma+schur.inv%*%score.beta
#         v <- 1 # initialize step size
#         if (backtrack) {
#           d.loglkd <- Loglkd(rep(gamma.prov+v*d.gamma.prov, n.prov), beta+v*d.beta) - loglkd
#           lambda <- c(score.gamma,score.beta)%*%c(d.gamma.prov,d.beta)
#           while (d.loglkd < s*v*lambda) {  #update step size
#             v <- t * v
#             d.loglkd <- Loglkd(rep(gamma.prov+v*d.gamma.prov, n.prov), beta+v*d.beta) - loglkd
#           }
#         }
#         gamma.prov <- gamma.prov + v * d.gamma.prov
#         gamma.prov <- pmin(pmax(gamma.prov, median(gamma.prov)-bound), median(gamma.prov)+bound)
#         beta.new <- beta + v * d.beta
#
#         d.loglkd = Loglkd(rep(gamma.prov, n.prov), beta.new) - loglkd
#
#         # stopping criterion
#         if (stop=="beta"){
#           crit <- norm(matrix(beta-beta.new),"I")
#           if (message){
#             cat(paste0("Iter ",iter,": Inf norm of running diff in est reg parm is ",
#                        formatC(crit,digits=3,format="e"),";\n"))
#           }
#         }
#         else if (stop=="relch"){
#           crit <- abs(d.loglkd/(d.loglkd+loglkd))
#           if (message) {
#             cat(paste0("Iter ",iter,": Relative change in est log likelihood is ",
#                        formatC(crit,digits=3,format="e"),";\n"))
#           }
#         }
#         else if (stop=="ratch"){
#           crit <- abs(d.loglkd/(d.loglkd+loglkd-loglkd_init))
#           if (message) {
#             cat(paste0("Iter ",iter,": Adjusted relative change in est log likelihood is ",
#                        formatC(crit,digits=3,format="e"),";\n"))
#           }
#         }
#         else if (stop=="all"){
#           crit_beta <- norm(matrix(beta-beta.new),"I")
#           crit_relch <- abs(d.loglkd/(d.loglkd+loglkd))
#           crit_ratch <- abs(d.loglkd/(d.loglkd+loglkd-loglkd_init))
#           crit <- max(crit_beta, crit_relch, crit_ratch)
#           if (message) {
#             cat(sprintf("Iter %d: Maximum criterion across all checks is %.3e;\n", iter, crit))
#           }
#         }
#         else if (stop=="or"){
#           crit_beta <- norm(matrix(beta-beta.new),"I")
#           crit_relch <- abs(d.loglkd/(d.loglkd+loglkd))
#           crit_ratch <- abs(d.loglkd/(d.loglkd+loglkd-loglkd_init))
#           crit <- min(crit_beta, crit_relch, crit_ratch)
#           if (message) {
#             cat(sprintf("Iter %d: Minimum criterion across all checks is %.3e;\n", iter, crit))
#           }
#         }
#
#         beta <- beta.new
#
#       }
#       if (message){
#         message("\n SerBIN algorithm converged after ",iter," iterations!")
#       }
#     }
#   } else if (algorithm == "BAN"){
#     if (Rcpp) {
#       ls <- logis_fe_prov(as.matrix(data[,Y.char]),Z,n.prov,gamma.prov,beta,backtrack,max.iter,bound,tol,message,stop)
#       gamma.prov <- as.numeric(ls$gamma); beta <- as.numeric(ls$beta)
#     } else {
#       iter <- 0
#       crit <- 100 # initialize stop criterion
#       if (message){
#         message("Implementing BAN algorithm for fixed provider effects model ...")
#       }
#       if (backtrack){ # initialize parameters for backtracking line search
#         s <- 0.01
#         t <- 0.8
#       }
#
#       while (iter<=max.iter & crit>=tol) {
#         iter <- iter + 1
#         # provider effect update
#         gamma.obs <- rep(gamma.prov, n.prov)
#         loglkd.old = Loglkd(gamma.obs, beta)
#         Z.beta <- Z%*%beta
#         p <- c(plogis(gamma.obs+Z.beta)); pq <- p*(1-p)
#         pq[pq == 0] <- 1e-20
#         score.gamma.prov <- sapply(split(data[,Y.char]-p, data[,ProvID.char]), sum)
#         d.gamma.prov <- score.gamma.prov / sapply(split(pq, data[,ProvID.char]), sum)
#         v <- 1 # initialize step size
#         if (backtrack) {
#           loglkd <- Loglkd(rep(gamma.prov, n.prov), beta)
#           d.loglkd <- Loglkd(rep(gamma.prov+v*d.gamma.prov, n.prov), beta) - loglkd
#           lambda <- score.gamma.prov%*%d.gamma.prov
#           while (d.loglkd < s*v*lambda) {
#             v <- t * v
#             d.loglkd <- Loglkd(rep(gamma.prov+v*d.gamma.prov, n.prov), beta) - loglkd
#           }
#         }
#         gamma.prov <- gamma.prov + v * d.gamma.prov
#         gamma.prov <- pmin(pmax(gamma.prov, median(gamma.prov)-bound), median(gamma.prov)+bound)
#         gamma.obs <- rep(gamma.prov, n.prov)
#
#         # regression parameter update
#         p <- c(plogis(gamma.obs+Z.beta)); pq <- p*(1-p)
#         score.beta <- t(Z)%*%(data[,Y.char]-p)
#         info.beta <- t(Z)%*%(c(pq)*Z)
#         d.beta <- as.numeric(solve(info.beta)%*%score.beta)
#         v <- 1 # initialize step size
#         if (backtrack) {
#           loglkd <- Loglkd(gamma.obs, beta)
#           d.loglkd <- Loglkd(gamma.obs, beta+v*d.beta) - loglkd
#           lambda <- c(score.beta)%*%d.beta
#           while (d.loglkd < s*v*lambda) {
#             v <- t * v
#             d.loglkd <- Loglkd(gamma.obs, beta+v*d.beta) - loglkd
#           }
#         }
#         beta.new <- beta + v * d.beta
#         d.loglkd = Loglkd(rep(gamma.prov, n.prov), beta.new) - loglkd.old
#
#         # stopping criterion
#         if (stop=="beta"){
#           crit <- norm(matrix(beta-beta.new),"I")
#           if (message){
#             cat(paste0("Iter ",iter,": Inf norm of running diff in est reg parm is ",
#                        formatC(crit,digits=3,format="e"),";\n"))
#           }
#         }
#         else if (stop=="relch"){
#           crit <- abs(d.loglkd/(d.loglkd+loglkd.old))
#           if (message) {
#             cat(paste0("Iter ",iter,": Relative change in est log likelihood is ",
#                        formatC(crit,digits=3,format="e"),";\n"))
#           }
#         }
#         else if (stop=="ratch"){
#           crit <- abs(d.loglkd/(d.loglkd+loglkd.old-loglkd_init))
#           if (message) {
#             cat(paste0("Iter ",iter,": Adjusted relative change in est log likelihood is ",
#                        formatC(crit,digits=3,format="e"),";\n"))
#           }
#         }
#         else if (stop=="all"){
#           crit_beta <- norm(matrix(beta-beta.new),"I")
#           crit_relch <- abs(d.loglkd/(d.loglkd+loglkd.old))
#           crit_ratch <- abs(d.loglkd/(d.loglkd+loglkd.old-loglkd_init))
#           crit <- max(crit_beta, crit_relch, crit_ratch)
#           if (message) {
#             cat(sprintf("Iter %d: Maximum criterion across all checks is %.3e;\n", iter, crit))
#           }
#         }
#         else if (stop=="or"){
#           crit_beta <- norm(matrix(beta-beta.new),"I")
#           crit_relch <- abs(d.loglkd/(d.loglkd+loglkd.old))
#           crit_ratch <- abs(d.loglkd/(d.loglkd+loglkd.old-loglkd_init))
#           crit <- min(crit_beta, crit_relch, crit_ratch)
#           if (message) {
#             cat(sprintf("Iter %d: Minimum criterion across all checks is %.3e;\n", iter, crit))
#           }
#         }
#
#         beta <- beta.new
#       }
#       if (message){
#         message("\n BAN algorithm converged after ",iter," iterations!")
#       }
#     }
#   } else {
#     stop("Argument 'algorithm' NOT as required!")
#   }
#
#   # Coefficient
#   beta <- matrix(beta)
#   gamma.prov <- matrix(gamma.prov)
#   dimnames(beta) <- list(Z.char, "beta")
#   dimnames(gamma.prov) <- list(names(n.prov), "gamma")
#
#   # Variance
#   var_ls <- logis_fe_var(data[,Y.char], as.matrix(data[,Z.char]), n.prov, gamma.prov, beta)
#
#   gamma.obs <- rep(gamma.prov, n.prov)
#   neg2Loglkd <- -2*sum((gamma.obs+Z%*%beta)*data[,Y.char]-log(1+exp(gamma.obs+Z%*%beta)))
#   AIC <- neg2Loglkd + 2 * (length(gamma.prov)+length(beta))
#   BIC <- neg2Loglkd + log(nrow(data)) * (length(gamma.prov)+length(beta))
#
#   df.prov <- data.frame(Obs_provider = sapply(split(data[,Y.char],data[,ProvID.char]),sum),
#                         gamma_est = gamma.prov) #original gamma-hat, for internal using
#   linear_pred <- Z %*% beta
#   pred <- as.numeric(plogis(gamma.obs + linear_pred))
#
#
#
#   char_list <- list(Y.char = Y.char,
#                     ProvID.char = ProvID.char,
#                     Z.char = Z.char)
#
#   return_ls <- structure(list(beta = beta,
#                               gamma = gamma.prov, #provider effect
#                               linear_pred = linear_pred, #linear predictor
#                               pred = pred, #predicted probability
#                               neg2Loglkd = neg2Loglkd,
#                               AIC = AIC,
#                               BIC = BIC,
#                               obs = data[, Y.char], #patient-level obs
#                               prov = data[, ProvID.char],
#                               var = var_ls),
#                          class = "logis_fe")
#   if (AUC) {
#     AUC <- pROC::auc(data[,Y.char], pred)
#     return_ls$AUC <- AUC[1]
#   }
#   return_ls$df.prov <- df.prov
#   return_ls$char_list <- char_list
#   return_ls$data_include <- data
#   return(return_ls)
# }
#
