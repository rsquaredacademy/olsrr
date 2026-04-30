#' Result Summaries of Covariate Estimates from a fitted `logis_fe` object
#'
#' Provide the summary statistics for the covariate estimates for a fixed effect logistic model.
#'
#' @param object a model fitted from \code{logis_fe}.
#' @param parm Specifies a subset of covariates for which the result summaries should be output.
#' By default, all covariates are included.
#' @param level the confidence level during the hypothesis test, meaning a significance level of \eqn{1 - \text{level}}.
#' The default value is 0.95.
#' @param test a character string specifying the type of testing method. The default is "wald".
#'   \itemize{
#'     \item{\code{"wald"}:} wald test.
#'     \item{\code{"lr"}:} likelihood ratio test.
#'     \item{\code{"score"}:} score test.
#'   }
#' @param null a number defining the null hypothesis for the covariate estimates. The default value is \code{0}.
#' @param \dots additional arguments that can be passed to the function.
#'
#' @return A data frame containing summary statistics for covariate estimates, with the following columns:
#' \item{Estimate}{the estimates of covariate coefficients.}
#' \item{Std.Error}{the standard error of the estimate, included only when \code{test = "wald"}.}
#' \item{Stat}{the test statistic.}
#' \item{p value}{the p-value for the hypothesis test.}
#' \item{CI.upper}{the lower bound of the confidence interval, included only when \code{test = "wald"}.}
#' \item{CI.lower}{the upper bound of the confidence interval, included only when \code{test = "wald"}.}
#'
#' @examples
#' data(ExampleDataBinary)
#' outcome = ExampleDataBinary$Y
#' covar = ExampleDataBinary$Z
#' ProvID = ExampleDataBinary$ProvID
#'
#' fit_fe <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID, message = FALSE)
#' summary.wald <- summary(fit_fe, level = 0.95, test = "wald")
#' summary.wald
#'
#' @importFrom Rcpp evalCpp
#' @importFrom stats plogis pnorm qnorm pchisq median
#'
#' @exportS3Method summary logis_fe

summary.logis_fe <- function(object, parm, level = 0.95, test = "wald", null = 0, ...) {
  if (missing(object)) stop ("Argument 'object' is required!",call.=F)
  if (!class(object) %in% c("logis_fe")) stop("Object `object` is not of the classes 'logis_fe'!",call.=F)
  if (!(test %in% c("wald", "lr", "score"))) stop("Argument 'test' NOT as required!",call.=F)

  Y.char <- object$char_list$Y.char
  Z.char <- object$char_list$Z.char
  ProvID.char <- object$char_list$ProvID.char
  beta <- object$coefficient$beta
  gamma <- object$coefficient$gamma
  alpha <- 1 - level

  if (missing(parm)) {
    ind <- 1:length(Z.char)
  } else if (is.character(parm)) {
    ind <- which(Z.char %in% parm)
  } else if (is.numeric(parm) & max(abs(as.integer(parm)-parm))==0 & !(0 %in% parm)) {
    ind <- parm
  } else {
    stop("Argument 'parm' includes invalid elements!")
  }

  if (test=="wald") { # Wald test and test-based CIs
    se.beta <- sqrt(diag(object$variance$beta))
    stat <- (beta - null) / se.beta

    p_value <-  2 * (1 - pnorm(abs(stat)))
    crit_value <- qnorm(1 - alpha / 2)
    lower_bound <- beta - crit_value * se.beta
    upper_bound <- beta + crit_value * se.beta

    # if (alternative == "two.sided") {
    #   p_value <-  2 * (1 - pnorm(abs(stat)))
    #   crit_value <- qnorm(1 - alpha / 2)
    #   lower_bound <- beta - crit_value * se.beta
    #   upper_bound <- beta + crit_value * se.beta
    # }
    # else if (alternative == "greater") {
    #   p_value <- 1 - pnorm(stat)
    #   crit_value <- qnorm(1-alpha)
    #   lower_bound <- beta - crit_value * se.beta
    #   upper_bound <- Inf
    # }
    # else if (alternative == "less") {
    #   p_value <- pnorm(stat)
    #   crit_value <- qnorm(1-alpha)
    #   lower_bound <- -Inf
    #   upper_bound <- beta + crit_value * se.beta
    # }
    # else {
    #   stop("Argument 'alternative' should be one of 'two.sided', 'less', 'greater'.")
    # }

    p_value <- format.pval(p_value, digits = 7, eps = 1e-10)

    result <- data.frame(beta = beta, se.beta = se.beta, stat = stat, p_value = p_value,
                         lower_bound = lower_bound, upper_bound = upper_bound)
    colnames(result) <- c("Estimate", "Std.Error", "Stat", "p value", "CI.Lower", "CI.Upper")

    return(result[ind, ])

    # data <- object$data_include[, c(Y.char,Z.char,ProvID.char)]
    # gamma.obs <- rep(object$df.prov$gamma_est, sapply(split(data[,Y.char],data[,ProvID.char]),length))
    # probs <- as.numeric(plogis(gamma.obs+as.matrix(data[,Z.char]) %*% object$coefficient$beta))
    # probs <- pmin(pmax(probs,1e-10),1-1e-10)
    # info.gamma.inv <- 1/sapply(split(probs*(1-probs), data[,ProvID.char]),sum)
    # info.betagamma <- sapply(by(probs*(1-probs)*as.matrix(data[,Z.char]),data[,ProvID.char],identity),colSums)
    # info.beta <- t(as.matrix(data[,Z.char]))%*%(probs*(1-probs)*as.matrix(data[,Z.char]))
    # se.beta <- sqrt(diag(solve(info.beta-info.betagamma%*%(info.gamma.inv*t(info.betagamma)))))[ind] #S^-1
    # p <- pnorm((object$beta[ind]-null)/se.beta, lower=F)
    # df <- data.frame(beta = object$beta[ind],
    #                  se.beta = se.beta,
    #                  p=2*pmin(p,1-p),
    #                  CI.lower=object$beta[ind]-qnorm(1-alpha/2)*se.beta,
    #                  CI.upper=object$beta[ind]+qnorm(1-alpha/2)*se.beta)
    # row.names(df) <- Z.char[ind]
    #return(df)
  }
  # else if (test=="wald.cpp") {  #use cpp function to calculate Cov(\beta)
  #   data <- object$data_include[, c(Y.char,Z.char,ProvID.char)]
  #   n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)
  #   ls <- wald_covar(data[,Y.char], as.matrix(data[,Z.char]), n.prov, object$df.prov$gamma_est, as.numeric(object$beta), ind, null, alpha)
  #   #ls$p <- c(ls$p); ls$stat <- c(ls$stat); ls$se.beta <- c(ls$se.beta)
  #   #ls$beta.lower <- c(ls$beta.lower); ls$beta.upper <- c(ls$beta.upper)
  #   df <- data.frame(beta = object$beta[ind],
  #                    se.beta=c(ls$se.beta),
  #                    p=c(ls$p),
  #                    #stat=ls$stat,
  #                    CI.lower=c(ls$beta.lower),
  #                    CI.upper=c(ls$beta.upper))
  #   row.names(df) <- Z.char[ind]
  #   return(df)
  # }
  else if (test=="lr") { # Note: only null=0 allowed for now and no CIs
    if (null!=0) stop("Argument 'null' is invalid!")
    data <- object$data_include
    gamma.obs <- rep(pmax(pmin(gamma,median(gamma)+10),median(gamma)-10), sapply(split(data[,Y.char],data[,ProvID.char]),length))
    neg2Loglkd <- -2*sum((gamma.obs+as.matrix(data[,Z.char])%*%beta)*data[,Y.char]-log(1+exp(gamma.obs+as.matrix(data[,Z.char])%*%beta)))
    lr <- function(index) {
      data.null <- as.data.frame(cbind(data[,Y.char], data[,ProvID.char], data[,Z.char[-index]], data[, (ncol(data) - 2):ncol(data)]))
      char_list.null <- object$char_list
      char_list.null$Z.char <- char_list.null$Z.char[-index]
      colnames(data.null)[1:2] <- c(char_list.null$Y.char, char_list.null$ProvID.char)
      data.prep.null <- list(data = data.null,
                             char_list = char_list.null)
      fe.null <- logis_fe(data = data.null, Y.char = char_list.null$Y.char, Z.char = char_list.null$Z.char, ProvID.char = char_list.null$ProvID.char, message = F)
      Z.null <- as.matrix(data[,Z.char[-index]])
      gamma.obs.null <- rep(pmax(pmin(fe.null$coefficient$gamma,median(fe.null$coefficient$gamma)+10),median(fe.null$coefficient$gamma)-10), sapply(split(data[,Y.char],data[,ProvID.char]),length))
      neg2Loglkd.null <- -2*sum((gamma.obs.null+Z.null%*%fe.null$coefficient$beta)*data[,Y.char]-log(1+exp(gamma.obs.null+Z.null%*%fe.null$coefficient$beta)))
      #p <- pchisq(neg2Loglkd.null-neg2Loglkd, 1, lower.tail=F)
      test_stat <- neg2Loglkd.null - neg2Loglkd
      p <- pchisq(test_stat, 1, lower.tail = FALSE)
      return(list(test_stat = test_stat, p = p))
    }
    # df <- data.frame(
    #   beta = object$beta[ind],
    #   p = sapply(ind, function(i) lr(i)$p)
    # )
    df <- data.frame(
      beta = object$coefficient$beta[ind],
      test_stat = sapply(ind, function(i) lr(i)$test_stat),
      p = sapply(ind, function(i) lr(i)$p)
    )
    rownames(df) <- Z.char[ind]
    colnames(df) <- c('Estimate', 'stat', 'p value')
    return(df)
  } else if (test=="score") { # Note: only null=0 allowed for now and no CIs
    if (null!=0) stop("Argument 'null' is invalid!")
    data <- object$data_include
    score <- function(index) {
      data.null <- as.data.frame(cbind(data[,Y.char], data[,ProvID.char], data[,Z.char[-index]], data[, (ncol(data) - 2):ncol(data)]))
      char_list.null <- object$char_list
      char_list.null$Z.char <- char_list.null$Z.char[-index]
      colnames(data.null)[1:2] <- c(char_list.null$Y.char, char_list.null$ProvID.char)
      data.prep.null <- list(data = data.null,
                             char_list = char_list.null)
      fe.null <- logis_fe(data = data.null, Y.char = char_list.null$Y.char, Z.char = char_list.null$Z.char, ProvID.char = char_list.null$ProvID.char, message = F)
      Z.null <- as.matrix(data[,Z.char[-index]])
      gamma.obs.null <- rep(fe.null$coefficient$gamma, sapply(split(data[,Y.char],data[,ProvID.char]),length))
      probs.null <- as.numeric(plogis(gamma.obs.null+as.matrix(data[,Z.char[-index]])%*%fe.null$coefficient$beta))
      probs.null <- pmin(pmax(probs.null,1e-10),1-1e-10)
      info.gamma.inv.null <- 1/sapply(split(probs.null*(1-probs.null), data[,ProvID.char]),sum)
      info.betagamma.null <- sapply(by(probs.null*(1-probs.null)*Z.null,data[,ProvID.char],identity),colSums)
      info.beta.null <- t(Z.null)%*%(probs.null*(1-probs.null)*Z.null)
      mat.tmp <- info.gamma.inv.null*t(info.betagamma.null)
      schur.inv.null <- solve(info.beta.null-info.betagamma.null%*%mat.tmp)
      info.inv.11 <- mat.tmp%*%schur.inv.null%*%t(mat.tmp)
      diag(info.inv.11) <- diag(info.inv.11) + info.gamma.inv.null
      info.inv.21 <- -schur.inv.null%*%t(mat.tmp)
      info.beta.add.gamma <- sapply(by(probs.null*(1-probs.null)*data[,Z.char[index]],data[,ProvID.char],identity),sum)
      info.beta.add.beta <- t(as.matrix(data[,Z.char[index]]))%*%(probs.null*(1-probs.null)*Z.null)
      info.beta.add <- t(data[,Z.char[index]])%*%(probs.null*(1-probs.null)*data[,Z.char[index]]) -
        t(info.beta.add.gamma)%*%info.inv.11%*%info.beta.add.gamma -
        2*info.beta.add.beta%*%info.inv.21%*%info.beta.add.gamma -
        info.beta.add.beta%*%schur.inv.null%*%t(info.beta.add.beta)
      score.beta.add <- t(as.matrix(data[,Z.char[index]]))%*%(data[,Y.char]-probs.null)
      score.stat <- score.beta.add^2/info.beta.add
      p <- pchisq(score.stat, 1, lower.tail=F)
      return(list(test_stat = score.stat, p = p))
    }
    df <- data.frame(beta = object$coefficient$beta[ind],
                     test_stat = sapply(ind, function(i) score(i)$test_stat),
                     p = sapply(ind, function(i) score(i)$p))
    rownames(df) <- Z.char[ind]
    colnames(df) <- c('Estimate', 'stat', 'p value')
    return(df)
  }
}
