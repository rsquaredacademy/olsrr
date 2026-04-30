#' Conduct hypothesis testing for provider effects from a fitted `logis_fe` object
#'
#' Conduct hypothesis tests on provider effects and identify outlying providers for a fixed effect logistic model.
#'
#' @param fit a model fitted from \code{logis_fe}.
#' @param parm specifies a subset of providers for which confidence intervals are to be given.
#' By default, all providers are included. The class of `parm` should match the class of the provider IDs.
#' @param level the confidence level during the hypothesis test, meaning a significance level of \eqn{1 - \text{level}}.
#' The default value is 0.95.
#' @param test a character string specifying the type of testing method to be conducted. The default is "exact.poisbinom".
#'   \itemize{
#'   \item{\code{"exact.poisbinom"}:} exact test based on Poisson-binomial distribution of \eqn{O_i|Z_i}.
#'   \item{\code{"exact.bootstrap"}:} exact test based on bootstrap procedure.
#'   \item{\code{"wald"}:} wald test.
#'   \item{\code{"score"}:} score test.
#'   }
#' @param score_modified a logical indicating whether to use the modified score test
#' ignoring the randomness of covariate coefficient for score teat (\code{"test = score"}). The default value is TRUE.
#' @param null a character string or a number specifying null hypotheses of fixed provider effects. The default is \code{"median"}.
#' @param n resample size for bootstrapping when (\code{"test = exact.bootstrap"}). The default value is 10,000.
#' @param threads an integer specifying the number of threads to use. The default value is 1.
#' @param alternative a character string specifying the alternative hypothesis, must be one of
#' \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}.
#' @param \dots additional arguments that can be passed to the function.
#'
#'
#' @details
#' By default, the function uses the `"exact.poisbinom"` method.
#' The wald test is invalid for extreme providers (i.e. when provider effect goes to infinity).
#' For the score test, consider that when the number of tested providers is large,
#' refitting the models to get the restricted MLEs will take a long time.
#' Therefore, we use unrestricted MLEs to replace the restricted MLEs during the testing procedure by default.
#' However, the user can specify \code{score_modified = FALSE} to perform a standard score test.
#'
#'
#' @return A data frame containing the results of the hypothesis test, with the following columns:
#' \item{flag}{a flagging indicator where \code{1} means statistically higher than expected
#' and \code{-1} means statistically lower than expected.}
#' \item{p-value}{the p-value of the hypothesis test.}
#' \item{stat}{the test statistic.}
#' \item{Std.Error}{The standard error of the provider effect estimate, included only when \code{test = "wald"}.}
#'
#'
#' @examples
#' data(ExampleDataBinary)
#' outcome = ExampleDataBinary$Y
#' covar = ExampleDataBinary$Z
#' ProvID = ExampleDataBinary$ProvID
#' fit_fe <- logis_fe(Y = outcome, Z = covar, ProvID = ProvID, message = FALSE)
#' test(fit_fe, test = "score")
#'
#' @importFrom stats plogis qnorm pnorm rbinom
#' @importFrom poibin ppoibin
#'
#' @references
#' Wu, W, Yang, Y, Kang, J, He, K. (2022) Improving large-scale estimation and inference for profiling health care providers.
#' \emph{Statistics in Medicine}, \strong{41(15)}: 2840-2853.
#' \cr
#'
#' @exportS3Method test logis_fe

test.logis_fe <- function(fit, parm, level = 0.95, test = "exact.poisbinom", score_modified = TRUE,
                          null = "median", n = 10000, threads = 1, alternative = "two.sided", ...) {
  if (missing(fit)) stop ("Argument 'fit is required!", call.=F)
  if (!class(fit) %in% c("logis_fe")) stop("Object fit is not of the classes 'logis_fe'!", call.=F)
  if (!(test %in% c("exact.poisbinom", "exact.bootstrap", "score", "wald", "robust_wald")))
    stop("Argument 'test' NOT as required!",call.=F)

  alpha <- 1 - level

  Y.char <- fit$char_list$Y.char
  Z.char <- fit$char_list$Z.char
  ProvID.char <- fit$char_list$ProvID.char
  data <- fit$data_include[, c(Y.char, Z.char, ProvID.char)] #already sorted by provider ID
  n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)

  # gamma <- fit$df.prov$gamma_est #not use the potential Inf of gamma here
  gamma <- fit$coefficient$gamma
  beta <- fit$coefficient$beta
  se.gamma <- sqrt(fit$variance$gamma)
  gamma.null <- ifelse(null=="median", median(gamma),
                       ifelse(is.numeric(null), null[1],
                              stop("Argument 'null' NOT as required!",call.=F)))


  if (!missing(parm)) {
    if (is.numeric(parm)) {  #avoid "integer" class
      parm <- as.numeric(parm)
    }
    if (class(parm) == class(data[, ProvID.char]) & !(test == "wald" | (test == "score" & !score_modified))) {
      n.prov <- n.prov[names(n.prov) %in% parm]
      data <- data[data[, ProvID.char] %in% parm, ]
    } else if (class(parm) == class(data[, ProvID.char]) & (test == "wald" | (test == "score" & !score_modified))) {
      indices <- which(unique(data[, ProvID.char]) %in% parm)
    } else {
      stop("Argument 'parm' includes invalid elements!")
    }
  }

  if (test=="exact.bootstrap") { #should consistent to exact.poisbinom method
    if (n <= 0 | as.integer(n) != n) stop("Argument 'n' NOT a positive integer ! ",call.=F)
    exact.bootstrap <- function(df, n, alternative) {
      probs <- plogis(gamma.null + unname(as.matrix(df[, Z.char])) %*% beta)
      probs <- pmin(pmax(probs,1e-10),1-1e-10)
      obs <- sum(df[,Y.char])
      sums <- colSums(matrix(rbinom(n=length(probs)*n, size=1, prob=rep(probs,times=n)), ncol=n))
      # p <- (sum(sums>obs)+ 0.5*sum(sums==obs))/n
      # z.score <- qnorm(p, lower=F)
      # flag <- ifelse(p<alpha/2, 1, ifelse(p<=1-alpha/2, 0, -1))
      # p.val <- 2 * min(p, 1-p)

      if (alternative == "two.sided") {
        p <- (sum(sums > obs) + 0.5 * sum(sums == obs)) / n
        z.score <- qnorm(p, lower.tail = F)
        p.val <- 2 * min(p, 1 - p)
        flag <- ifelse(p<alpha/2, 1, ifelse(p<=1-alpha/2, 0, -1))
      } else if (alternative == "greater") {
        p <- sum(sums >= obs) / n
        z.score <- qnorm(p, lower.tail = F)
        p.val <- p
        flag <- ifelse(p < alpha, 1, 0)
      } else if (alternative == "less") {
        p <- sum(sums <= obs) / n
        z.score <- qnorm(p, lower.tail = T)
        p.val <- p
        flag <- ifelse(p < alpha, -1, 0)
      } else {
        stop("Argument 'alternative' should be one of 'two.sided', 'greater', or 'less'")
      }

      return(c(flag, p.val, z.score))
    }
    results <- sapply(by(data, data[,ProvID.char], identity),
                      FUN=function(x) exact.bootstrap(x, n, alternative))

    return_df <- data.frame(flag = factor(results[1,]),
                            p = results[2,],
                            stat = results[3,],
                            row.names = unique(data[, ProvID.char]))
    colnames(return_df) <- c("flag", "p value", "stat")
    attr(return_df, "provider size") <- n.prov

    return(return_df)

  } else if (test == "score") {
    if (score_modified == FALSE) {  #standard score test
      n.prov <- sapply(split(data[, Y.char], data[, ProvID.char]), length)
      m <- length(n.prov)
      if (missing(parm)) {
        indices <- 1:m
      }
      z.score <- Modified_score(data[,Y.char], as.matrix(data[,Z.char]), n.prov, gamma, beta,
                                gamma.null, m, indices - 1, threads)  #In cpp, indices starts from 0

      if (alternative == "two.sided") {
        p <- pnorm(z.score, lower.tail=F)
        flag <- ifelse(p<alpha/2, 1, ifelse(p<=1-alpha/2, 0, -1))
        p.val <- 2 * pmin(p, 1-p)
      } else if (alternative == "greater") {
        p <- pnorm(z.score, lower.tail=F)
        p.val <- p
        flag <- ifelse(p < alpha, 1, 0)
      } else if (alternative == "less") {
        p <- pnorm(z.score, lower.tail = T)
        p.val <- p
        flag <- ifelse(p < alpha, -1, 0)
      } else {
        stop("Argument 'alternative' should be one of 'two.sided', 'greater', or 'less'")
      }

      return_df <- data.frame(flag=factor(flag),
                              p=p.val,
                              stat=z.score,
                              row.names = unique(data[, ProvID.char])[indices])
      colnames(return_df) <- c("flag", "p value", "stat")
      attr(return_df, "provider size") <- n.prov[indices]

      return(return_df)

    } else {  #modified score test
      probs <- plogis(gamma.null + unname(as.matrix(data[, Z.char])) %*% beta)
      probs <- pmin(pmax(probs,1e-10),1-1e-10)
      z.score <- sapply(split(data[,Y.char]-probs,data[,ProvID.char]),sum) /
        sqrt(sapply(split(probs*(1-probs),data[,ProvID.char]),sum))

      if (alternative == "two.sided") {
        p <- pnorm(z.score, lower.tail=F)
        flag <- ifelse(p<alpha/2, 1, ifelse(p<=1-alpha/2, 0, -1))
        p.val <- 2 * pmin(p, 1-p)
      } else if (alternative == "greater") {
        p <- pnorm(z.score, lower.tail=F)
        p.val <- p
        flag <- ifelse(p < alpha, 1, 0)
      } else if (alternative == "less") {
        p <- pnorm(z.score, lower.tail = T)
        p.val <- p
        flag <- ifelse(p < alpha, -1, 0)
      } else {
        stop("Argument 'alternative' should be one of 'two.sided', 'greater', or 'less'")
      }

      return_df <- data.frame(flag=factor(flag),
                              p=p.val,
                              stat=z.score,
                              row.names = unique(data[, ProvID.char]))
      colnames(return_df) <- c("flag", "p value", "stat")
      attr(return_df, "provider size") <- n.prov

      return(return_df)
    }
  } else if (test=="exact.poisbinom") {
    exact.poisbinom <- function(df, alternative) {
      probs <- plogis(gamma.null + unname(as.matrix(df[, Z.char])) %*% beta)
      probs <- pmin(pmax(probs,1e-10),1-1e-10)
      obs <- sum(df[,Y.char])
      # p <- 1 - poibin::ppoibin(obs, probs) + 0.5*poibin::dpoibin(obs, probs)  #"ppoibin": probability of "#event <= obs"
      # z.score <- qnorm(p, lower=F)
      # flag <- ifelse(p<alpha/2, 1, ifelse(p<=1-alpha/2, 0, -1))
      # p.val <- 2 * min(p, 1-p)

      if (alternative == "two.sided") {
        p <- 1 - poibin::ppoibin(obs, probs) + 0.5*poibin::dpoibin(obs, probs)  #"ppoibin": probability of "#event <= obs"
        z.score <- qnorm(p, lower.tail=F)
        flag <- ifelse(p<alpha/2, 1, ifelse(p<=1-alpha/2, 0, -1))
        p.val <- 2 * min(p, 1-p)
      } else if (alternative == "greater") {
        p <- 1 - poibin::ppoibin(obs - 1, probs)
        # p <- 1 - poibin::ppoibin(obs, probs) + poibin::dpoibin(obs, probs)
        z.score <- qnorm(p, lower.tail = F)
        p.val <- p
        flag <- ifelse(p < alpha, 1, 0)
      } else if (alternative == "less") {
        p <- poibin::ppoibin(obs, probs)
        z.score <- qnorm(p, lower.tail = T)
        p.val <- p
        flag <- ifelse(p < alpha, -1, 0)
      } else {
        stop("Argument 'alternative' should be one of 'two.sided', 'greater', or 'less'")
      }

      return(c(flag, p.val, z.score))
    }
    results <- sapply(by(data, data[,ProvID.char],identity),
                      FUN=function(x) exact.poisbinom(x, alternative))

    return_df <- data.frame(flag = factor(results[1,]),
                            p = results[2,],
                            stat = results[3,],
                            row.names = unique(data[, ProvID.char]))
    colnames(return_df) <- c("flag", "p value", "stat")
    attr(return_df, "provider size") <- n.prov

    return(return_df)
  } else if (test=="wald") { # invalid in presence of outlying providers
    warning("Wald test fails for datasets with providers having all or no events. Score test or exact test are recommended.")

    # gamma.obs <- rep(gamma, sapply(split(data[,Y.char],data[,ProvID.char]),length)) #find gamma-hat
    # probs <- as.numeric(plogis(gamma.obs+as.matrix(data[,Z.char])%*%beta))
    # info.gamma.inv <- 1/sapply(split(probs*(1-probs), data[,ProvID.char]),sum) #I_11^-1
    # info.betagamma <- sapply(by(probs*(1-probs)*as.matrix(data[,Z.char]),data[,ProvID.char],identity),colSums) #I_21
    # info.beta <- t(as.matrix(data[,Z.char]))%*%(probs*(1-probs)*as.matrix(data[,Z.char]))
    # schur.inv <- solve(info.beta-info.betagamma%*%(info.gamma.inv*t(info.betagamma))) # inv of Schur complement; S^-1
    # if (missing(parm)) {
    #   mat.tmp <- info.gamma.inv*t(info.betagamma) #J_1^T
    #   names <- unique(data[, ProvID.char])
    # } else {
    #   mat.tmp <- info.gamma.inv[indices]*t(info.betagamma[,indices])
    #   info.gamma.inv <- info.gamma.inv[indices]
    #   gamma <- gamma[indices]
    #   names <- unique(data[, ProvID.char])[indices]
    # }
    # se.gamma <- sqrt(info.gamma.inv+apply(mat.tmp, 1, FUN=function(x) t(matrix(x))%*%schur.inv%*%matrix(x))) #only diagonal elements of (1,1) block of I^-1(\theta)
    # stat <- (gamma-gamma.null)/se.gamma
    # p <- pnorm(stat, lower=F)
    # flag <- ifelse(p<alpha/2, 1, ifelse(p<=1-alpha/2, 0, -1))
    # p.val <- 2 * pmin(p, 1-p)

    stat <- (gamma - gamma.null)/se.gamma
    prob <- pnorm(stat, lower.tail = F)

    if (alternative == "two.sided") {
      flag <- ifelse(prob < alpha/2, 1, ifelse(prob <= 1-alpha/2, 0, -1))
      p_value <- 2 * pmin(prob, 1-prob)
    }
    else if (alternative == "greater") {
      flag <- ifelse(prob < alpha, 1, 0)
      p_value <- prob
    }
    else if (alternative == "less") {
      flag <- ifelse(1 - prob < alpha, -1, 0)
      p_value <- 1 - prob
    }
    else {
      stop("Argument 'alternative' should be one of 'two.sided', 'less', 'greater'")
    }

    result <- data.frame(flag = factor(flag), p = p_value, stat = stat, Std.Error = se.gamma)
    colnames(result) <- c("flag", "p value", "stat", "Std.Error")

    if (missing(parm)) {
      attr(result, "provider size") <- n.prov
      return(result)
    }
    else {
      attr(result, "provider size") <- n.prov[indices]
      return(result[indices, ])
    }
  } else if (test == "robust wald") {
    return(fit)
  }

}
