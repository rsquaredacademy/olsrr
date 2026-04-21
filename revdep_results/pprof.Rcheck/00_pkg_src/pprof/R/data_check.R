#' Data quality check function
#'
#' Conduct data quality check including checking missingness, variation, correlation and VIF of variables.
#'
#' @param Y a numeric vector indicating the outcome variable.
#' @param Z a matrix or data frame representing covariates.
#' @param ProvID a numeric vector representing the provider identifier.
#'
#' @details The function performs the following checks:
#'   \itemize{
#'     \item \strong{Missingness:} Checks for any missing values in the dataset and provides a summary of missing data.
#'     \item \strong{Variation:} Identifies covariates with zero or near-zero variance which might affect model stability.
#'     \item \strong{Correlation:} Analyzes pairwise correlation among covariates and highlights highly correlated pairs.
#'     \item \strong{VIF:} Computes the Variable Inflation Factors to identify covariates with potential multicollinearity issues.
#'   }
#' If issues arise when using the model functions \code{logis_fe}, \code{linear_fe} and \code{linear_re},
#' this function can be called for data quality checking purposes.
#'
#' @return No return value, called for side effects.
#'
#' @importFrom caret nearZeroVar
#' @importFrom olsrr ols_vif_tol
#' @importFrom stats complete.cases lm as.formula
#'
#' @examples
#' data(ExampleDataBinary)
#' outcome = ExampleDataBinary$Y
#' covar = ExampleDataBinary$Z
#' ProvID = ExampleDataBinary$ProvID
#' data_check(outcome, covar, ProvID)
#'
#' @export

data_check <- function(Y, Z, ProvID) {
  data <- as.data.frame(cbind(Y, ProvID, Z))
  Y.char <- colnames(data)[1]
  prov.char <- colnames(data)[2]
  Z.char <- colnames(Z)

  ## check missingness of variables
  message("Checking missingness of variables ... ")
  if (sum(complete.cases(data[,c(Y.char,Z.char,prov.char)]))==NROW(data)) {
    message("Missing values NOT found. Checking missingness of variables completed!")
  } else {
    check.na <- function(name) {
      if (sum(is.na(data[,name])) > 0) {
        warning(sum(is.na(data[,name]))," out of ",NROW(data[,name])," in '",name,"' missing!",immediate.=T,call.=F)
      }
    }
    invisible(sapply(c(Y.char,Z.char,prov.char), check.na))
    missingness <- (1 - sum(complete.cases(data[,c(Y.char,Z.char,prov.char)])) / NROW(data)) * 100
    stop(paste(round(missingness,2), "% of all observations are missing!",sep=""),call.=F)
  }

  ## check variation in covariates
  message("Checking variation in covariates ... ")
  nzv <- caret::nearZeroVar(data[,Z.char], saveMetrics=T)
  if (sum(nzv$zeroVar==T) > 0) {
    stop("Covariate(s) '", paste(row.names(nzv[nzv$zeroVar==T,]), collapse="', '"),
         "' with zero variance(s)!", call.=F)
  } else if (sum(nzv$nzv==T) > 0) {
    warning("Covariate(s) '",paste(row.names(nzv[nzv$nzv==T,]), collapse="', '"),
            "' with near zero variance(s)!",immediate.=T,call.=F)
  }
  message("Checking variation in covariates completed!")

  ## check correlation
  message("Checking pairwise correlation among covariates ... ")
  cor <- cor(data[,Z.char])
  threshold.cor <- 0.9
  if (sum(abs(cor[upper.tri(cor)])>threshold.cor) > 0) {
    cor[lower.tri(cor,diag=T)] <- 0
    ind <- which(abs(cor)>threshold.cor)
    pairs <- sapply(ind, function(ind) c(rownames(cor)[ind%%NROW(cor)],
                                         colnames(cor)[ind%/%NROW(cor)+1]))
    warning("The following ", NCOL(pairs),
            " pair(s) of covariates are highly correlated (correlation > ",
            threshold.cor,"): ", immediate.=T, call.=F)
    invisible(apply(pairs,2,function(col) message('("',paste(col, collapse='", "'),'")')))
  }
  message("Checking pairwise correlation among covariates completed!")

  ## check VIF
  message("Checking VIF of covariates ... ")
  m.lm <- lm(as.formula(paste(Y.char,"~",paste(Z.char, collapse="+"))), data=data)
  vif <- olsrr::ols_vif_tol(m.lm)
  if(sum(vif$VIF >= 10) > 0){
    warning("Covariate(s) '",
            paste(as.data.frame(vif)[vif$VIF>=10,"Variables"], collapse="', '"),
            "' with serious multicollinearity!",immediate.=T,call.=F)
  }
  message("Checking VIF of covariates completed!")
}

