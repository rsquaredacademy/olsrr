#' All possible regression variable coefficients.
#'
#' Returns the coefficients for each variable from each model.
#'
#' @param object An object of class \code{lm}.
#' @param ... Other arguments.
#'
#' @return \code{check_betas} returns a \code{data.frame} containing:
#'
#' \item{x}{model}
#'
#' @examples
#' model <- lm(real_gdp~imp+exp+usdkzt+eurkzt, data = macroKZ)
#' check_betas(model)
#' @references Hebbali, Aravind. Published 2020-02-10. olsrr package
#' @import stats
#' @export
#'


check_betas <- function(object, ...) {

  if (!all(class(object) == "lm")) {
    stop("Please specify a OLS linear regression model.", call. = FALSE)
  }

  if (length(object$coefficients) < 3) {
    stop("Please specify a model with at least 2 predictors.", call. = FALSE)
  }

  betas  <- NULL
  rsq    <- NULL
  lpreds <- NULL

  metrics    <- allpos_helper(object)
  beta_names <- names(metrics$betas)
  mindex     <- seq_len(length(metrics$rsq))
  reps       <- metrics$lpreds + 1
  m_index    <- rep(mindex, reps)
  beta       <- metrics$betas

  model$call$formula<-as.formula(model)

  s<-splitFormula(object$call$formula, sep="+")
  s<-gsub("~",replacement="",x=s,ignore.case = TRUE)
  n<-length(s)
  if (n>=0)
    N<-function(n){
      if (n==1)
        return(1)
      else if (n>=2)
        return (2*N(n-1)+1)
    }

  cat(paste("Based on the chosen regression model", N(n),"models can be generated. See below:"), sep="\n")
  data.frame(model = m_index, predictor = beta_names, beta = beta)

}

#' All possible regression internal
#'
#' Internal function for all possible regression.
#'
#' @param model An object of class \code{lm}.
#' @importFrom utils combn
#' @noRd


allpos_helper <- function(model) {

  nam   <- coeff_names(model)
  n     <- length(nam)
  r     <- seq_len(n)
  combs <- list()

  for (i in seq_len(n)) {
    combs[[i]] <- combn(n, r[i])
  }

  predicts  <- nam
  lc        <- length(combs)
  varnames  <- model_colnames(model)
  len_preds <- length(predicts)
  gap       <- len_preds - 1
  data      <- mod_sel_data(model)
  space     <- coeff_length(predicts, gap)
  colas     <- unname(unlist(lapply(combs, ncol)))
  response  <- varnames[1]
  p         <- colas
  t         <- cumsum(colas)
  q         <- c(1, t[-lc] + 1)

  mcount    <- 0
  rsq       <- list()
  adjrsq    <- list()
  predrsq   <- list()
  cp        <- list()
  aic       <- list()
  sbic      <- list()
  sbc       <- list()
  msep      <- list()
  fpe       <- list()
  apc       <- list()
  hsp       <- list()
  preds     <- list()
  lpreds    <- c()
  betas     <- c()

  for (i in seq_len(lc)) {
    for (j in seq_len(colas[i])) {

      predictors <- nam[combs[[i]][, j]]
      lp         <- length(predictors)

      out <- ols_regress(paste(response, "~",
                               paste(predictors, collapse = " + ")),
                         data = data)

      mcount            <- mcount + 1
      lpreds[mcount]    <- lp
      rsq[[mcount]]     <- out$rsq
      adjrsq[[mcount]]  <- out$adjr
      predrsq[[mcount]] <- ols_pred_rsq(out$model)
      cp[[mcount]]      <- ols_mallows_cp(out$model, model)
      aic[[mcount]]     <- ols_aic(out$model)
      sbic[[mcount]]    <- ols_sbic(out$model, model)
      sbc[[mcount]]     <- ols_sbc(out$model)
      msep[[mcount]]    <- ols_msep(out$model)
      fpe[[mcount]]     <- ols_fpe(out$model)
      apc[[mcount]]     <- ols_apc(out$model)
      hsp[[mcount]]     <- ols_hsp(out$model)
      preds[[mcount]]   <- paste(predictors, collapse = " ")
      betas             <- append(betas, out$betas)
    }
  }

  result <- list(
    lpreds = lpreds, rsq = rsq, adjrsq = adjrsq,
    predrsq = predrsq, cp = cp, aic = aic, sbic = sbic,
    sbc = sbc, msep = msep, fpe = fpe, apc = apc, hsp = hsp,
    preds = preds, lc = lc, q = q, t = t, betas = betas
  )

  return(result)
}

#' Coefficient names
#'
#' Returns the names of the coefficients including interaction variables.
#'
#' @param model An object of class \code{lm}.
#' @noRd
#'

coeff_names <- function(model) {
  terms <- NULL
  colnames(attr(model$terms, which = "factor"))
}

#' Model data columns
#'
#' Returns the names of the columns in the data used in the model.
#'
#' @param model An object of class \cdoe{lm}.
#' @noRd
#'

model_colnames <- function(model) {
  names(model.frame(model))
}

#' Coefficients length
#'
#' Returns the length of the coefficient names.
#'
#' @param predicts Name of the predictors in the model.
#' @param gap A numeric vector.
#' @noRd
#'

coeff_length <- function(predicts, gap) {
  sum(nchar(predicts)) + gap
}

mod_sel_data <- function(model) {

  eval(model$call$data)

}

l <- function(x) {
  x <- as.character(x)
  k <- grep("\\$", x)
  if (length(k) == 1) {
    temp <- strsplit(x, "\\$")
    out <- temp[[1]][2]
  } else {
    out <- x
  }
  return(out)
}



