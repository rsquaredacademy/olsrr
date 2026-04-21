#' @title VIF by variable
#' @description
#' Calculates the variation inflation factors of all predictors in regression models
#' @param model is a linear regression model
#' @examples
#' data(macroKZ)
#' model <- lm(real_gdp ~ imp + exp + poil + eurkzt + tonia_rate, data = macroKZ)
#' vif_reg(model)
#' @importFrom cli console_width
#' @references Petrie, Adam. Published 2020-02-21. regclass package
#' @rdname vif_reg
#' @export

vif_reg<-function (model)
{
  if (any(is.na(coef(model))))
    stop("there are aliased coefficients in the model")
  v <- vcov(model)
  assign <- attr(model.matrix(model), "assign")
  if (names(coefficients(model)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  }
  else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(model))
  n.terms <- length(terms)
  if (n.terms < 2)
    stop("model contains fewer than 2 terms")
  R <- cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n.terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in 1:n.terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs,
                                                                       -subs]))/detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1))
    result <- result[, 1]
  else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  l<-matrix(result, dimnames=list(terms))

  #print
  w3 <- console_width()
  cat(format(as.character("Variance Inflation Factor"), width=w3, justify="centre"), "\n\n")
  cat(paste("If statistics exceeds 5, please be aware of multicollinearity."), sep='\n')

  cat("\n")
  print(round(result,3))
  cat("\n")

  l<-matrix(result, dimnames=list(terms))
  for (i in 1:length(l)){
    if (l[i]>5)
      cat(paste("This value", round(l[i], 3), "exceeds acceptable threshold."), sep='\n')
  }
}



