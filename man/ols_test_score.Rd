% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ols-score-test.R
\name{ols_test_score}
\alias{ols_test_score}
\title{Score test}
\usage{
ols_test_score(model, fitted_values = TRUE, rhs = FALSE, vars = NULL)
}
\arguments{
\item{model}{An object of class \code{lm}.}

\item{fitted_values}{Logical; if TRUE, use fitted values of regression model.}

\item{rhs}{Logical; if TRUE, specifies that tests for heteroskedasticity be
performed for the right-hand-side (explanatory) variables of the fitted
regression model.}

\item{vars}{Variables to be used for for heteroskedasticity test.}
}
\value{
\code{ols_test_score} returns an object of class \code{"ols_test_score"}.
An object of class \code{"ols_test_score"} is a list containing the
following components:

\item{score}{f statistic}
\item{p}{p value of \code{score}}
\item{df}{degrees of freedom}
\item{fv}{fitted values of the regression model}
\item{rhs}{names of explanatory variables of fitted regression model}
\item{resp}{response variable}
\item{preds}{predictors}
}
\description{
Test for heteroskedasticity under the assumption that the errors are
independent and identically distributed (i.i.d.).
}
\examples{
# model
model <- lm(mpg ~ disp + hp + wt, data = mtcars)

# using fitted values of the model
ols_test_score(model)

# using predictors from the model
ols_test_score(model, rhs = TRUE)

# specify predictors from the model
ols_test_score(model, vars = c('disp', 'wt'))

}
\references{
Breusch, T. S. and Pagan, A. R. (1979) A simple test for heteroscedasticity and random coefficient variation. Econometrica 47, 1287–1294.

Cook, R. D. and Weisberg, S. (1983) Diagnostics for heteroscedasticity in regression. Biometrika 70, 1–10.

Koenker, R. 1981. A note on studentizing a test for heteroskedasticity. Journal of Econometrics 17: 107–112.
}
\seealso{
Other heteroskedasticity tests: 
\code{\link{ols_test_bartlett}()},
\code{\link{ols_test_breusch_pagan}()},
\code{\link{ols_test_f}()}
}
\concept{heteroskedasticity tests}
