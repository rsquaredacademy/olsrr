% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ols-breusch-pagan-test.R
\name{ols_test_breusch_pagan}
\alias{ols_test_breusch_pagan}
\title{Breusch pagan test}
\usage{
ols_test_breusch_pagan(
  model,
  fitted.values = TRUE,
  rhs = FALSE,
  multiple = FALSE,
  p.adj = c("none", "bonferroni", "sidak", "holm"),
  vars = NA
)
}
\arguments{
\item{model}{An object of class \code{lm}.}

\item{fitted.values}{Logical; if TRUE, use fitted values of regression model.}

\item{rhs}{Logical; if TRUE, specifies that tests for heteroskedasticity be
performed for the right-hand-side (explanatory) variables of the fitted
regression model.}

\item{multiple}{Logical; if TRUE, specifies that multiple testing be performed.}

\item{p.adj}{Adjustment for p value, the following options are available:
bonferroni, holm, sidak and none.}

\item{vars}{Variables to be used for heteroskedasticity test.}
}
\value{
\code{ols_test_breusch_pagan} returns an object of class \code{"ols_test_breusch_pagan"}.
An object of class \code{"ols_test_breusch_pagan"} is a list containing the
following components:

\item{bp}{breusch pagan statistic}
\item{p}{p-value of \code{bp}}
\item{fv}{fitted values of the regression model}
\item{rhs}{names of explanatory variables of fitted regression model}
\item{multiple}{logical value indicating if multiple tests should be performed}
\item{padj}{adjusted p values}
\item{vars}{variables to be used for heteroskedasticity test}
\item{resp}{response variable}
\item{preds}{predictors}
}
\description{
Test for constant variance. It assumes that the error terms are normally
distributed.
}
\details{
Breusch Pagan Test was introduced by Trevor Breusch and Adrian Pagan in 1979.
It is used to test for heteroskedasticity in a linear regression model.
It test whether variance of errors from a regression is dependent on the
values of a independent variable.

\itemize{
\item Null Hypothesis: Equal/constant variances
\item Alternative Hypothesis: Unequal/non-constant variances
}

Computation

\itemize{
\item Fit a regression model
\item Regress the squared residuals from the above model on the independent variables
\item Compute \eqn{nR^2}. It follows a chi square distribution with p -1 degrees of
freedom, where p is the number of independent variables, n is the sample size and
\eqn{R^2} is the coefficient of determination from the regression in step 2.
}
}
\examples{
# model
model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)

# use fitted values of the model
ols_test_breusch_pagan(model)

# use independent variables of the model
ols_test_breusch_pagan(model, rhs = TRUE)

# use independent variables of the model and perform multiple tests
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE)

# bonferroni p value adjustment
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'bonferroni')

# sidak p value adjustment
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'sidak')

# holm's p value adjustment
ols_test_breusch_pagan(model, rhs = TRUE, multiple = TRUE, p.adj = 'holm')

}
\references{
T.S. Breusch & A.R. Pagan (1979), A Simple Test for Heteroscedasticity and
Random Coefficient Variation. Econometrica 47, 1287–1294

Cook, R. D.; Weisberg, S. (1983). "Diagnostics for Heteroskedasticity in Regression". Biometrika. 70 (1): 1–10.
}
\seealso{
Other heteroskedasticity tests: 
\code{\link{ols_test_bartlett}()},
\code{\link{ols_test_f}()},
\code{\link{ols_test_score}()}
}
\concept{heteroskedasticity tests}
