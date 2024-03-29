% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ols-normality-tests.R
\name{ols_test_normality}
\alias{ols_test_normality}
\alias{ols_test_normality.lm}
\title{Test for normality}
\usage{
ols_test_normality(y, ...)

\method{ols_test_normality}{lm}(y, ...)
}
\arguments{
\item{y}{A numeric vector or an object of class \code{lm}.}

\item{...}{Other arguments.}
}
\value{
\code{ols_test_normality} returns an object of class \code{"ols_test_normality"}.
An object of class \code{"ols_test_normality"} is a list containing the
following components:

\item{kolmogorv}{kolmogorv smirnov statistic}
\item{shapiro}{shapiro wilk statistic}
\item{cramer}{cramer von mises statistic}
\item{anderson}{anderson darling statistic}
}
\description{
Test for detecting violation of normality assumption.
}
\examples{
model <- lm(mpg ~ disp + hp + wt + qsec, data = mtcars)
ols_test_normality(model)

}
\seealso{
Other residual diagnostics: 
\code{\link{ols_plot_resid_box}()},
\code{\link{ols_plot_resid_fit}()},
\code{\link{ols_plot_resid_hist}()},
\code{\link{ols_plot_resid_qq}()},
\code{\link{ols_test_correlation}()}
}
\concept{residual diagnostics}
