.onAttach <- function(...) {
  if (!interactive() || stats::runif(1) > 0.1) return()

  tips <- c(
    "Learn more about olsrr at http://github.com/rsquaredacademy/olsrr/.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Need help getting started with regression models? Visit: http://www.rsquaredacademy.com",
    "Check out our interactive app for quick data exploration. Visit: http://www.rsquaredlabs.com:3838/explorer/."
  )

  tip <- sample(tips, 1)
  packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
}
