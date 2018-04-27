.onAttach <- function(...) {

  if (!interactive() || stats::runif(1) > 0.1) return()

  pkgs <- utils::available.packages()
  
  cran_version <- 
    pkgs %>%
    extract("olsrr", "Version") %>%
    package_version()

  local_version <- packageVersion("olsrr")
  behind_cran <- cran_version > local_version

  tips <- c(
    "Learn more about olsrr at http://github.com/rsquaredacademy/olsrr/.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages.",
    "Need help getting started with regression models? Visit: http://www.rsquaredacademy.com",
    "Check out our interactive app for quick data exploration. Visit: http://www.rsquaredlabs.com:3838/explorer/."
  )

  tip <- sample(tips, 1)

  if (behind_cran) {
    packageStartupMessage("A new version of olsrr (0.5.1) is available with bug fixes and new features.")
  } else {
    packageStartupMessage(paste(strwrap(tip), collapse = "\n"))
  }   

}
