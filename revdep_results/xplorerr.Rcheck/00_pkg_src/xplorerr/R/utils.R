#' @importFrom utils packageVersion menu install.packages
check_suggests <- function(pkg) {

  pkg_flag <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)

  if (is.na(pkg_flag)) {

    msg <- message(paste0(pkg, ' must be installed for this functionality.'))

    if (interactive()) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::install.packages(pkg)
      } else {
        stop(paste0(pkg, ' must be installed for this functionality.'), call. = FALSE)
      }
    } else {
      stop(paste0(pkg, ' must be installed for this functionality.'), call. = FALSE)
    }
  }

}


