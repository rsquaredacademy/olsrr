.onAttach <- function(libname, pkgname) {

  mylib <- dirname(system.file(package = "AFR"))
  ver <- utils::packageDescription("AFR")["Version"]
  txt <- c("\n",
           paste(sQuote("AFR"), "version:", ver),
           "\n",
           paste(sQuote("AFR"),
                 "is a package for banking sector analysis",
                 "and easier interpretation of statistical functions."),
           "\n",
           paste("See",
                 sQuote("library(help=\"AFR\")"),
                 "for details."))

  if(interactive() || getOption("verbose"))
  packageStartupMessage(paste(strwrap(txt,
                                        indent = 4,
                                        exdent = 4),
                                collapse = "\n"))

}

