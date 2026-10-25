#' @title Launch shiny app
#' @description Launches shiny app for interactive model building.
#' @examples
#' \dontrun{
#' ols_launch_app()
#' }
#' @export
#'
ols_launch_app <- function() {
  if (!xplorerr_available()) {
    stop("The `xplorerr` package is required to launch the app. Install it with `install.packages(\"xplorerr\")`.",
      call. = FALSE
    )
  }
  xplorerr::app_linear_regression()
}

xplorerr_available <- function() {
  requireNamespace("xplorerr", quietly = TRUE)
}
 