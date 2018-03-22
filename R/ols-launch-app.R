#' @importFrom shiny runApp
#' @title Launch shiny app
#' @description Launches shiny app for interactive model building.
#' @examples
#' \dontrun{
#' ols_launch_app()
#' }
#' @export
#'
ols_launch_app <- function() {
  runApp(appDir = system.file("application", package = "olsrr"))
}
