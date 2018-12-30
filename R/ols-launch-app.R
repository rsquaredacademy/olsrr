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
  rlang::abort("The shiny app has been moved to a new package, `xplorerr`. To launch the app, run the below code:\n 
	- install.packages('xplorerr')\n - xplorerr::app_linear_regression()")
}
 