#' @title Launch shiny app
#' @description Launches shiny app for interactive model building.
#' @examples
#' \dontrun{
#' ols_launch_app()
#' }
#' @export
#'
ols_launch_app <- function() {

	rlang::inform("`ols_launch_app()` has been soft-deprecated and will be removed in the next release. In future, to launch the app, run the below code:\n 
	- install.packages('xplorerr')\n - xplorerr::app_linear_regression()\n")

	check_suggests('caret')
	check_suggests('descriptr')
	check_suggests('jsonlite')
	check_suggests('haven')
	check_suggests('lubridate')
	check_suggests('readr')
	check_suggests('readxl')
	check_suggests('scales')
	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')

	xplorerr::app_linear_regression()
}
 