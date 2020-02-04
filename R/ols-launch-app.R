#' @title Launch shiny app
#' @description Launches shiny app for interactive model building.
#' @examples
#' \dontrun{
#' ols_launch_app()
#' }
#' @export
#'
ols_launch_app <- function() {

	message("`ols_launch_app()` has been soft-deprecated and will be removed in the next release. In future, to launch the app, run the below code:\n\n - install.packages('xplorerr')\n - xplorerr::app_linear_regression()\n")

	check_suggests('xplorerr')
	xplorerr::app_linear_regression()
}
 