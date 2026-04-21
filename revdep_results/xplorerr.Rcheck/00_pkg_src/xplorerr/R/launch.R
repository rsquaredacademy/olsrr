#' @title Visualization
#' @description Launches the visualizer app.
#' @examples
#' \dontrun{
#' app_visualizer()
#' }
#' @export
#'
app_visualizer <- function() {

	check_suggests('shiny')
	check_suggests('shinyBS')
	check_suggests('shinythemes')
	check_suggests('descriptr')
	check_suggests('dplyr')
	check_suggests('ggplot2')
	check_suggests('plotly')
	check_suggests('highcharter')
	check_suggests('purrr')
	check_suggests('tidyr')
	check_suggests('tibble')
	check_suggests('readxl')
	check_suggests('readr')
	check_suggests('jsonlite')
	check_suggests('magrittr')
	check_suggests('tools')
	check_suggests('lubridate')
	check_suggests('scales')
	check_suggests('stringr')

  message('Highcharts (www.highcharts.com) is a Highsoft software product which is not free for commercial and Governmental use')
  shiny::runApp(appDir = system.file("app-visualize", package = "xplorerr"))
}

#' @title Descriptive Statistics
#' @description Launches the descriptive statistics app.
#' @examples
#' \dontrun{
#' app_descriptive()
#' }
#' @export
#'
app_descriptive <- function() {

	check_suggests('descriptr')
	check_suggests('haven')
	check_suggests('jsonlite')
	check_suggests('readr')
	check_suggests('readxl')
	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')
	check_suggests('stringr')
	check_suggests('lubridate')

  shiny::runApp(appDir = system.file("app-descriptr", package = "xplorerr"))

}

#' @title Visualize distributions
#' @description Launches app for visualizing probability distributions.
#' @examples
#' \dontrun{
#' app_vistributions()
#' }
#' @export
#'
app_vistributions <- function() {

	check_suggests('vistributions')
	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')

  shiny::runApp(appDir = system.file("app-vistributions", package = "xplorerr"))

}


#' @title Inferential Statistics
#' @description Launches the inferential statistics app.
#' @examples
#' \dontrun{
#' app_inference()
#' }
#' @export
#'
app_inference <- function() {

	check_suggests('data.table')
	check_suggests('magrittr')
	check_suggests('descriptr')
	check_suggests('jsonlite')
	check_suggests('haven')
	check_suggests('lubridate')
	check_suggests('readr')
	check_suggests('readxl')
	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')
	check_suggests('stringr')

  shiny::runApp(appDir = system.file("app-inferr", package = "xplorerr"))

}

#' @title Linear Regression
#' @description Launches the linear regression app.
#' @examples
#' \dontrun{
#' app_linear_regression()
#' }
#' @export
#'
app_linear_regression <- function() {

	check_suggests('olsrr')
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

  shiny::runApp(appDir = system.file("app-olsrr", package = "xplorerr"))

}

#' @title Logistic Regression
#' @description Launches the logistic regression app.
#' @examples
#' \dontrun{
#' app_logistic_regression()
#' }
#' @export
#'
app_logistic_regression <- function() {

	check_suggests('blorr')
	check_suggests('descriptr')
	check_suggests('jsonlite')
	check_suggests('haven')
	check_suggests('lubridate')
	check_suggests('readr')
	check_suggests('readxl')
	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')
	check_suggests('stringr')
	check_suggests('tidyr')

  shiny::runApp(appDir = system.file("app-blorr", package = "xplorerr"))

}

#' @title RFM Analysis
#' @description Launches the RFM analysis app.
#' @examples
#' \dontrun{
#' app_rfm_analysis()
#' }
#' @export
#'
app_rfm_analysis <- function() {

	check_suggests('rfm')
	check_suggests('haven')
	check_suggests('jsonlite')
	check_suggests('readr')
	check_suggests('readxl')
	check_suggests('shinyBS')
	check_suggests('shinycssloaders')
	check_suggests('shinythemes')
	check_suggests('stringr')
	check_suggests('DT')

  shiny::runApp(appDir = system.file("app-rfm", package = "xplorerr"))

}




