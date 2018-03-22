library(shiny)
library(shinyBS)
library(shinythemes)
library(descriptr)
library(dplyr)

shinyUI(
		
  navbarPage(HTML("olsrr"), id = 'mainpage',

  source('ui/ui_data.R', local = TRUE)[[1]],
  source('ui/ui_analyze.R', local = TRUE)[[1]],
  source('ui/ui_exit_button.R', local = TRUE)[[1]]

))
