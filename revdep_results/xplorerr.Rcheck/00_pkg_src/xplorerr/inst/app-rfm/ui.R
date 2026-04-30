library(shiny)
library(shinyBS)
library(shinycssloaders)
library(magrittr)
library(standby)

shinyUI(

    navbarPage(HTML("rfm"), id = 'mainpage',

    # source('ui/ui_welcome.R', local = TRUE)[[1]],	
    source('ui/ui_data.R', local = TRUE)[[1]],
    source('ui/ui_analyze.R', local = TRUE)[[1]],
    source('ui/ui_exit_button.R', local = TRUE)[[1]]
))
