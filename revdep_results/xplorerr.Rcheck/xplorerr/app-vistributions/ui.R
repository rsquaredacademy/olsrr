library(shiny)
library(shinyBS)
library(magrittr)

shinyUI(

    navbarPage(HTML("vistributions"), id = 'mainpage',

    source('ui/ui_analyze.R', local = TRUE)[[1]],
    source('ui/ui_exit_button.R', local = TRUE)[[1]]
))
