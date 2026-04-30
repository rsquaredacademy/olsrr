library(shiny)
library(shinyBS)
library(DT)

shinyUI(

    navbarPage(HTML("descriptr"), id = 'mainpage',

    source('ui/ui_data.R', local = TRUE)[[1]],
    source('ui/ui_analyze.R', local = TRUE)[[1]],
    source('ui/ui_exit_button.R', local = TRUE)[[1]]
))
