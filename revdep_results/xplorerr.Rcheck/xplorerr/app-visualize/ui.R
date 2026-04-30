library(shiny)
library(shinyBS)
library(descriptr)
library(dplyr)
library(DT)

shinyUI(	
  navbarPage(HTML("visualizer"), id = 'mainpage',
    source('ui/ui_data.R', local = TRUE)[[1]],
    source('ui/ui_visualize.R', local = TRUE)[[1]],
    source('ui/ui_exit_button.R', local = TRUE)[[1]]
))
