tabPanel('Measures of Influence', value = 'tab_inflobs',
  fluidPage(
    fluidRow(
      column(2, align = 'right', br(), h5('Select Procedure:')),
      column(4, align = 'left',
        selectInput('inflobs_select', label = '', width = '300px',
          choices = c("Cook's D Bar Plot", "Cook's D Chart", 
            "DFBETAs Panel", "DFFITS Plot", "Studentized Residual Plot",
            "Studentized Residual Chart", "Studentized Residuals vs Leverage",
            "Deleted Stud Resid vs Fitted", "Hadi Plot", "Potential Residual Plot"),
          selected = "Cook's D Bar Plot")
        )
    ),
    hr(),
    fluidRow(
      column(12, uiOutput("ui_inflobslink"))
    ),
    hr(),
    fluidRow(
      column(12, uiOutput("ui_inflobsfmla"))
    ),
    fluidRow(
      column(12, uiOutput("ui_inflobsprev"))
    ),
    fluidRow(
      column(12, uiOutput("ui_inflobssubmit"))
    ),
    fluidRow(
      br(),
      column(12, uiOutput('ui_inflobsplot'))
    ),
    fluidRow(
      br(),
      column(12, uiOutput('ui_inflobsprint'))
    )
  )
)
