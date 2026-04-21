tabPanel('Residual Diagnostics', value = 'tab_res_diag',
  fluidPage(
    fluidRow(
      column(2, align = 'right', br(), h5('Select Procedure:')),
      column(4, align = 'left',
        selectInput('restrial1', label = '', width = '300px',
          choices = c("Residual vs Predicted Plot", "Residual Box Plot", 
            "Residual Histogram", "Residual QQ Plot", "Normality Test"),
          selected = "Residual vs Predicted Plot")
        )
    ),
    hr(),
    fluidRow(
      column(12, uiOutput("ui_resdiaglink"))
    ),
    hr(),
    fluidRow(
      column(12, uiOutput("ui_resdiagfmla"))
    ),
    fluidRow(
      column(12, uiOutput("ui_resdiagprev"))
    ),
    fluidRow(
      column(12, uiOutput("ui_resdiagsubmit"))
    ),
    fluidRow(
      br(),
      column(12, uiOutput("ui_resdiagout"))
      # column(12, align = 'center', plotOutput('resdiagplot', height = '500px'))
    )
  )
)
