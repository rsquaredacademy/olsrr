tabPanel('Model Fit Assessment', value = 'tab_mfit',
  fluidPage(
    fluidRow(
      column(2, align = 'right', br(), h5('Select Procedure:')),
      column(4, align = 'left',
        selectInput('mfit_select', label = '', width = '300px',
          choices = c("Residual Fit Spread Plot", "Part & Partial Correlations",
            "Observed vs Fitted Plot", "Lack of Fit F Test", 
            "Diagnostics Panel"),
          selected = "Residual Fit Spread Plot")
        )
    ),
    hr(),
    fluidRow(
      column(12, uiOutput("ui_mfitlink"))
    ),
    hr(),
    fluidRow(
      column(12, uiOutput("ui_mfitfmla"))
    ),
    fluidRow(
      column(12, uiOutput("ui_mfitprev"))
    ),
    fluidRow(
      column(12, uiOutput("ui_mfitsubmit"))
    ),
    fluidRow(
      column(12, uiOutput("ui_mfitout"))
    )
    
  )
)
