tabPanel('Variable Selection', value = 'tab_var_select',
  fluidPage(
    fluidRow(
      column(2, align = 'right', br(), h5('Select Procedure:')),
      column(4, align = 'left',
        selectInput('mselect', label = '', width = '300px',
          choices = c("All Possible", "Best Subset", "Stepwise", "Forward",
            "Backward",  "stepAIC Forward", "stepAIC Backward", "stepAIC Both"),
          selected = "Residual vs Predicted Plot")
        )
    ),
    hr(),
    fluidRow(
      column(12, uiOutput("ui_mselectlink"))
    ),
    hr(),
    fluidRow(
      column(12, uiOutput("ui_mselectfmla"))
    ),
    fluidRow(
      column(12, uiOutput("ui_mselectprev"))
    ),
    fluidRow(
      column(12, uiOutput("ui_mselectrow1"))
    ),
    fluidRow(
      column(12, uiOutput("ui_mselectrow2"))
    ),
    fluidRow(
      column(12, uiOutput("ui_mselectsubmit"))
    ),
    fluidRow(
      column(12, uiOutput('ui_mseloutput'))
    ),
    fluidRow(
      column(12, uiOutput('ui_mselplot'))
    )
  )
)
