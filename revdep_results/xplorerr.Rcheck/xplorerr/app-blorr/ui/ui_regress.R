tabPanel('Regression', value = 'tab_regress',
  fluidPage(
    br(),
    fluidRow(
      column(6, align = 'left',
        h4('Binary Logistic Regression'),
        p('Maximum likelihood estimation.')
      ),
      column(6, align = 'right',
        actionButton(inputId='mlr1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_regress.html', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("regress_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("regress_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_regress', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_regress", "Click here to view regression result.",
                      "bottom", options = list(container = "body")))
    ),
    fluidRow(
        br(),
        uiOutput('reg1_title'),
        # column(12, align = 'center', h4('Regression Result')),
        hr(),
        column(12, align = 'center', verbatimTextOutput('regress_out')),
        hr()
    )
  )
)
