tabPanel('McNemar Test', value = 'tab_mcnemar',
  fluidPage(
    fluidRow(
      column(8, align = 'left',
        h4('McNemar Test'),
        p('Test if the proportions of two dichotomous variables are equal in the same population.')
      ),
      column(4, align = 'right',
        actionButton(inputId='mclink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_mcnemar_test.html', '_blank')"),
        actionButton(inputId='mclink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=MrbnGGcF6ek', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Using Variables',
            fluidPage(
              fluidRow(
                column(2, align = 'right', br(), h5('Variable 1:')),
                column(2, align = 'left',
                    selectInput("var_mcnemar1", label = '', width = '200px',
                                    choices = "", selected = ""),
                    bsTooltip("var_mcnemar1", "Select a variable.",
                              "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Variable 2:')),
                column(2, align = 'left',
                  selectInput("var_mcnemar2", label = '', width = '200px',
                    choices = "", selected = ""),
                  bsTooltip("var_mcnemar2", "Select a variable.",
                    "left", options = list(container = "body"))),
                    column(4, align = 'center',
                    br(),
                    actionButton(inputId = 'submit_mcnemar', label = 'Submit', width = '120px', icon = icon('check')),
                    bsTooltip("submit_mcnemar", "Click here to view test result.",
                                  "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('mcnemar_out')
                  )
              )
            )
          ),
          tabPanel('Calculator',
            fluidPage(
              fluidRow(
                column(5, align = 'right', br(), h5('0')),
                column(1, align = 'right', br(), h5('')),
                column(1, align = 'right', br(), h5('1'))
              ),
              fluidRow(
                column(4, align = 'right', br(), h5('0')),
                column(2, align = 'left',
                    numericInput("mc_00", label = '', width = '100px',
                                    min = 0, step = 1, value = 1)),
                # column(2, align = 'right', br(), h5('')),
                column(4, align = 'left',
                    numericInput('mc_01', label = '', width = '100px',
                      min = 0, value = 1, step = 1))
              ),
              fluidRow(
                column(4, align = 'right', br(), h5('1')),
                column(2, align = 'left',
                    numericInput("mc_10", label = '', width = '100px',
                                    min = 0, step = 1, value = 1)),
                # column(2, align = 'right', br(), h5('')),
                column(4, align = 'left',
                    numericInput('mc_11', label = '', width = '100px',
                      min = 0, value = 1, step = 1))
              ),
              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_mcnemarc', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_mcnemarc", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('mcnemarc_out'))
              )
            )
          )
        )
      )
    )
  )
)
