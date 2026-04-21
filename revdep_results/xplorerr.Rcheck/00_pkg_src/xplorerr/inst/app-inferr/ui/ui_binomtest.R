tabPanel('Binomial Test', value = 'tab_binomtest',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('Binomial Test'),
        p('Test whether the proportion of successes on a two-level categorical 
          dependent variable significantly differs from a hypothesized value.')
      ),
      column(6, align = 'right',
        actionButton(inputId='binomtestlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_binom_calc.html', '_blank')"),
        actionButton(inputId='binomtestlink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=Xe-NJX55gcg', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Using Variable',
            fluidPage(
              fluidRow(
                column(1, align = 'right', br(), h5('Variable:')),
                column(3, align = 'left',
                    selectInput("var_binomtest", label = '', width = '200px',
                                    choices = "", selected = ""),
                    bsTooltip("var_binomtest", "Select a variable.",
                              "left", options = list(container = "body"))),
                column(1, align = 'right', br(), h5('Probability:')),
                column(3, align = 'left',
                    numericInput('binomtest_prob', label = '', width = '200px',
                                     min = 0, value = 0.5, max = 1, step = 0.01),
                    bsTooltip("binomtest_prob", "Probability",
                              "bottom", options = list(container = "body"))),
                column(4, align = 'center',
                  br(),
                  actionButton(inputId = 'submit_binomtest', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_binomtest", "Click here to view t test result.",
                    "bottom", options = list(container = "body")))
              ),

              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('binomtest_out')
                  )
              )
            )
          ),
          tabPanel('Calculator',
            fluidPage(
              fluidRow(
                column(2, align = 'right', br(), h5('N:')),
                column(2, align = 'left',
                    numericInput("n_binomcalc", label = '', width = '200px',
                                    min = 0, step = 1, value = 1),
                    bsTooltip("n_binomcalc", "Number of observations",
                              "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Success:')),
                column(2, align = 'left',
                    numericInput('s_binomcalc', label = '', width = '200px',
                                     min = 0, value = 1, step = 1),
                    bsTooltip("s_binomcalc", "Number of success",
                              "bottom", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Probability:')),
                column(2, align = 'left',
                  numericInput('p_binomcalc', label = '', width = '200px',
                    min = 0, value = 0.5, step = 0.1, max = 1),
                  bsTooltip("p_binomcalc", "Probability",
                      "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_binomcalc', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_binomcalc", "Click here to view t test result.",
                                "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('binomcalc_out')
                  )
              )
            )
          )
        )
      )
    )
  )
)
