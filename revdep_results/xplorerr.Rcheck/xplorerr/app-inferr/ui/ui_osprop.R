tabPanel('One Sample Proportion', value = 'tab_osproptest',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('One Sample Proportion Test'),
        p('Compares proportion in one group to a specified population proportion.')
      ),
      column(6, align = 'right',
        actionButton(inputId='osproplink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_os_prop_test.html', '_blank')"),
        actionButton(inputId='osproplink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=K8BNGJYmvlI', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Using Variables',
            fluidPage(
              fluidRow(
                column(2, align = 'right', br(), h5('Variable:')),
                column(2, align = 'left',
                    selectInput("var_osproptest", label = '', width = '200px',
                                    choices = "", selected = ""),
                    bsTooltip("var_osproptest", "Select a variable.",
                              "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Probability:')),
                column(2, align = 'left',
                    numericInput('osproptest_prob', label = '',
                                     min = 0, value = 0.5, max = 1, step = 0.01),
                    bsTooltip("osproptest_prob", "Probability",
                              "bottom", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Alternative:')),
                column(2, align = 'left',
                  selectInput('osproptest_type', '',
                    choices = c("both", "less", "greater", "all"),
                    selected = "both"),
                bsTooltip("osproptest_type", "Alternative hypothesis",
                  "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_osproptest', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_osproptest", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('osproptest_out')
                  )
              )
            )
          ),
          tabPanel('Calculator',
            fluidPage(
              fluidRow(
                column(3, align = 'right', br(), h5('N:')),
                column(3, align = 'left',
                    numericInput("n_ospropcalc", label = '', width = '150px',
                                    min = 0, step = 1, value = 1),
                    bsTooltip("n_ospropcalc", "Number of observations",
                              "left", options = list(container = "body"))),
                column(3, align = 'right', br(), h5('Hypothesized Proportion:')),
                column(3, align = 'left',
                    numericInput('p_ospropcalc', label = '', width = '150px',
                                     min = 0, value = 0.5, step = 0.1, max = 1),
                    bsTooltip("p_ospropcalc", "Hypothesized Proportion",
                              "bottom", options = list(container = "body")))
              ),
              fluidRow(
                column(3, align = 'right', br(), h5('Probability:')),
                column(3, align = 'left',
                  numericInput('prob_ospropcalc', label = '', width = '150px',
                    min = 0, value = 0.5, step = 0.1, max = 1),
                  bsTooltip("prob_ospropcalc", "Probability",
                      "bottom", options = list(container = "body"))),
                column(3, align = 'right', br(), h5('Alternative:')),
                column(3, align = 'left',
                  selectInput('ospropcalc_type', '', width = '150px',
                    choices = c("both", "less", "greater", "all"),
                    selected = "both"),
                bsTooltip("ospropcalc_type", "Alternative hypothesis",
                        "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_ospropcalc', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_ospropcalc", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('ospropcalc_out')
                  )
              )
            )
          )
        )
      )
    )
  )
)
