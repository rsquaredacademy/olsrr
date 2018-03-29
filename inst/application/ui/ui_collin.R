tabPanel('Collinearity Diagnostics', value = 'tab_regcollin',
  fluidPage(
              br(),
              fluidRow(
                column(6, align = 'left',
                  h4('Collinearity Diagnostics'),
                  p('Variance inflation factor, tolerance, eigenvalues and condition indices.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='cdiaglink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_coll_diag.html', '_blank')")
                )
              ),
              hr(),
              fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("collin_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("collin_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'colldiag_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("colldiag_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_colldiag', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_colldiag", "Click here to view collinearity diagnostics.",
                              "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center', verbatimTextOutput('colldiag'))
              )
            )
)
