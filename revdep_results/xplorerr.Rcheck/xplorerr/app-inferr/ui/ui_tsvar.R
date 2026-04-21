tabPanel('Two Sample Variance', value = 'tab_tsvartest',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('Two Sample Variance Test'),
        p('Performs tests on the equality of standard deviations (variances).')
      ),
      column(6, align = 'right',
        actionButton(inputId='tsvarlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_ts_var_test.html', '_blank')"),
        actionButton(inputId='tsvarlink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=H5XX3wmF1Sc', '_blank')")
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
                    selectInput("var_tsvartest1", label = '', width = '200px',
                                    choices = "", selected = ""),
                    bsTooltip("var_tsvartest1", "Select a variable.",
                              "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Variable 2:')),
                column(2, align = 'left',
                  selectInput("var_tsvartest2", label = '', width = '200px',
                    choices = "", selected = ""),
                  bsTooltip("var_tsvartest2", "Select a variable.",
                    "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Alternative:')),
                column(2, align = 'left',
                  selectInput('tsvartest_type', '',
                      choices = c("less", "greater", "all"),
                      selected = "all"),
                  bsTooltip("tsvartest_type", "Alternative hypothesis",
                    "bottom", options = list(container = "body"))
                  )
              ),

              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_tsvartest', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_tsvartest", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),

              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('tsvartest_out')
                  )
              )
            )
          ),
          tabPanel('Using Groups',
            fluidPage(
              fluidRow(
                column(2, align = 'right', br(), h5('Variable:')),
                column(2, align = 'left',
                    selectInput("var_tsvartestg1", label = '', width = '200px',
                                    choices = "", selected = ""),
                    bsTooltip("var_tsvartestg1", "Select a variable.",
                              "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Grouping Variable:')),
                column(2, align = 'left',
                  selectInput("var_tsvartestg2", label = '', width = '200px',
                    choices = "", selected = ""),
                  bsTooltip("var_tsvartestg2", "Select grouping variable.",
                    "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Alternative:')),
                column(2, align = 'left',
                  selectInput('tsvartestg_type', '',
                      choices = c("less", "greater", "all"),
                      selected = "all"),
                  bsTooltip("tsvartestg_type", "Alternative hypothesis",
                    "bottom", options = list(container = "body"))
                  )
              ),

              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_tsvartestg', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_tsvartestg", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),

              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('tsvartestg_out')
                  )
              )
            )
          )
        )
      )
    )
  )
)
