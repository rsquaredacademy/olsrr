tabPanel('Levene Test', value = 'tab_levtest',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('Levene Test'),
        p("Levene's robust test statistic for the equality of variances and the
          two statistics proposed by Brown and Forsythe that replace the mean in
          Levene's formula with alternative location estimators. The first alternative
          replaces the mean with the median. The second alternative replaces the mean
          with the 10% trimmed mean.")
      ),
      column(6, align = 'right',
        actionButton(inputId='levtestlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_levene_test.html', '_blank')"),
        actionButton(inputId='levtestlink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=Yz5fhDhzMKI', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Using Variables',
            fluidPage(
              fluidRow(
                column(2, align = 'right', br(), h5('Variables:')),
                column(10, align = 'left',
                    selectInput("var_levtest", label = '', width = '660px',
                                    choices = "", selected = "", multiple = TRUE,
                                    selectize = TRUE),
                    bsTooltip("var_levtest", "Select variables.",
                              "left", options = list(container = "body")))
              ),
              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_levtest', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_levtest", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('levtest_out')
                  )
              )
            )
          ),
          tabPanel('Using Groups',
            fluidPage(
              fluidRow(
                column(2, align = 'right', br(), h5('Variable:')),
                column(2, align = 'left',
                    selectInput("var_levtestg1", label = '', width = '200px',
                                    choices = "", selected = ""),
                    bsTooltip("var_levtestg1", "Select a variable.",
                              "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Grouping Variable:')),
                column(2, align = 'left',
                selectInput("var_levtestg2", label = '', width = '200px',
                  choices = "", selected = ""),
                bsTooltip("var_levtestg2", "Select a  grouping variable.",
                  "left", options = list(container = "body"))),
                  column(4, align = 'center',
                  br(),
                  actionButton(inputId = 'submit_levtestg', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_levtestg", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('levtestg_out')
                  )
              )
            )
          )
        )
      )
    )
  )
)
