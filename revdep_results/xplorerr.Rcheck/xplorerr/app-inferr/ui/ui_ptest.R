tabPanel('Paired Sample t', value = 'tab_ptest',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('Paired Sample t Test'),
        p('Tests that two samples have the same mean, assuming paired data.')
      ),
      column(6, align = 'right',
        actionButton(inputId='ab1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_ts_paired_ttest.html', '_blank')"),
        actionButton(inputId='ab3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=dgMJcgeXLL0', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      br(),
      column(2, align = 'right', br(), h5('Variable 1:')),
      column(4, align = 'left',
          selectInput("var_ptest1", label = '', width = '200px',
                          choices = "", selected = ""),
          bsTooltip("var_ptest1", "Select a variable.",
                    "left", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Variable 2:')),
      column(4, align = 'left',
        selectInput("var_ptest2", label = '', width = '200px',
                        choices = "", selected = ""),
        bsTooltip("var_ptest2", "Select a variable.",
                  "left", options = list(container = "body")))
    ),

    fluidRow(
      column(2, align = 'right', br(), h5('Conf Int')),
      column(4, align = 'left',
          numericInput('ptest_conf', label = '', width = '200px',
                           min = 0, value = 0.95, step = 0.01),
          bsTooltip("ptest_conf", "Confidence Level",
                    "bottom", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Alternative:')),
      column(4, align = 'left',
        selectInput('ptest_type', '', width = '200px',
                    choices = c("both", "less", "greater", "all"),
                    selected = "both"),
        bsTooltip("ptest_type", "Alternative hypothesis",
                  "bottom", options = list(container = "body"))
      )
    ),

    fluidRow(

        column(12, align = 'center',

        br(),
        br(),

        actionButton(inputId = 'submit_ptest', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_ptest", "Click here to view t test result.",
                      "bottom", options = list(container = "body"))

        )
    ),

    fluidRow(

        br(),

        column(12, align = 'center',

            verbatimTextOutput('ptest_out')

        )

    )
  )
)
