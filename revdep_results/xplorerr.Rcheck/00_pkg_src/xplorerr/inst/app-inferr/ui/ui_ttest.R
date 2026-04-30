tabPanel('One Sample t', value = 'tab_ttest',
  fluidPage(
    fluidRow(
      column(8, align = 'left',
        h4('One Sample t Test'),
        p('Performs t tests on the equality of means. It tests the hypothesis 
          that a sample has a mean equal to a hypothesized value.')
      ),
      column(4, align = 'right',
        actionButton(inputId='ostlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_os_t_test.html', '_blank')"),
        actionButton(inputId='ostlink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=7eNfzplm86Y', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(2, align = 'right', br(), h5('Variable:')),
      column(4, align = 'left',
          selectInput("var_ttest", label = '', width = '200px',
                          choices = "", selected = ""),
          bsTooltip("var_ttest", "Select a variable.",
                    "left", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Alternative:')),
      column(4, align = 'left',
        selectInput('ttest_type', '', width = '200px',
          choices = c("both", "less", "greater", "all"),
          selected = "both"),
        bsTooltip("ttest_type", "Alternative hypothesis",
          "bottom", options = list(container = "body"))
      )

    ),

    fluidRow(
      column(2, align = 'right', br(), h5('alpha:')),
      column(4, align = 'left',
          numericInput('ttest_alpha', label = '', width = '200px',
                           min = 0, value = 0.05, step = 0.01),
          bsTooltip("ttest_alpha", "Acceptable tolerance for type 1 error.",
                    "bottom", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Mu:')),
      column(4, align = 'left',
        numericInput('ttest_mu', label = '', min = 0, value = 1, step = 1, width = '200px'),
        bsTooltip("ttest_mu", "True value of the mean.",
          "bottom", options = list(container = "body"))
      )

    ),

    fluidRow(

        column(12, align = 'center',

        br(),
        br(),

        actionButton(inputId = 'submit_ttest', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_ttest", "Click here to view t test result.",
                      "bottom", options = list(container = "body"))

        )
    ),

    fluidRow(

        br(),

        column(12, align = 'center',

            verbatimTextOutput('ttest_out')

        )

    )
  )
)
