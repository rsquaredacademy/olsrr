tabPanel('One Sample Variance', value = 'tab_osvartest',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('One Sample Variance Test'),
        p('Performs tests on the equality of standard deviations (variances).It 
          tests that the standard deviation of a sample is equal to a hypothesized value.')
      ),
      column(6, align = 'right',
        actionButton(inputId='osvarlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_os_var_test.html', '_blank')"),
        actionButton(inputId='osvarlink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=00ZCHwMPaFY', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(2, align = 'right', br(), h5('Variable:')),
      column(4, align = 'left',
          selectInput("var_osvartest", label = '', width = '200px',
                          choices = "", selected = ""),
          bsTooltip("var_osvartest", "Select a variable.",
                    "left", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Alternative:')),
      column(4, align = 'left',
        selectInput('osvartest_type', '', width = '200px',
          choices = c("both", "less", "greater", "all"),
          selected = "both"),
        bsTooltip("osvartest_type", "Alternative hypothesis",
          "bottom", options = list(container = "body")))

    ),

    fluidRow(
      column(2, align = 'right', br(), h5('Conf Int')),
      column(4, align = 'left',
          numericInput('osvartest_conf', label = '', width = '200px',
                           min = 0, value = 0.95, step = 0.01),
          bsTooltip("osvartest_conf", "Confidence Level",
                    "bottom", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Std. Deviation')),
      column(4, align = 'left',
        numericInput("sd_osvartest", label = '', width = '200px',
          min = 0, step = 0.1, value = 0.5),
        bsTooltip("sd_osvartest", "Specify standard deviation",
          "left", options = list(container = "body")))
    ),

    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_osvartest', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_osvartest", "Click here to view test result.",
                      "bottom", options = list(container = "body")))
    ),

    fluidRow(
        br(),
        column(12, align = 'center',
            verbatimTextOutput('osvartest_out')
        )
    )
  )
)
