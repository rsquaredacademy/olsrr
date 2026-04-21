tabPanel('Chi Square GoF', value = 'tab_chigof',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('Chi Square Goodness of Fit Test'),
        p('Test whether the observed proportions for a categorical variable differ from hypothesized proportions')
      ),
      column(6, align = 'right',
        actionButton(inputId='chigoflink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_chisq_gof_test.html', '_blank')"),
        actionButton(inputId='chigoflink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=QJiKVByQTt8', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(3, align = 'right', br(), h5('Variable:')),
      column(3, align = 'left',
          selectInput("var_chigof", label = '', width = '200px',
                          choices = "", selected = ""),
          bsTooltip("var_chigof", "Select a variable.",
                    "left", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Continuity Correction:')),
      column(4, align = 'left',
      selectInput('chigof_cc', '', width = '200px',
                  choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
      bsTooltip("chigof_cc", "Apply continuity correction",
        "bottom", options = list(container = "body")))
    ),
    fluidRow(
      br(),
      br(),
      column(6, align = 'right', br(), h5('Expected Proportion:')),
      column(4, align = 'left', uiOutput('chigof_prop'))
    ),
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'update_chigof', label = 'Update', width = '120px', icon = icon('check')),
        bsTooltip("update_chigof", "Update proportions",
                      "bottom", options = list(container = "body")))
    ),
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_chigof', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_chigof", "Click here to view result.",
                      "bottom", options = list(container = "body")))
    ),
    fluidRow(
        br(),
        column(12, align = 'center',
            verbatimTextOutput('chigof_out')
        )
    )
  )
)
