tabPanel('One Way ANOVA', value = 'tab_anova',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('One Way ANOVA'),
        p('One way analysis of variance.')
      ),
      column(6, align = 'right',
        actionButton(inputId='anovalink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_oneway_anova.html', '_blank')"),
        actionButton(inputId='anovalink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=6ywh-fVTvFQ', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(2, align = 'right', br(), h5('Variable:')),
      column(2, align = 'left',
          selectInput("var_anova1", label = '', width = '180px',
                          choices = "", selected = ""),
          bsTooltip("var_anova1", "Select a variable.",
                    "left", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Grouping Variable:')),
      column(2, align = 'left',
        selectInput("var_anova2", label = '', width = '180px',
                        choices = "", selected = ""),
        bsTooltip("var_anova2", "Select a grouping variable.",
                  "left", options = list(container = "body"))),
                  column(4, align = 'center',
                  br(),
                  actionButton(inputId = 'submit_anova', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_anova", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
    ),

    fluidRow(
        br(),
        column(12, align = 'center',
            verbatimTextOutput('anova_out')
        )
    )
  )
)
