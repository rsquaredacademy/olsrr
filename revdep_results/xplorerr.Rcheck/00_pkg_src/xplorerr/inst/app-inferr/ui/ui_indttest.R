tabPanel('Independent Sample t', value = 'tab_indttest',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('Independent Sample t Test'),
        p('Compare the means of two independent groups in order to determine 
          whether there is statistical evidence that the associated population 
          means are significantly different.')
      ),
      column(6, align = 'right',
        actionButton(inputId='indttest1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_ts_ind_ttest.html', '_blank')"),
        actionButton(inputId='indttest3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=iKFFzv9WiUo', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(2, align = 'right', br(), h5('Variable 1:')),
      column(4, align = 'left',
          selectInput("var_itest1", label = '', width = '200px',
                          choices = "", selected = ""),
          bsTooltip("var_itest1", "Select a variable.",
                    "left", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Variable 2:')),
      column(4, align = 'left',
        selectInput("var_itest2", label = '', width = '200px',
                        choices = "", selected = ""),
        bsTooltip("var_itest2", "Select a variable.",
                  "left", options = list(container = "body")))
    ),

    fluidRow(
      column(2, align = 'right', br(), h5('alpha:')),
      column(4, align = 'left',
          numericInput('itest_conf', label = '', width = '200px',
                           min = 0, value = 0.95, step = 0.01),
          bsTooltip("itest_conf", "Confidence Level",
                    "bottom", options = list(container = "body"))),
      column(2, align = 'right', br(), h5('Alternative:')),
      column(4, align = 'left',
        selectInput('itest_type', '', width = '200px',
                    choices = c("both", "less", "greater", "all"),
                    selected = "both"),
        bsTooltip("itest_type", "Alternative hypothesis",
                  "bottom", options = list(container = "body"))
      )
    ),

    fluidRow(

        column(12, align = 'center',

        br(),
        br(),

        actionButton(inputId = 'submit_itest', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submitittest", "Click here to view t test result.",
                      "bottom", options = list(container = "body"))

        )
    ),

    fluidRow(

        br(),

        column(12, align = 'center',

            verbatimTextOutput('itest_out')

        )

    )
  )
)
