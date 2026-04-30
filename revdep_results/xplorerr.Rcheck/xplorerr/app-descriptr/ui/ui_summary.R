tabPanel('Summary', value = 'tab_summary',

    fluidPage(

      fluidRow(
        column(8, align = 'left',
          h4('Summary Statistics'),
          p('Generate descriptive statistics for continuous data.')
        ),
        column(4, align = 'right',
          actionButton(inputId='sumrylink1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/ds_summary_stats.html', '_blank')"),
          actionButton(inputId='sumrylink3', label="Demo", icon = icon("video-camera"),
            onclick ="window.open('https://www.youtube.com/watch?v=cq6_1SQjNmM', '_blank')")
        )
      ),
      hr(),

        fluidRow(

            column(4, align = 'right',

                br(),
                br(),
                h5('Variable:')

            ),

            column(2, align = 'left',

                br(),

                selectInput("var_summary", label = '',
                               choices = "", selected = "", width = '150px'
                ),
                bsTooltip("var_summary", "Select a variable.",
                              "bottom", options = list(container = "body"))

            ),

            column(6, align = 'left',

                br(),
                br(),

                actionButton(inputId = 'submit_summary', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_summary", "Click here to view summary statistics.",
                              "bottom", options = list(container = "body"))

            )

        ),

        fluidRow(

            br(),
            br(),

            column(12, align = 'center',

                verbatimTextOutput('summary')

            )

        )

    )

)
