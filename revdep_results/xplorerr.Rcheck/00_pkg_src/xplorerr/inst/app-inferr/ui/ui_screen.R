tabPanel('Screen', value = 'tab_screen',

        fluidPage(

          fluidRow(
            column(8, align = 'left',
              h4('Data Screening'),
              p('Screen data for missing values, verify column names and data types.')
            ),
            column(4, align = 'right',
              actionButton(inputId='dscreenlink1', label="Help", icon = icon("question-circle"),
                onclick ="window.open('http://www.rsquaredacademy.com/descriptr/reference/ds_screener.html', '_blank')"),
              actionButton(inputId='dscreenlink3', label="Demo", icon = icon("video-camera"),
                onclick ="window.open('https://www.youtube.com/watch?v=lheslEn5icc#t=03m04s', '_blank')")
            )
          ),
          hr(),

            fluidRow(

                column(12, align = 'center',

                  verbatimTextOutput('screen')

                )

            ),

            fluidRow(

                br(),

                column(12, align = 'center',
                    actionButton('finalok', 'Approve', width = '120px', icon = icon('sign-out')),
                    bsTooltip("finalok", "Click here to approve the data.",
                              "top", options = list(container = "body"))
                )

            )
        )
)
