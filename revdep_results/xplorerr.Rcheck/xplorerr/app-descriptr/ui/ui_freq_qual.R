tabPanel('Frequency - I', value = 'tab_fqual',

        fluidPage(

          fluidRow(
            column(6, align = 'left',
              h4('Frequency Tables'),
              p('Generates frequency table for factor data and returns the frequency, cumulative frequency, 
                frequency percent, cumulative frequency percent and a bar plot.')
            ),
            column(6, align = 'right',
              actionButton(inputId='fqualink1', label="Help", icon = icon("question-circle"),
                onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/ds_freq_table.html', '_blank')"),
              actionButton(inputId='fqualink3', label="Demo", icon = icon("video-camera"),
                onclick ="window.open('https://www.youtube.com/watch?v=BMKV2Rocm1s', '_blank')")
            )
          ),
          hr(),

            fluidRow(

                column(4, align = 'right',

                    br(),
                    br(),
                    h5('Variable:')

                ),

                column(3, align = 'left',

                    br(),

                    selectInput("var_table", label = '',
                                   choices = "", selected = "", width = '200px'),
                    bsTooltip("var_table", "Select a variable.",
                              "right", options = list(container = "body"))

                ),

                column(4, align = 'left',

                    br(),
                    br(),

                    actionButton(inputId = 'submit_fqual', label = 'Submit', width = '120px', icon = icon('check')),
                    bsTooltip("submit_fqual", "Click here to view frequency table.",
                              "bottom", options = list(container = "body"))

                )

            ),

            fluidRow(

                column(12, align = 'center',

                    br(),
                    br(),
                    uiOutput('freq1_title'),
                    # h3('Frequency Table'),
                    br(),
                    verbatimTextOutput('freq_qual'),
                    br(),
                    plotOutput('qual_vert', height = "500px", width = "75%"),
                    br(),
                    plotOutput('qual_horiz')


                 )

            )
        )
)
