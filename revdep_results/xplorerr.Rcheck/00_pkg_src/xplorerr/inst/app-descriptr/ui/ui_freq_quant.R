tabPanel('Frequency - II', value = 'tab_fquant',

         fluidPage(

           fluidRow(
             column(6, align = 'left',
               h4('Frequency Table (Quantitative Data)'),
               p('Generates the frequency distribution of continuous data by splitting
                the data into equidistant intervals created based on the number of bins specified.')
             ),
             column(6, align = 'right',
               actionButton(inputId='fquantlink1', label="Help", icon = icon("question-circle"),
                 onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/ds_freq_cont.html', '_blank')"),
               actionButton(inputId='fquantlink3', label="Demo", icon = icon("video-camera"),
                 onclick ="window.open('https://www.youtube.com/watch?v=ft1L6VnJEog', '_blank')")
             )
           ),
           hr(),

            fluidRow(

                column(1, align = 'right', br(), h5('Variable:')),

                column(3, align = 'left',

                    selectInput("var_freq_quant", label = '', width = '200px',
                                    choices = "", selected = ""
                    ),
                    bsTooltip("var_freq_quant", "Select a variable.",
                              "left", options = list(container = "body"))

                ),

                column(1, align = 'right', br(), h5('Filter:')),

                column(3, align = 'left',
                    sliderInput(inputId = 'filter_quant',  width = '250px',
                                label = '',
                                min = 0, max = 100,
                                step = 1, value = c(20, 80)
                    ),
                    bsTooltip("filter_quant", "Filter data.",
                              "bottom", options = list(container = "body"))
                ),

                column(1, align = 'right', br(), h5('Bins:')),

                column(3, align = 'left',

                    numericInput('bins', label = '', width = '200px',
                                     min = 1, value = 5),
                    bsTooltip("bins", "Specify the number of bins.",
                              "bottom", options = list(container = "body"))

                )

            ),

            fluidRow(

                column(12, align = 'center',

                br(),
                br(),

                actionButton(inputId = 'submit_fquant', label = 'Submit', width = '180px', icon = icon('check')),
                bsTooltip("submit_fquant", "Click here to view frequency table.",
                              "bottom", options = list(container = "body"))

                )
            ),

            fluidRow(

                br(),

                column(12, align = 'center',

                    verbatimTextOutput('freq_quant')

                )

            ),

            fluidRow(

                column(12, align = 'center',

                    uiOutput('freq2_title'),
                    # h3('Histogram'),
                    plotOutput('hist_freq_quant', height = "500px", width = "75%")
                )
            )

    )

)
