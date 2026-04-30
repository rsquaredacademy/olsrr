tabPanel('Cross Table', value = 'tab_cross',

         fluidPage(

            includeCSS("styles.css"),

            fluidRow(
              column(5, align = 'left',
                h4('Two Way Tables'),
                p('Generates two way tables of categorical variables. The tables created can be 
                    visualized as barplots and mosaicplots.')
              ),
              column(7, align = 'right',
                actionButton(inputId='crosstablink1', label="Help", icon = icon("question-circle"),
                  onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/ds_cross_table.html', '_blank')"),
                actionButton(inputId='crosstablink3', label="Demo", icon = icon("video-camera"),
                  onclick ="window.open('https://www.youtube.com/watch?v=_93BIFMIaGg', '_blank')")
              )
            ),
            hr(),

            fluidRow(

                column(2, align = 'right', br(), h5('Variable 1:')),

                column(2, align = 'left',

                        selectInput("var1_cross", label = '', width = '150px',
                                    choices = "", selected = ""
                        ),
                        bsTooltip("var1_cross", "Select first variable.",
                              "left", options = list(container = "body"))
                ),

                column(2, align = 'right', br(), h5('Variable 2:')),

                column(2, align = 'left',

                        selectInput("var2_cross", label = '', width = '150px',
                                    choices = "", selected = ""
                        ),
                        bsTooltip("var2_cross", "Select second variable.",
                              "left", options = list(container = "body"))
                ),

                column(4, align = 'center',

                    br(),

                    actionButton(inputId = 'submit_cross', label = 'Submit', width = '150px', icon = icon('check')),
                    bsTooltip("submit_cross", "Click here to view cross tab.",
                              "top", options = list(container = "body"))

                )
            ),

            fluidRow(

                column(12, align = 'center',

                    br(),
                    br(),

                    verbatimTextOutput('cross')

                )

            ),

            fluidRow(

                column(12, align = 'center',

                    # h3('Stacked Bar Plot', style = 'align:center;'),
                    uiOutput('cross1_title'),
                    plotOutput('cross_bar_stacked', height = "500px", width = "75%")

                )

            ),

            fluidRow(

                column(12, align = 'center',

                    # h3('Grouped Bar Plot'),
                    uiOutput('cross2_title'),
                    plotOutput('cross_bar_grouped', height = "500px", width = "75%")

                )

            ),

            fluidRow(

                column(12, align = 'center',

                    # h3('Proportional Bar Plot'),
                    uiOutput('cross3_title'),
                    plotOutput('cross_bar_proportional', height = "500px", width = "75%")

                )

            ),

            fluidRow(

                column(12, align = 'center',

                    # h3('Mosaic Bar Plot'),
                    uiOutput('cross4_title'),
                    plotOutput('cross_mosaic_plot', height = "500px", width = "75%")

                )

            )

         )

)
