tabPanel('Pie Chart', value = 'tab_pie',

    fluidPage(
        fluidRow(
                 column(12, align = 'left',
                   h4('Pie Chart')
                 )
               ),
               hr(),

        fluidRow(

            column(12,

                tabsetPanel(type = 'tabs',

                    # variable selection
                    tabPanel('Variable',

                        # user interface
                        column(4,

                            column(6,

                                # select variable
                                selectInput('pie_select', 'Select Variable',
                                                choices = "", selected = ""
                                ),

                                # direction
                                selectInput('pie_clock', 'Clockwise',
                                                choices = c("TRUE" = TRUE, "FALSE" = FALSE), selected = "FALSE"
                                ),

                                # edges
                                numericInput('pie_edges', 'Edges', min = 1, step = 1, value = 200)

                            ),

                            column(6,

                                # select labels
                                selectInput('pie_label', 'Select Labels',
                                                choices = "", selected = ""
                                ),


                                # radius
                                numericInput('pie_radius', 'Radius', min = 0.1, step = 0.1, value = 1),

                                # initial angle
                                numericInput('pie_angle', 'Initial Angle', min = 0, step = 1, value = 45)

                            )

                        ),

                        # plot
                        column(8,

                            plotOutput('pie_1', '500px')

                        )

                    ),

                    # pie
                    tabPanel('Options',

                        # user interface
                        column(4,

                            fluidRow(

                                column(6,

                                    # border color
                                    textInput('pie_border', 'Border Color', value = "black"),

                                    # shading density
                                    numericInput('pie_density', 'Shading Density', min = 0, step = 1, value = NULL)

                                ),

                                column(6,

                                    # line type
                                    numericInput('pie_lty', 'Line Type', min = 1, max = 5, step = 1, value = 1),

                                    # shading density angle
                                    numericInput('pie_dangle', 'Angle', min = 0, step = 1, value = 45)

                                )

                            ),

                            h4('Color Options'),

                            # dynamic ui for setting bar colors
                                fluidRow(
                                        column(6, numericInput("ncolpie", "Number of Colors", value = 0, min = 0)),
                                        column(6, uiOutput("ui_ncolpie"))
                                    )
                                ),


                        # plot
                        column(8,

                            plotOutput('pie_2', '500px')

                        )

                    ),

                     # variable selection
                    tabPanel('Title',

                        # user interface
                        column(4,

                            column(6,

                                # title
                                textInput('pie_title', 'Title', value = 'title'),

                                # title color
                                textInput('pie_titlecol', 'Title Color', value = 'black')

                            ),

                            column(6,

                                # title font
                                numericInput('pie_font', 'Font', min = 1, step = 1, value = 1, max = 5),

                                # initial angle
                                numericInput('pie_cex', 'Size', min = 0.1, step = 0.1, value = 1)

                            )

                        ),

                        # plot
                        column(8,

                            plotOutput('pie_3', '500px')

                        )

                    ),

                    tabPanel('Plot',

                        fluidRow(

                            column(8, offset = 2,

                                plotOutput('pie_final', '600px')

                            )

                        )

                    )

                )

            )

        )

    )

)
