tabPanel('3D Pie Chart', value = 'tab_pie3d',

    fluidPage(
        fluidRow(
                 column(12, align = 'left',
                   h4('3D Pie Chart')
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
                                selectInput('pie3_select', 'Select Variable',
                                                choices = "", selected = ""
                                ),

                                # radius
                                numericInput('pie3_radius', 'Radius', min = 0.1, step = 0.1, value = 1)

                            ),

                            column(6,

                                # select labels
                                selectInput('pie3_label', 'Select Labels',
                                                choices = "", selected = ""
                                ),

                                # height
                                numericInput('pie3_height', 'Height', min = 0.1, step = 0.1, value = 0.1)

                            )

                        ),

                        # plot
                        column(8,

                            plotOutput('pie3_1', '500px')

                        )

                    ),

                    # pie
                    tabPanel('Options',

                        # user interface
                        column(4,

                            fluidRow(

                                column(6,

                                    # border color
                                    textInput('pie3_border', 'Border Color', value = "black"),

                                    # start
                                    numericInput('pie3_start', 'Start', min = 0, step = 1, value = 0),

                                    # explode
                                    numericInput('pie3_explode', 'Explode', min = 0, step = 0.1, value = 0),

                                    # shade
                                    numericInput('pie3_shade', 'Shade', min = 0, step = 0.1, value = 0.8),

                                    # edges
                                    numericInput('pie3_edges', 'Edges', min = 0, step = 1, value = 200)

                                )

                            )

                        ),

                        # plot
                        column(8,

                            plotOutput('pie3_2', '500px')

                        )

                    ),

                    # pie
                    tabPanel('Color',

                        # user interface
                        column(4,

                                # dynamic ui for setting bar colors
                                fluidRow(
                                  column(6, numericInput("ncolpie3", "Number of Colors", value = 0, min = 0)),
                                  column(6, uiOutput("ui_ncolpie3"))
                                )

                        ),

                        # plot
                        column(8,

                            plotOutput('pie3_3', '500px')

                        )

                    ),

                    # pie
                    tabPanel('Label',

                        # user interface
                        column(4,

                            fluidRow(

                                column(6,

                                    # label color
                                    textInput('pie3_labcol', 'Label Color', value = "black"),

                                    # label size
                                    numericInput('pie3_labcex', 'Label Size', min = 0.1, step = 0.1, value = 1.5),

                                    # label radius
                                    numericInput('pie3_labrad', 'Label Radius', min = 0.1, step = 0.01, value = 1.25)

                                )

                            ),

                            # dynamic ui for setting bar colors
                                fluidRow(
                                        column(6, numericInput("nlabpospie3", "Number of Labels", value = 0, min = 0)),
                                        column(6, uiOutput("ui_nlabpospie3"))
                                )

                        ),

                        # plot
                        column(8,

                            plotOutput('pie3_4', '500px')

                        )

                    ),

                     # variable selection
                    tabPanel('Title',

                        # user interface
                        column(4,

                            column(6,

                                # title
                                textInput('pie3_title', 'Title', value = 'title'),

                                # title color
                                textInput('pie3_titlecol', 'Title Color', value = 'black')

                            ),

                            column(6,

                                # title font
                                numericInput('pie3_font', 'Font', min = 1, step = 1, value = 1, max = 5),

                                # initial angle
                                numericInput('pie3_cex', 'Size', min = 0.1, step = 0.1, value = 1)

                            )

                        ),

                        # plot
                        column(8,

                            plotOutput('pie3_5', '500px')

                        )

                    ),

                    tabPanel('Plot',

                        fluidRow(

                            column(8, offset = 2,

                                plotOutput('pie3_final', '600px')

                            )

                        )

                    )

                )

            )

        )

    )

)
