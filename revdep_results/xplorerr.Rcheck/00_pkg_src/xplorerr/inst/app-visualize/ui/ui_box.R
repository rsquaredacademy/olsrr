tabPanel('Box Plot', value = 'tab_box',

    fluidPage(
      fluidRow(
                 column(12, align = 'left',
                   h4('Box Plot')
                 )
               ),
               hr(),

        fluidRow(

            column(12,

                tabsetPanel(type = 'tabs',

                    # variable selection
                    tabPanel('Variables',

                        # user interface
                        column(6,

                            column(6, align = 'center',

                                # select variable
                                selectInput('ubox_select', 'Select Variable: ',
                                                   choices = "", selected = ""
                                ),

                                # X axes label
                                textInput(inputId = "ubox_xlabel", label = "X Axes Label", value = "Label"),

                                # color
                                textInput(inputId = "ubox_colour", label = "Color", value = "blue")

                            ),

                            column(6, align = 'center',

                                # plot title
                                textInput(inputId = "ubox_title", label = "Title", value = "Title"),

                                # Y axes label
                                textInput(inputId = "ubox_ylabel", label = "Y Axes Label", value = "Label"),

                                # border color
                                textInput(inputId = "ubox_borcolour", label = "Border Color", value = "black")

                            )


                        ),

                        # plot
                        column(6,

                            plotOutput('ubox_plot_1')

                        )

                    ),

                    # box chart options
                    tabPanel('Box Options',

                        # user interface
                        column(6,

                            column(6, align = 'center',

                                # horizontal
                                selectInput('ubox_horiz', 'Horizontal',
                                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                    selected = "FALSE"
                                ),

                                # range
                                numericInput('ubox_range', 'Range', min = 0, value = 1.5, step = 0.5),

                                # varwidth
                                selectInput('ubox_varwidth', 'Varwidth',
                                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                    selected = "FALSE"
                                ),

                                # notch
                                selectInput('ubox_notch', 'Notch',
                                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                    selected = "FALSE"
                                ),

                                # outline
                                selectInput('ubox_outline', 'Outline',
                                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                    selected = "TRUE"
                                )

                            )

                        ),

                        # plot
                        column(6,

                            plotOutput('ubox_plot_2')

                        )

                    ),

                    tabPanel('Text',
                      column(6,
                        tabsetPanel(type = 'tabs',
                          tabPanel('Text (Inside Plot)',
                            flowLayout(

                                # x axis location
                                numericInput(
                                    inputId = "ubox_text_x_loc",
                                    label = "X Intercept: ",
                                    value = 1, step = 1
                                ),

                                # y axis location
                                numericInput(
                                    inputId = "ubox_text_y_loc",
                                    label = "Y Intercept: ",
                                    value = 1, step = 1
                                ),

                                # text
                                textInput(inputId = "ubox_plottext", label = "Text:", value = ""),

                                # text font
                                numericInput(
                                    inputId = "ubox_textfont",
                                    label = "Text Font: ",
                                    value = 1, min = 1, max = 5, step = 1
                                ),

                                # text color
                                textInput(
                                    inputId = "ubox_textcolor",
                                    label = "Text Color: ",
                                    value = "black"
                                ),

                                # text size
                                numericInput(
                                    inputId = "ubox_textsize",
                                    label = "Text Size: ",
                                    value = 1, min = 0.1, step = 0.1
                                )
                            )
                          ),

                          tabPanel('Marginal Text',
                            flowLayout(

                                # text
                                textInput(inputId = "ubox_mtextplot", label = "Text:", value = ""),

                                # text side
                                numericInput(
                                    inputId = "ubox_mtext_side",
                                    label = "Side: ",
                                    value = 1, min = 1, max = 4, step = 1
                                ),

                                # text line
                                numericInput(
                                    inputId = "ubox_mtext_line",
                                    label = "Line: ",
                                    value = 1, step = 1
                                ),

                                # text adjustment
                                numericInput(
                                    inputId = "ubox_mtextadj",
                                    label = "Adj: ",
                                    value = 0.5, min = 0, max = 1, step = 0.1
                                ),

                                # text font
                                numericInput(
                                    inputId = "ubox_mtextfont",
                                    label = "Text Font: ",
                                    value = 1, min = 1, max = 5, step = 1
                                ),

                                # text color
                                textInput(
                                    inputId = "ubox_mtextcolor",
                                    label = "Text Color: ",
                                    value = "black"
                                ),

                                # text size
                                numericInput(
                                    inputId = "ubox_mtextsize",
                                    label = "Text Size: ",
                                    value = 1, min = 0.1, step = 0.1
                                )
                            )
                          )
                        )
                      ),
                      column(6,
                            plotOutput('ubox_plot_3')
                      )
                    ),

                    # graphical parameters
                    tabPanel('Others',
                        column(6,
                          tabsetPanel(type = 'tabs',
                            tabPanel('Color',
                              flowLayout(
                                  textInput('ubox_colaxis', 'Axis Color: ', 'black'),
                                  textInput('ubox_coltitle', 'Title Color: ', 'black'),
                                  textInput('ubox_collabel', 'Label Color: ', 'black'),
                                  textInput('ubox_colsub', 'Subtitle Color: ', 'black')
                              )
                            ),
                            tabPanel('Size',
                              flowLayout(
                                  numericInput('ubox_cexmain', 'Title Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  ),
                                  numericInput('ubox_cexsub', 'Subtitle Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  ),
                                  numericInput('ubox_cexaxis', 'Axis Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  ),
                                  numericInput('ubox_cexlab', 'Label Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  )
                              )
                            ),
                            tabPanel('Font',
                              flowLayout(
                                  numericInput('ubox_fontmain', 'Title Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  ),
                                  numericInput('ubox_fontsub', 'Subtitle Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  ),
                                  numericInput('ubox_fontaxis', 'Axis Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  ),
                                  numericInput('ubox_fontlab', 'Label Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  )
                              )
                            )
                          )
                        ),
                        column(6,
                            plotOutput('ubox_plot_4')
                        )
                      ),

                    # final plot
                    tabPanel('Plot',
                        fluidRow(
                            column(8, offset = 2,
                                plotOutput('ubox_plot_final', height = '500px')
                            )
                        )

                    )

                )

            )

        )

    )

)
