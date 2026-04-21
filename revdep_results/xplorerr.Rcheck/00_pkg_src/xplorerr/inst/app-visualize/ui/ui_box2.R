tabPanel('2 Factor Box Plot', value = 'tab_box2',

    fluidPage(
      fluidRow(
                 column(12, align = 'left',
                   h4('2 Factor Box Plot')
                 )
               ),
               hr(),

        fluidRow(

            column(12,

                tabsetPanel(type = 'tabs',

                    # variable selection
                    tabPanel('Variables',

                        # user interface
                        column(4,

                            column(6, align = 'center',

                                # select variable
                                selectInput('bbox_select_x', 'Select Variable: ',
                                                   choices = "", selected = ""
                                ),

                                # plot title
                                textInput(inputId = "bbox_title", label = "Title", value = "Title"),

                                # X axes label
                                textInput(inputId = "bbox_xlabel", label = "X Axes Label", value = "Label")

                            ),

                            column(6, align = 'center',

                                # select variable
                                selectInput('bbox_select_y', 'Select Variable: ',
                                                   choices = "", selected = ""
                                ),

                                # plot title
                                textInput(inputId = "bbox_subtitle", label = "Subtitle", value = "Subtitle"),

                                # Y axes label
                                textInput(inputId = "bbox_ylabel", label = "Y Axes Label", value = "Label")

                            ),

                            br(),
                            br(),

                            column(12, align = 'center',
                              actionButton(inputId = 'box2_create', label = 'Generate Plot')
                            )


                        ),

                        # plot
                        column(8,

                            plotOutput('bbox_plot_1')

                        )

                    ),

                    # box chart options
                    tabPanel('Box Options',

                        # user interface
                        column(4,

                            column(6,

                                # horizontal
                                selectInput('bbox_horiz', 'Horizontal',
                                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                    selected = "FALSE"
                                ),

                                # range
                                numericInput('bbox_range', 'Range', min = 0, value = 1.5, step = 0.5),

                                # varwidth
                                selectInput('bbox_varwidth', 'Varwidth',
                                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                    selected = "FALSE"
                                ),

                                # notch
                                selectInput('bbox_notch', 'Notch',
                                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                    selected = "FALSE"
                                ),

                                # outline
                                selectInput('bbox_outline', 'Outline',
                                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                    selected = "TRUE"
                                )

                            )

                        ),

                        # plot
                        column(8,

                            plotOutput('bbox_plot_2')

                        )

                    ),

                    tabPanel('Color & Labels',
                      column(4,
                        tabsetPanel(type = 'tabs',
                          tabPanel('Color',
                            column(6, numericInput("ncolbox2", "Number of Colors", value = 1, min = 1)),
                            column(6, uiOutput("ui_ncolbox2"))
                          ),
                          tabPanel('Border',
                            column(6, numericInput("nborbox2", "Number of Border Colors", value = 1, min = 1)),
                            column(6, uiOutput("ui_nborbox2"))
                          ),
                          tabPanel('Label',
                            column(6, numericInput("nbox2label", "Number of Labels", value = 1, min = 1)),
                            column(6, uiOutput("ui_nbox2label"))
                          )
                        )
                      ),
                      column(8,
                        plotOutput('bbox_plot_3')
                      )
                    ),

                    # tabPanel("Legend",
                    #     column(4,
                    #       tabsetPanel(type = 'tabs',
                    #         tabPanel('General',
                    #           br(),
                    #           flowLayout(
                    #             selectInput('box2_leg_yn', 'Create Legend',
                    #                     choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                    #                     selected = "FALSE")
                    #             ),
                    #             br(),
                    #             h4('Axis Position', align = 'left', style = 'color:black'),
                    #           flowLayout(
                    #             numericInput('box2_leg_x', 'X Axis:', value = 1),
                    #             numericInput('box2_leg_y', 'Y Axis:', value = 1)
                    #           ),
                    #           br(),
                    #           h4('Legend Names', align = 'left', style = 'color:black'),
                    #           flowLayout(
                    #             numericInput('box2_legnames', 'Select:', value = 0, min = 0, step = 1),
                    #             uiOutput('ui_box2_legnames')
                    #           ),
                    #           br(),
                    #           h4('Points', align = 'left', style = 'color:black'),
                    #           flowLayout(
                    #             numericInput('box2_leg_point', 'Select:', value = 0, min = 0, max = 25, step = 1),
                    #             uiOutput('ui_box2_legpoint')
                    #           )
                    #         ),
                    #         tabPanel('Legend Box',
                    #           flowLayout(
                    #               selectInput('box2_leg_boxtype', 'Box Type', choices = c('o', 'n'), selected = 'o'),
                    #               textInput('box2_leg_boxcol', 'Box Color', value = 'white'),
                    #               numericInput('box2_leg_boxlty', 'Box Border Line', value = 1, min = 1, max = 6, step = 1),
                    #               numericInput('box2_leg_boxlwd', 'Box Border Width', value = 1, min = 0, step = 0.1),
                    #               textInput('box2_leg_boxborcol', 'Box Border Color', value = 'black'),
                    #               numericInput('box2_leg_boxxjust', 'Horizontal Justification', value = 0, min = 0, max = 1),
                    #               numericInput('box2_leg_boxyjust', 'Vertical Justification', value = 1, min = 0, max = 1)
                    #           )
                    #         ),
                    #         tabPanel('Legend Text',
                    #           flowLayout(
                    #               selectInput('box2_leg_texthoriz', 'Horizontal Text', choices = c(TRUE, FALSE), selected = FALSE),
                    #               textInput('box2_leg_textcol', 'Text Color', value = 'black'),
                    #               textInput('box2_leg_title', 'Title', value = ''),
                    #               numericInput('box2_leg_textfont', 'Text Font', value = 1, min = 1, max = 5, step = 1),
                    #               numericInput('box2_leg_textcolumns', 'Columns', value = 1, min = 0, step = 1),
                    #               textInput('box2_leg_titlecol', 'Title Color', value = 'black'),
                    #               numericInput('box2_leg_textadj', 'Text Horizontal Adj', value = 0.5, min = 0, max = 1, step = 0.1)
                    #           )
                    #         )
                    #       )
                    #     ),

                    #     column(8,
                    #         plotOutput('bbox_plot_4', height = '400px')
                    #     )
                    # ),

                    tabPanel('Text',
                      column(4,
                        tabsetPanel(type = 'tabs',
                          tabPanel('Text (Inside Plot)',
                            flowLayout(

                                # x axis location
                                numericInput(
                                    inputId = "bbox_text_x_loc",
                                    label = "X Intercept: ",
                                    value = 1, step = 1
                                ),

                                # y axis location
                                numericInput(
                                    inputId = "bbox_text_y_loc",
                                    label = "Y Intercept: ",
                                    value = 1, step = 1
                                ),

                                # text
                                textInput(inputId = "bbox_plottext", label = "Text:", value = ""),

                                # text font
                                numericInput(
                                    inputId = "bbox_textfont",
                                    label = "Text Font: ",
                                    value = 1, min = 1, max = 5, step = 1
                                ),

                                # text color
                                textInput(
                                    inputId = "bbox_textcolor",
                                    label = "Text Color: ",
                                    value = "black"
                                ),

                                # text size
                                numericInput(
                                    inputId = "bbox_textsize",
                                    label = "Text Size: ",
                                    value = 1, min = 0.1, step = 0.1
                                )
                            )
                          ),

                          tabPanel('Marginal Text',
                            flowLayout(

                                # text
                                textInput(inputId = "bbox_mtextplot", label = "Text:", value = ""),

                                # text side
                                numericInput(
                                    inputId = "bbox_mtext_side",
                                    label = "Side: ",
                                    value = 1, min = 1, max = 4, step = 1
                                ),

                                # text line
                                numericInput(
                                    inputId = "bbox_mtext_line",
                                    label = "Line: ",
                                    value = 1, step = 1
                                ),

                                # text adjustment
                                numericInput(
                                    inputId = "bbox_mtextadj",
                                    label = "Adj: ",
                                    value = 0.5, min = 0, max = 1, step = 0.1
                                ),

                                # text font
                                numericInput(
                                    inputId = "bbox_mtextfont",
                                    label = "Text Font: ",
                                    value = 1, min = 1, max = 5, step = 1
                                ),

                                # text color
                                textInput(
                                    inputId = "bbox_mtextcolor",
                                    label = "Text Color: ",
                                    value = "black"
                                ),

                                # text size
                                numericInput(
                                    inputId = "bbox_mtextsize",
                                    label = "Text Size: ",
                                    value = 1, min = 0.1, step = 0.1
                                )
                            )
                          )
                        )
                      ),
                      column(8,
                            plotOutput('bbox_plot_5')
                      )
                    ),

                    tabPanel('Others',
                        column(4,
                          tabsetPanel(type = 'tabs',
                            tabPanel('Color',
                              flowLayout(
                                  textInput('bbox_colaxis', 'Axis Color: ', 'black'),
                                  textInput('bbox_coltitle', 'Title Color: ', 'black'),
                                  textInput('bbox_collabel', 'Label Color: ', 'black'),
                                  textInput('bbox_colsub', 'Subtitle Color: ', 'black')
                              )
                            ),
                            tabPanel('Size',
                              flowLayout(
                                  numericInput('bbox_cexmain', 'Title Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  ),
                                  numericInput('bbox_cexsub', 'Subtitle Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  ),
                                  numericInput('bbox_cexaxis', 'Axis Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  ),
                                  numericInput('bbox_cexlab', 'Label Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  )
                              )
                            ),
                            tabPanel('Font',
                              flowLayout(
                                  numericInput('bbox_fontmain', 'Title Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  ),
                                  numericInput('bbox_fontsub', 'Subtitle Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  ),
                                  numericInput('bbox_fontaxis', 'Axis Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  ),
                                  numericInput('bbox_fontlab', 'Label Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  )
                              )
                            )
                          )
                        ),
                        column(8,
                            plotOutput('bbox_plot_6')
                        )
                      ),

                    # final plot
                    tabPanel('Plot',
                      fluidRow(
                        column(8, offset = 2,
                          plotOutput('bbox_plot_final', height = '500px')
                          )
                      )

                    )

                )

            )

        )

    )
)
