tabPanel('Histogram', value = 'tab_hist',

    fluidPage(
      fluidRow(
                 column(12, align = 'left',
                   h4('Histogram')
                 )
               ),
               hr(),

        fluidRow(

            column(12,

                tabsetPanel(type = 'tabs',

                    # variable
                    tabPanel('Variables',

                        # user interface
                        column(4,

                            column(6,

                                # select variable
                                selectInput('hist_select', 'Select Variable: ',
                                                   choices = "", selected = ""
                                ),

                                # density
                                selectInput('hist_frequency', 'Probability', choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                    selected = "FALSE"),

                                # X axes label
                                textInput(inputId = "hist_xlabel", label = "X Axes Label", value = ""),

                                # hide axes
                                selectInput('hist_hideaxes', 'Axes: ',
                                                   choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                    selected = "TRUE"
                                )

                            ),

                            column(6,

                                # plot title
                                textInput(inputId = "hist_title", label = "Title", value = ""),

                                # right closed intervals
                                selectInput('hist_interval', 'Intervals',
                                                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                    selected = "TRUE"
                                ),

                                # Y axes label
                                textInput(inputId = "hist_ylabel", label = "Y Axes Label", value = ""),

                                # show labels
                                selectInput('hist_showlabels', 'Labels ',
                                                   choices = c(TRUE, FALSE),
                                                    selected = FALSE
                                )

                            )

                        ),

                        # plot
                        column(8,

                            plotOutput('hist_1')

                        )

                    ),

                    # bins
                    tabPanel('Bins',
                      column(5,
                        tabsetPanel(type = 'tabs',
                          tabPanel('Options',
                            selectInput('bin_opt', 'Binning Options',
                                          choices = c(Bins = 'Bins',
                                          Intervals = 'Intervals',
                                          Algorithms = 'Algorithms')
                            )
                          ),
                          tabPanel('Bins',
                            numericInput("nbins", "Number of Bins", value = 5, min = 1)
                          ),
                          tabPanel('Intervals',
                            numericInput("bin_intervals", "Number of Intervals", value = 1, min = 1),
                            uiOutput("ui_nbin_intervals")
                          ),
                          tabPanel('Algorithms',
                            selectInput('alg_options', 'Binning Algorithms',
                                          choices = c(None = 'None',
                                          Sturges = 'Sturges', Scott = 'Scott',
                                          FD = 'FD'))
                          )
                        )
                      ),
                      column(7,
                        plotOutput('hist_2')
                      )
                    ),

                    tabPanel('Color & Shading',
                      column(4,
                        tabsetPanel(type = 'tabs',
                          tabPanel('Histogram',
                            column(6, numericInput("ncolhist", "Number of Colors", value = 0, min = 0)),
                            column(6, uiOutput("ui_ncolhist"))
                          ),
                          tabPanel('Border',
                            column(6, numericInput("nborhist", "Number of Border Colors", value = 0, min = 0)),
                            column(6, uiOutput("ui_nborhist"))
                          )
                        )
                      ),
                      column(8,
                        plotOutput('hist_3')
                      )
                    ),

                    tabPanel("Legend",
                        column(4,
                          tabsetPanel(type = 'tabs',
                            tabPanel('General',
                              br(),
                              flowLayout(
                                selectInput('hist_leg_yn', 'Create Legend',
                                        choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                        selected = "FALSE")
                                ),
                                br(),
                                h4('Axis Position', align = 'left', style = 'color:black'),
                              flowLayout(
                                numericInput('hist_leg_x', 'X Axis:', value = 1),
                                numericInput('hist_leg_y', 'Y Axis:', value = 1)
                              ),
                              br(),
                              h4('Legend Names', align = 'left', style = 'color:black'),
                              flowLayout(
                                numericInput('hist_leg_names', 'Select:', value = 1, min = 1, step = 1),
                                uiOutput('ui_hist_legnames')
                              ),
                              br(),
                              h4('Points', align = 'left', style = 'color:black'),
                              flowLayout(
                                numericInput('hist_leg_point', 'Select:', value = 1, min = 0, max = 25, step = 1),
                                uiOutput('ui_hist_legpoint')
                              )
                            ),
                            tabPanel('Legend Box',
                              flowLayout(
                                  selectInput('hist_leg_boxtype', 'Box Type', choices = c('o', 'n'), selected = 'o'),
                                  textInput('hist_leg_boxcol', 'Box Color', value = 'white'),
                                  numericInput('hist_leg_boxlty', 'Box Border Line', value = 1, min = 1, max = 6, step = 1),
                                  numericInput('hist_leg_boxlwd', 'Box Border Width', value = 1, min = 0, step = 0.1),
                                  textInput('hist_leg_boxborcol', 'Box Border Color', value = 'black'),
                                  numericInput('hist_leg_boxxjust', 'Horizontal Justification', value = 0, min = 0, max = 1),
                                  numericInput('hist_leg_boxyjust', 'Vertical Justification', value = 1, min = 0, max = 1)
                              )
                            ),
                            tabPanel('Legend Text',
                              flowLayout(
                                  selectInput('hist_leg_texthoriz', 'Horizontal Text', choices = c(TRUE, FALSE), selected = FALSE),
                                  textInput('hist_leg_textcol', 'Text Color', value = 'black'),
                                  textInput('hist_leg_title', 'Title', value = ''),
                                  numericInput('hist_leg_textfont', 'Text Font', value = 1, min = 1, max = 5, step = 1),
                                  numericInput('hist_leg_textcolumns', 'Columns', value = 1, min = 0, step = 1),
                                  textInput('hist_leg_titlecol', 'Title Color', value = 'black'),
                                  numericInput('hist_leg_textadj', 'Text Horizontal Adj', value = 0.5, min = 0, max = 1, step = 0.1)
                              )
                            )
                          )
                        ),

                        column(8,
                            plotOutput('hist_4', height = '400px')
                        )
                    ),

                    tabPanel('Text',
                      column(4,
                        tabsetPanel(type = 'tabs',
                          tabPanel('Text (Inside Plot)',
                            flowLayout(

                                # x axis location
                                numericInput(
                                    inputId = "hist_text_x_loc",
                                    label = "X Intercept: ",
                                    value = 1, step = 1
                                ),

                                # y axis location
                                numericInput(
                                    inputId = "hist_text_y_loc",
                                    label = "Y Intercept: ",
                                    value = 1, step = 1
                                ),

                                # text
                                textInput(inputId = "hist_plottext", label = "Text:", value = ""),

                                # text font
                                numericInput(
                                    inputId = "hist_textfont",
                                    label = "Text Font: ",
                                    value = 1, min = 1, max = 5, step = 1
                                ),

                                # text color
                                textInput(
                                    inputId = "hist_textcolor",
                                    label = "Text Color: ",
                                    value = "black"
                                ),

                                # text size
                                numericInput(
                                    inputId = "hist_textsize",
                                    label = "Text Size: ",
                                    value = 1, min = 0.1, step = 0.1
                                )
                            )
                          ),

                          tabPanel('Marginal Text',
                            flowLayout(

                                # text
                                textInput(inputId = "hist_mtextplot", label = "Text:", value = ""),

                                # text side
                                numericInput(
                                    inputId = "hist_mtext_side",
                                    label = "Side: ",
                                    value = 1, min = 1, max = 4, step = 1
                                ),

                                # text line
                                numericInput(
                                    inputId = "hist_mtext_line",
                                    label = "Line: ",
                                    value = 1, step = 1
                                ),

                                # text adjustment
                                numericInput(
                                    inputId = "hist_mtextadj",
                                    label = "Adj: ",
                                    value = 0.5, min = 0, max = 1, step = 0.1
                                ),

                                # text font
                                numericInput(
                                    inputId = "hist_mtextfont",
                                    label = "Text Font: ",
                                    value = 1, min = 1, max = 5, step = 1
                                ),

                                # text color
                                textInput(
                                    inputId = "hist_mtextcolor",
                                    label = "Text Color: ",
                                    value = "black"
                                ),

                                # text size
                                numericInput(
                                    inputId = "hist_mtextsize",
                                    label = "Text Size: ",
                                    value = 1, min = 0.1, step = 0.1
                                )
                            )
                          )
                        )
                      ),
                      column(8,
                            plotOutput('hist_5')
                      )
                    ),

                    tabPanel('Others',
                        column(6,
                          tabsetPanel(type = 'tabs',
                            tabPanel('Color',
                              flowLayout(
                                  textInput('hist_colaxis', 'Axis Color: ', 'black'),
                                  textInput('hist_coltitle', 'Title Color: ', 'black'),
                                  textInput('hist_collabel', 'Label Color: ', 'black'),
                                  textInput('hist_colsub', 'Subtitle Color: ', 'black')
                              )
                            ),
                            tabPanel('Size',
                              flowLayout(
                                  numericInput('hist_cexmain', 'Title Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  ),
                                  numericInput('hist_cexsub', 'Subtitle Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  ),
                                  numericInput('hist_cexaxis', 'Axis Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  ),
                                  numericInput('hist_cexlab', 'Label Size: ',
                                               value = 1, min = 0.1, step = 0.1
                                  )
                              )
                            ),
                            tabPanel('Font',
                              flowLayout(
                                  numericInput('hist_fontmain', 'Title Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  ),
                                  numericInput('hist_fontsub', 'Subtitle Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  ),
                                  numericInput('hist_fontaxis', 'Axis Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  ),
                                  numericInput('hist_fontlab', 'Label Font',
                                               value = 1, min = 1, max = 5, step = 1
                                  )
                              )
                            )
                          )
                        ),
                        column(6,
                            plotOutput('hist_6')
                        )
                      ),

                    # plot
                    tabPanel('Plot',
                        fluidRow(
                            column(8, offset = 2,
                                plotOutput('hist_final')
                            )
                        )
                    )

                  )

            )

        )

    )

)
