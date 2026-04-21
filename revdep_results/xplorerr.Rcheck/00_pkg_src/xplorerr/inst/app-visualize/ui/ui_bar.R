tabPanel('Bar Plot', value = 'tab_bar',

    fluidPage(
      fluidRow(
                 column(12, align = 'left',
                   h4('Bar Plot')
                 )
               ),
               hr(),

                    fluidRow(

                        column(12,

                                tabsetPanel(type = "tabs",

                                    # variables and title
                                    tabPanel("Variables",

                                        column(6,

                                            # column one begins here
                                            column(6, align = 'center',

                                                br(), br(),

                                                # select variable
                                                selectInput('ubar_select', 'Select Variable: ', width = '150px',
                                                            choices = "", selected = ""
                                                ),

                                                # plot title
                                                textInput(inputId = "ubar_title", label = "Title", value = "",  width = '150px'),

                                                # X axes label
                                                textInput(inputId = "ubar_xlabel", label = "X Axes Label", value = "",  width = '150px'),


                                                tags$br(),

                                                # # file name for download
                                                # textInput(inputId = "ubar_fileName", label = "Plot Name (Download)", value = "",  width = '120px',),

                                                # # plot download button
                                                # downloadButton("ubar_downloadGraph", "Download"),

                                                tags$br()

                                            ),  # column one ends here

                                            # column two begins here
                                            column(6, align = 'center',

                                                br(), br(),

                                                # horizontal
                                                selectInput('ubar_horiz', 'Horizontal',
                                                            choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                            selected = "FALSE",  width = '150px'
                                                ),

                                                # bar space
                                                    numericInput('ubar_barspace', 'Bar Space: ',  width = '150px',
                                                                 value = 1, min = 0.1, step = 0.1
                                                    ),

                                                # Y axes label
                                                textInput(inputId = "ubar_ylabel", label = "Y Axes Label", value = "",  width = '150px')


                                            )

                                        ),

                                        column(6,

                                            plotOutput('ubar_plot_1', height = '400px')
                                        )

                                    ),

                                    # color, width, sharing and axis options
                                    tabPanel("Bar Options",
                                      column(6,
                                        tabsetPanel(type = 'tabs',
                                          tabPanel('Bar Width',
                                            column(4, numericInput("nbarwidth", "Number of Bars", value = 1, min = 0)),
                                            column(4, uiOutput("ui_nbarwidth"))
                                          ),
                                          tabPanel('Border Color',
                                            column(4, numericInput("nborbar", "Number of Border Colors", value = 0, min = 0)),
                                            column(4, uiOutput("ui_nborbar"))
                                          ),
                                          tabPanel('Bar Label',
                                            column(6, numericInput("nbarlabel", "Number of Labels", value = 0, min = 0)),
                                            column(6, uiOutput("ui_nbarlabel"))
                                          ),
                                          tabPanel('Bar Color',
                                            column(4, numericInput("ncolbar", "Number of Colors", value = 0, min = 0)),
                                            column(4, uiOutput("ui_ncolbar"))
                                          )
                                        )
                                      ),

                                      column(6,
                                        plotOutput('ubar_plot_2', height = '400px')
                                      )
                                    ),

                                    # legend options
                                    tabPanel("Axis",
                                        column(6,
                                          tabsetPanel(type = 'tabs',
                                            tabPanel('Axes',
                                              column(4,
                                                selectInput('ubar_axes', 'Axes',
                                                            choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                            selected = "TRUE"),
                                                  numericInput('ubar_axislty', 'Line Type: ', value = 0, min = 0),
                                                  numericInput('ubar_offset', 'Offset: ', value = 0, min = 0)
                                              )
                                            )
                                          )
                                        ),
                                        column(6,
                                            plotOutput('ubar_plot_3', height = '400px')
                                        )
                                    ),

                                    # legend options
                                    tabPanel("Legend",
                                        column(6,
                                          tabsetPanel(type = 'tabs',
                                            tabPanel('General',
                                              br(),
                                              flowLayout(
                                                selectInput('leg_yn', 'Create Legend',
                                                        choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                        selected = "FALSE")
                                                ),
                                                br(),
                                                h4('Axis Position', align = 'left', style = 'color:black'),
                                              flowLayout(
                                                numericInput('leg_x', 'X Axis:', value = 1),
                                                numericInput('leg_y', 'Y Axis:', value = 1)
                                              ),
                                              br(),
                                              h4('Legend Names', align = 'left', style = 'color:black'),
                                              flowLayout(
                                                numericInput('leg_names', 'Select:', value = 1, min = 1, step = 1),
                                                uiOutput('ui_legnames')
                                              ),
                                              br(),
                                              h4('Points', align = 'left', style = 'color:black'),
                                              flowLayout(
                                                numericInput('leg_point', 'Select:', value = 1, min = 0, max = 25, step = 1),
                                                uiOutput('ui_legpoint')
                                              )
                                            ),
                                            tabPanel('Legend Box',
                                              flowLayout(
                                                  selectInput('leg_boxtype', 'Box Type', choices = c('o', 'n'), selected = 'o'),
                                                  textInput('leg_boxcol', 'Box Color', value = 'white'),
                                                  numericInput('leg_boxlty', 'Box Border Line', value = 1, min = 1, max = 6, step = 1),
                                                  numericInput('leg_boxlwd', 'Box Border Width', value = 1, min = 0, step = 0.1),
                                                  textInput('leg_boxborcol', 'Box Border Color', value = 'black'),
                                                  numericInput('leg_boxxjust', 'Horizontal Justification', value = 0, min = 0, max = 1),
                                                  numericInput('leg_boxyjust', 'Vertical Justification', value = 1, min = 0, max = 1)
                                              )
                                            ),
                                            tabPanel('Legend Text',
                                              flowLayout(
                                                  selectInput('leg_texthoriz', 'Horizontal Text', choices = c(TRUE, FALSE), selected = FALSE),
                                                  textInput('leg_textcol', 'Text Color', value = 'black'),
                                                  textInput('leg_title', 'Title', value = ''),
                                                  numericInput('leg_textfont', 'Text Font', value = 1, min = 1, max = 5, step = 1),
                                                  numericInput('leg_textcolumns', 'Columns', value = 1, min = 0, step = 1),
                                                  textInput('leg_titlecol', 'Title Color', value = 'black'),
                                                  numericInput('leg_textadj', 'Text Horizontal Adj', value = 0.5, min = 0, max = 1, step = 0.1)
                                              )
                                            )
                                          )
                                        ),

                                        column(6,
                                            plotOutput('ubar_plot_4', height = '400px')
                                        )
                                    ),

                                    # graphical parameters
                                    tabPanel("Others",
                                        column(6,
                                          tabsetPanel(type = 'tabs',
                                            tabPanel('Color',
                                              flowLayout(
                                                  textInput('ubar_colaxis', 'Axis Color: ', 'black'),
                                                  textInput('ubar_coltitle', 'Title Color: ', 'black'),
                                                  textInput('ubar_collabel', 'Label Color: ', 'black'),
                                                  textInput('ubar_colsub', 'Subtitle Color: ', 'black')
                                              )
                                            ),
                                            tabPanel('Size',
                                              flowLayout(
                                                  numericInput('ubar_cexmain', 'Title Size: ',
                                                               value = 1, min = 0.1, step = 0.1
                                                  ),
                                                  numericInput('ubar_cexsub', 'Subtitle Size: ',
                                                               value = 1, min = 0.1, step = 0.1
                                                  ),
                                                  numericInput('ubar_cexaxis', 'Axis Size: ',
                                                               value = 1, min = 0.1, step = 0.1
                                                  ),
                                                  numericInput('ubar_cexlab', 'Label Size: ',
                                                               value = 1, min = 0.1, step = 0.1
                                                  )
                                              )
                                            ),
                                            tabPanel('Font',
                                              flowLayout(
                                                  numericInput('ubar_fontmain', 'Title Font',
                                                               value = 1, min = 1, max = 5, step = 1
                                                  ),
                                                  numericInput('ubar_fontsub', 'Subtitle Font',
                                                               value = 1, min = 1, max = 5, step = 1
                                                  ),
                                                  numericInput('ubar_fontaxis', 'Axis Font',
                                                               value = 1, min = 1, max = 5, step = 1
                                                  ),
                                                  numericInput('ubar_fontlab', 'Label Font',
                                                               value = 1, min = 1, max = 5, step = 1
                                                  )
                                              )
                                            ),
                                            tabPanel('Text',
                                              flowLayout(
                                                  numericInput(
                                                      inputId = "ubar_text_x_loc",
                                                      label = "X Intercept: ",
                                                      value = 1, step = 1
                                                  ),
                                                  numericInput(
                                                      inputId = "ubar_text_y_loc",
                                                      label = "Y Intercept: ",
                                                      value = 1, step = 1
                                                  ),
                                                  textInput(inputId = "ubar_plottext", label = "Text:", value = ""),
                                                  numericInput(
                                                      inputId = "ubar_textfont",
                                                      label = "Text Font: ",
                                                      value = 1, min = 1, max = 5, step = 1
                                                  ),
                                                  textInput(
                                                      inputId = "ubar_textcolor",
                                                      label = "Text Color: ",
                                                      value = "black"
                                                  ),
                                                  numericInput(
                                                      inputId = "ubar_textsize",
                                                      label = "Text Size: ",
                                                      value = 1, min = 0.1, step = 0.1
                                                  )
                                              )
                                            ),
                                            tabPanel('Marginal Text',
                                              flowLayout(
                                                  textInput(inputId = "ubar_mtextplot", label = "Text:", value = ""),
                                                  numericInput(
                                                      inputId = "ubar_mtext_side",
                                                      label = "Side: ",
                                                      value = 1, min = 1, max = 4, step = 1
                                                  ),
                                                  numericInput(
                                                      inputId = "ubar_mtext_line",
                                                      label = "Line: ",
                                                      value = 1, step = 1
                                                  ),
                                                  numericInput(
                                                      inputId = "ubar_mtextadj",
                                                      label = "Adj: ",
                                                      value = 0.5, min = 0, max = 1, step = 0.1
                                                  ),
                                                  numericInput(
                                                      inputId = "ubar_mtextfont",
                                                      label = "Text Font: ",
                                                      value = 1, min = 1, max = 5, step = 1
                                                  ),
                                                  textInput(
                                                      inputId = "ubar_mtextcolor",
                                                      label = "Text Color: ",
                                                      value = "black"
                                                  ),
                                                  numericInput(
                                                      inputId = "ubar_mtextsize",
                                                      label = "Text Size: ",
                                                      value = 1, min = 0.1, step = 0.1
                                                  )
                                              )
                                            )
                                          )
                                        ),
                                        column(6,
                                            plotOutput('ubar_plot_5', height = '400px')
                                        )
                                    ),

                                    # plot
                                    tabPanel("Plot",

                                        fluidRow(

                                            column(8, offset = 2,
                                                plotOutput('ubar_plot_final', height = '500px')
                                            )

                                        )
                                    )
                                )
                        )

                    )
                )

)
