tabPanel('2 Factor Bar Plot', value = 'tab_bar2',

		fluidPage(
			fluidRow(
                 column(12, align = 'left',
                   h4('2 Factor Bar Plot')
                 )
               ),
               hr(),

            fluidRow(

                column(12,

                    tabsetPanel(type = "tabs",

                        tabPanel('Variables',

                            # user interface
                            column(6,

                                column(6,

                                    # select variable
                                    selectInput('bar2_select_x', 'Variable 1: ',
                                                choices = "", selected = ""
                                    ),

                                    # horizontal
                                    selectInput('bar2_horiz', 'Horizontal',
                                                choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                                                selected = "FALSE"
                                    ),

                                    # X axes label
                                    textInput(inputId = "bar2_xlabel", label = "X Axes Label", value = ""),


                                    # plot title
                                    textInput(inputId = "bar2_title", label = "Title", value = "")


                                ),

                                column(6,

                                    # select variable
                                    selectInput('bar2_select_y', 'Variable 2: ',
                                                   choices = "", selected = ""
                                    ),

                                    # select variable
                                    selectInput('bar2_beside', 'Grouped',
                                                   choices = c("TRUE" = TRUE, "FALSE" = FALSE), selected = "TRUE"
                                    ),

                                    # Y axes label
                                    textInput(inputId = "bar2_ylabel", label = "Y Axes Label", value = "")

                                )

                            ),

                            # plot
                            column(6,

                                plotOutput('bbar_plot_1')
                            )

                        ),

												tabPanel("Bar Options",
													column(6,
														tabsetPanel(type = 'tabs',
															tabPanel('Bar Width',
																column(4, numericInput("nbarwidth2", "Number of Bars", value = 1, min = 0)),
																column(4, uiOutput("ui_nbarwidth2"))
															),
															tabPanel('Border Color',
																column(4, numericInput("nborbar2", "Number of Border Colors", value = 0, min = 0)),
																column(4, uiOutput("ui_nborbar2"))
															),
															tabPanel('Bar Label',
																column(4, numericInput("nbarlabel2", "Number of Labels", value = 0, min = 0)),
																column(6, uiOutput("ui_nbarlabel2"))
															),
															tabPanel('Bar Color',
																column(4, numericInput("ncolbar2", "Number of Colors", value = 0, min = 0)),
																column(4, uiOutput("ui_ncolbar2"))
															)
														)
													),

													column(6,
														plotOutput('bbar_plot_2', height = '400px')
													)
												),

												tabPanel("Shading & Axis Options",
														column(6,
															tabsetPanel(type = 'tabs',
																tabPanel('Axes',
																	column(4,
																		selectInput('bar2_axes', 'Axes',
																								choices = c("TRUE" = TRUE, "FALSE" = FALSE),
																								selected = "TRUE"),
																			numericInput('bar2_axislty', 'Line Type: ', value = 0, min = 0),
																			numericInput('bar2_offset', 'Offset: ', value = 0, min = 0)
																	)
																)
															)
														),
														column(6,
																plotOutput('bbar_plot_3', height = '400px')
														)
												),
												tabPanel("Legend",
														column(6,
															tabsetPanel(type = 'tabs',
																tabPanel('General',
																	br(),
																	flowLayout(
																		selectInput('bar2_leg_yn', 'Create Legend',
																						choices = c("TRUE" = TRUE, "FALSE" = FALSE),
																						selected = "FALSE")
																		),
																		br(),
																		h4('Axis Position', align = 'left', style = 'color:black'),
																	flowLayout(
																		numericInput('bar2_leg_x', 'X Axis:', value = 1),
																		numericInput('bar2_leg_y', 'Y Axis:', value = 1)
																	),
																	br(),
																	h4('Legend Names', align = 'left', style = 'color:black'),
																	flowLayout(
																		numericInput('bar2_legnames', 'Select:', value = 0, min = 0, step = 1),
																		uiOutput('ui_bar2_legnames')
																	),
																	br(),
																	h4('Points', align = 'left', style = 'color:black'),
																	flowLayout(
																		numericInput('bar2_leg_point', 'Select:', value = 0, min = 0, max = 25, step = 1),
																		uiOutput('ui_bar2_legpoint')
																	)
																),
																tabPanel('Legend Box',
																	flowLayout(
																			selectInput('bar2_leg_boxtype', 'Box Type', choices = c('o', 'n'), selected = 'o'),
																			textInput('bar2_leg_boxcol', 'Box Color', value = 'white'),
																			numericInput('bar2_leg_boxlty', 'Box Border Line', value = 1, min = 1, max = 6, step = 1),
																			numericInput('bar2_leg_boxlwd', 'Box Border Width', value = 1, min = 0, step = 0.1),
																			textInput('bar2_leg_boxborcol', 'Box Border Color', value = 'black'),
																			numericInput('bar2_leg_boxxjust', 'Horizontal Justification', value = 0, min = 0, max = 1),
																			numericInput('bar2_leg_boxyjust', 'Vertical Justification', value = 1, min = 0, max = 1)
																	)
																),
																tabPanel('Legend Text',
																	flowLayout(
																			selectInput('bar2_leg_texthoriz', 'Horizontal Text', choices = c(TRUE, FALSE), selected = FALSE),
																			textInput('bar2_leg_textcol', 'Text Color', value = 'black'),
																			textInput('bar2_leg_title', 'Title', value = ''),
																			numericInput('bar2_leg_textfont', 'Text Font', value = 1, min = 1, max = 5, step = 1),
																			numericInput('bar2_leg_textcolumns', 'Columns', value = 1, min = 0, step = 1),
																			textInput('bar2_leg_titlecol', 'Title Color', value = 'black'),
																			numericInput('bar2_leg_textadj', 'Text Horizontal Adj', value = 0.5, min = 0, max = 1, step = 0.1)
																	)
																)
															)
														),

														column(6,
																plotOutput('bbar_plot_4', height = '400px')
														)
												),

												tabPanel("Others",
														column(6,
															tabsetPanel(type = 'tabs',
																tabPanel('Color',
																	flowLayout(
																			textInput('bar2_colaxis', 'Axis Color: ', 'black'),
																			textInput('bar2_coltitle', 'Title Color: ', 'black'),
																			textInput('bar2_collabel', 'Label Color: ', 'black')
																	)
																),
																tabPanel('Size',
																	flowLayout(
																			numericInput('bar2_cexmain', 'Title Size: ',
																									 value = 1, min = 0.1, step = 0.1
																			),
																			numericInput('bar2_cexaxis', 'Axis Size: ',
																									 value = 1, min = 0.1, step = 0.1
																			),
																			numericInput('bar2_cexlab', 'Label Size: ',
																									 value = 1, min = 0.1, step = 0.1
																			)
																	)
																),
																tabPanel('Font',
																	flowLayout(
																			numericInput('bar2_fontmain', 'Title Font',
																									 value = 1, min = 1, max = 5, step = 1
																			),
																			numericInput('bar2_fontaxis', 'Axis Font',
																									 value = 1, min = 1, max = 5, step = 1
																			),
																			numericInput('bar2_fontlab', 'Label Font',
																									 value = 1, min = 1, max = 5, step = 1
																			)
																	)
																),
																tabPanel('Text',
																	flowLayout(
																			numericInput(
																					inputId = "bar2_text_x_loc",
																					label = "X Intercept: ",
																					value = 1, step = 1
																			),
																			numericInput(
																					inputId = "bar2_text_y_loc",
																					label = "Y Intercept: ",
																					value = 1, step = 1
																			),
																			textInput(inputId = "ubar_plottext", label = "Text:", value = ""),
																			numericInput(
																					inputId = "bar2_textfont",
																					label = "Text Font: ",
																					value = 1, min = 1, max = 5, step = 1
																			),
																			textInput(
																					inputId = "bar2_textcolor",
																					label = "Text Color: ",
																					value = "black"
																			),
																			numericInput(
																					inputId = "bar2_textsize",
																					label = "Text Size: ",
																					value = 1, min = 0.1, step = 0.1
																			)
																	)
																),
																tabPanel('Marginal Text',
																	flowLayout(
																			textInput(inputId = "bar2_mtextplot", label = "Text:", value = ""),
																			numericInput(
																					inputId = "bar2_mtext_side",
																					label = "Side: ",
																					value = 1, min = 1, max = 4, step = 1
																			),
																			numericInput(
																					inputId = "bar2_mtext_line",
																					label = "Line: ",
																					value = 1, step = 1
																			),
																			numericInput(
																					inputId = "bar2_mtextadj",
																					label = "Adj: ",
																					value = 0.5, min = 0, max = 1, step = 0.1
																			),
																			numericInput(
																					inputId = "bar2_mtextfont",
																					label = "Text Font: ",
																					value = 1, min = 1, max = 5, step = 1
																			),
																			textInput(
																					inputId = "bar2_mtextcolor",
																					label = "Text Color: ",
																					value = "black"
																			),
																			numericInput(
																					inputId = "bar2_mtextsize",
																					label = "Text Size: ",
																					value = 1, min = 0.1, step = 0.1
																			)
																	)
																)
															)
														),
														column(6,
																plotOutput('bbar_plot_5', height = '400px')
														)
												),

                        tabPanel('Plot',
                            fluidRow(
                                column(8, offset = 2,
                                    plotOutput('bbar_plot_final')
                                )
                            )
                        )

                    )

                )

            )

        )
)
