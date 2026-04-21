tabPanel('Line Graph', value = 'tab_line',
	fluidPage(
		fluidRow(
                 column(12, align = 'left',
                   h4('Line Chart')
                 )
               ),
               hr(),
		fluidRow(
			column(12,
				tabsetPanel(type = 'tabs',
					tabPanel('Variables',
						fluidRow(
							column(2,
								selectInput('line_select_x', 'X Axis Variable: ',
									choices = "", selected = ""),
								textInput('line_title', 'Title', value = ''),
								textInput('line_xlabel', 'X Axis Label', '')
							),
							column(2,
								selectInput('line_select_y', 'Y Axis Variable: ',
									choices = "", selected = ""),
								textInput('line_subtitle', 'Subitle', value = ''),
								textInput('line_ylabel', 'Y Axis Label', '')
							),
							column(8,
								plotOutput('line_plot_1')
							)
						)
					),
					tabPanel('Aesthetics',
						column(4,
							tabsetPanel(type = 'tabs',
								tabPanel('Line',
									column(6,
										numericInput('line_width', 'Line Width', value = 1,
											step = 0.1),
										textInput('line_color', 'Color', value = 'black'),
										numericInput('line_type', 'Line Type', min = 1, max = 5,
											value = 1, step = 1)
									)
								),
								tabPanel('Points',
									column(6,
										flowLayout(
											selectInput('add_points', 'Add Points',
												choices = c("TRUE" = TRUE, "FALSE" = FALSE),
												selected = "FALSE"),
											numericInput('point_shape', 'Shape', min = 0, max = 25,
												value = 1),
											numericInput('point_size', 'Size', min = 0.5, value = 1),
											textInput('point_col', 'Color', 'black'),
											textInput('point_bg', 'Background Color', 'red')
										)
									)
								)
							)
						),
						column(8,
							plotOutput('line_plot_2')
						)
					),
					tabPanel('Axis Range',
						column(2,
							uiOutput('ui_yrange_minl')
						),
						column(2,
							uiOutput('ui_yrange_maxl')
						),
						column(8,
							plotOutput('line_plot_3')
						)
					),
					tabPanel('Additional Lines',
						column(6,
							tabsetPanel(type = 'tabs',
								tabPanel('Add Lines',
									fluidRow(
										column(4,
										numericInput('n_lines', 'Lines', min = 0, value = 0,
												step = 1))
										),
										fluidRow(
											column(3, uiOutput('ui_nlines')),
											column(3, uiOutput('ui_ncolors')),
											column(3, uiOutput('ui_nltys')),
											column(3, uiOutput('ui_nlwds'))
										)
									)
								)
							),
							column(6,
								plotOutput('line_plot_4')
							)
						),

						tabPanel('Additional Points',
							column(6,
								tabsetPanel(type = 'tabs',
									tabPanel('Add Points',
										fluidRow(
											selectInput('extra_p', 'Add Points',
												choices = c("TRUE" = TRUE, "FALSE" = FALSE),
												selected = "FALSE")
										),
										fluidRow(
											column(3, uiOutput('ui_pcolors')),
											column(3, uiOutput('ui_pbgcolor')),
											column(3, uiOutput('ui_psize')),
											column(3, uiOutput('ui_pshape'))
										)
									)
								)
							),
							column(6,
								plotOutput('line_plot_5')
							)
						),

						tabPanel('Others',
	            column(4,
	              tabsetPanel(type = 'tabs',
	                tabPanel('Color',
	                  fluidRow(
	                    column(6,
	                      textInput('line_colaxis', 'Axis Color: ', 'black'),
	                      textInput('line_coltitle', 'Title Color: ', 'black')
	                    ),
	                    column(6,
	                      textInput('line_collabel', 'Label Color: ', 'black'),
	                      textInput('line_colsub', 'Subtitle Color: ', 'black')
	                    )
	                  )
	                ),
	                tabPanel('Size',
	                  fluidRow(
	                    column(6,
	                      numericInput('line_cexmain', 'Title Size: ',
	                                   value = 1, min = 0.1, step = 0.1),
	                      numericInput('line_cexsub', 'Subtitle Size: ',
	                                   value = 1, min = 0.1, step = 0.1)
	                    ),
	                    column(6,
	                      numericInput('line_cexaxis', 'Axis Size: ',
	                                   value = 1, min = 0.1, step = 0.1),
	                      numericInput('line_cexlab', 'Label Size: ',
	                                   value = 1, min = 0.1, step = 0.1)
	                    )
	                  )
	                ),
	                tabPanel('Font',
	                  fluidRow(
	                    column(6,
	                      numericInput('line_fontmain', 'Title Font',
	                                   value = 1, min = 1, max = 5, step = 1),
	                      numericInput('line_fontsub', 'Subtitle Font',
	                                   value = 1, min = 1, max = 5, step = 1)
	                    ),
	                    column(6,
	                      numericInput('line_fontaxis', 'Axis Font',
	                                   value = 1, min = 1, max = 5, step = 1),
	                      numericInput('line_fontlab', 'Label Font',
	                                   value = 1, min = 1, max = 5, step = 1)
	                    )
	                  )
	                )
	              )
	            ),
	            column(8,
	              plotOutput('line_plot_6')
	            )
	          ),
	          tabPanel('Text',
	            column(4,
	              tabsetPanel(type = 'tabs',
	                tabPanel('Text (Inside)',
	                  fluidRow(
	                    column(6,
	                      numericInput(
	                          inputId = "line_text_x_loc",
	                          label = "X Intercept: ",
	                          value = 1, step = 1),
	                      numericInput(
	                          inputId = "line_text_y_loc",
	                          label = "Y Intercept: ",
	                          value = 1, step = 1
	                      ),
	                      textInput(inputId = "line_plottext", label = "Text:",
	                        value = "")
	                    ),
	                    column(6,
	                      numericInput(
	                          inputId = "line_textfont",
	                          label = "Text Font: ",
	                          value = 1, min = 1, max = 5, step = 1
	                      ),
	                      numericInput(
	                          inputId = "line_textsize",
	                          label = "Text Size: ",
	                          value = 1, min = 0.1, step = 0.1
	                      ),
	                      textInput(
	                          inputId = "line_textcolor",
	                          label = "Text Color: ",
	                          value = "black"
	                      )
	                    )
	                  )
	                ),
	                tabPanel('Marginal Text',
	                  fluidRow(
	                    column(6,
	                      numericInput(
	                          inputId = "line_mtext_side",
	                          label = "Side: ",
	                          value = 1, min = 1, max = 4, step = 1
	                      ),
	                      numericInput(
	                          inputId = "line_mtext_line",
	                          label = "Line: ",
	                          value = 1, step = 1
	                      ),
	                      textInput(inputId = "line_mtextplot", label = "Text:", value = ""),
	                      numericInput(
	                          inputId = "line_mtextsize",
	                          label = "Text Size: ",
	                          value = 1, min = 0.1, step = 0.1
	                      )
	                    ),
	                    column(6,
	                      numericInput(
	                          inputId = "line_mtextadj",
	                          label = "Adj: ",
	                          value = 0.5, min = 0, max = 1, step = 0.1
	                      ),
	                      numericInput(
	                          inputId = "line_mtextfont",
	                          label = "Text Font: ",
	                          value = 1, min = 1, max = 5, step = 1
	                      ),
	                      textInput(
	                          inputId = "line_mtextcolor",
	                          label = "Text Color: ",
	                          value = "black"
	                      )
	                    )
	                  )
	                )
	              )
	            ),
	            column(8,
	              plotOutput('line_plot_7', height = '600px')
	            )
	          ),

						tabPanel("Legend",
								column(6,
									tabsetPanel(type = 'tabs',
										tabPanel('General',
											br(),
											flowLayout(
												selectInput('line_leg_yn', 'Create Legend',
																choices = c("TRUE" = TRUE, "FALSE" = FALSE),
																selected = "FALSE")
												),
												br(),
												h4('Axis Position', align = 'left', style = 'color:black'),
											flowLayout(
												numericInput('line_leg_x', 'X Axis:', value = 1),
												numericInput('line_leg_y', 'Y Axis:', value = 1)
											),
											br(),
											h4('Legend Names', align = 'left', style = 'color:black'),
											flowLayout(
												numericInput('line_legnames', 'Select:', value = 0, min = 0, step = 1),
												uiOutput('ui_line_legnames')
											),
											br(),
											h4('Lines', align = 'left', style = 'color:black'),
											flowLayout(
												numericInput('line_leg_line', 'Select:', value = 0, min = 1, max = 5, step = 1),
												uiOutput('ui_line_legline')
											),
											br(),
											h4('Points', align = 'left', style = 'color:black'),
											flowLayout(
												numericInput('line_leg_point', 'Select:', value = 0, min = 0, max = 25, step = 1),
												uiOutput('ui_line_legpoint')
											)
										),
										tabPanel('Legend Box',
											flowLayout(
													selectInput('line_leg_boxtype', 'Box Type', choices = c('o', 'n'), selected = 'o'),
													textInput('line_leg_boxcol', 'Box Color', value = 'white'),
													numericInput('line_leg_boxlty', 'Box Border Line', value = 1, min = 1, max = 6, step = 1),
													numericInput('line_leg_boxlwd', 'Box Border Width', value = 1, min = 0, step = 0.1),
													textInput('line_leg_boxborcol', 'Box Border Color', value = 'black'),
													numericInput('line_leg_boxxjust', 'Horizontal Justification', value = 0, min = 0, max = 1),
													numericInput('line_leg_boxyjust', 'Vertical Justification', value = 1, min = 0, max = 1)
											)
										),
										tabPanel('Legend Text',
											flowLayout(
													selectInput('line_leg_texthoriz', 'Horizontal Text', choices = c(TRUE, FALSE), selected = FALSE),
													textInput('line_leg_textcol', 'Text Color', value = 'black'),
													textInput('line_leg_title', 'Title', value = ''),
													numericInput('line_leg_textfont', 'Text Font', value = 1, min = 1, max = 5, step = 1),
													numericInput('line_leg_textcolumns', 'Columns', value = 1, min = 0, step = 1),
													textInput('line_leg_titlecol', 'Title Color', value = 'black'),
													numericInput('line_leg_textadj', 'Text Horizontal Adj', value = 0.5, min = 0, max = 1, step = 0.1)
											)
										)
									)
								),

								column(6,
										plotOutput('line_plot_8', height = '400px')
								)
						),

						tabPanel('Plot',
              fluidRow(
                column(8, offset = 2,
                  plotOutput('line_final')
                )
              )
            )

					)
				)
		)
	)
)
