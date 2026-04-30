tabPanel('Line Chart - I', value = 'tab_gline1',

	fluidPage(
		
		fluidRow(
			column(12, align = 'left',
				h4('Line Plot')
			)
		),

		hr(),

		fluidRow(
			column(12,
				tabsetPanel(type = 'tabs',

					tabPanel('Variables',

						fluidRow(
							column(2,
								selectInput('gline_select_x', 'Variable: ',
                              choices = "", selected = ""),
								textInput(inputId = "gline_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "gline_xlabel", label = "X Axes Label: ",
                  value = "label")
							),

							column(2,
								selectInput('gline_y', 'Columns: ', choices = "", selected = "",
									selectize = TRUE, multiple = TRUE),
								textInput(inputId = "gline_subtitle", label = "Subtitle: ",
									value = "subtitle"),
                textInput(inputId = "gline_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8,
                plotOutput('gline_plot_1', height = '600px')
              )
						)
					),

					# tabPanel('Aesthetics',
					# 	fluidRow(
					# 		column(4,
					# 			column(6,
					# 				textInput(inputId = 'gline_col', label = 'Color: ',
					# 					value = 'black'),
					# 				textInput(inputId = 'gline_alpha', label = 'Alpha: ',
					# 					value = '1')
					# 			),
					# 			column(6,
					# 				textInput(inputId = 'gline_ltype', label = 'Line Type: ',
					# 					value = '1'),
					# 				textInput(inputId = 'gline_lsize', label = 'Line Size: ',
					# 					value = '1')
					# 			),
					# 			column(12, align = 'center',
					# 				actionButton(inputId = 'gline2_submit', label = 'Submit')
					# 			)
					# 		),
					# 		column(8,
					# 			plotOutput('gline_plot_2', height = '600px')
					# 		)
					# 	)
					# ),

					tabPanel('Axis Range',
            fluidRow(
              column(2,
                uiOutput('ui_glineyrange_min'),
                selectInput('gline_remx', 'Remove X Axis Label',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE") 
              ),
              column(2,
              	uiOutput('ui_glineyrange_max'),
              	selectInput('gline_remy', 'Remove Y Axis Label',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE")
              ),
              column(8,
                plotOutput('gline_plot_3', height = '600px')
              )
            )
          ),

          tabPanel('Annotations',
						fluidRow(
							column(2,
								selectInput('gline_text', 'Add Text',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
								numericInput(
                  inputId = "gline_text_x_loc",
                  label = "X Intercept: ",
                  value = 1, step = 1),
                numericInput(
                  inputId = "gline_text_y_loc",
                  label = "Y Intercept: ",
                  value = 1, step = 1
                )
              ),
              column(2,
                textInput(inputId = "gline_plottext", label = "Text:",
                  value = ""),
                textInput(
                  inputId = "gline_textcolor",
                  label = "Text Color: ",
                  value = "black"
                ),
                numericInput(
                  inputId = "gline_textsize",
                  label = "Text Size: ",
                  value = 10, min = 1, step = 1
                )
              ),
              column(8,
                plotOutput('gline_plot_4', height = '600px')
              )
						)
					),

					tabPanel('Others',
						column(4,
							tabsetPanel(type = 'tabs',
								tabPanel('Color',
									fluidRow(
										column(6,
											textInput(inputId = "gline_title_col", label = "Title:",
	                  		value = "black"),
											textInput(inputId = "gline_sub_col", label = "Subtitle:",
	                  		value = "black"),
											textInput(inputId = "gline_xlab_col", label = "X Axis Label:",
	                  		value = "black"),
											textInput(inputId = "gline_ylab_col", label = "Y Axis Label:",
	                  		value = "black")
										)
									)
								),
								tabPanel('Font Family',
									fluidRow(
										column(6,
											textInput(inputId = "gline_title_fam", label = "Title:",
		                  		value = "Times"),
												textInput(inputId = "gline_sub_fam", label = "Subtitle:",
		                  		value = "Times"),
												textInput(inputId = "gline_xlab_fam", label = "X Axis Label:",
		                  		value = "Times"),
												textInput(inputId = "gline_ylab_fam", label = "Y Axis Label:",
		                  		value = "Times")
										)
									)	
								),
								tabPanel('Font Face',
									fluidRow(
										column(6,
											selectInput('gline_title_font', 'Title:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gline_subtitle_font', 'Subtitle:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gline_xlab_font', 'X Axis Label:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gline_ylab_font', 'Y Axis Label:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain")
										)
									)	
								),
								tabPanel('Font Size',
									fluidRow(
										column(6,
											numericInput(inputId = "gline_title_size", label = "Title:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gline_sub_size", label = "Subtitle:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gline_xlab_size", label = "X Axis Label:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gline_ylab_size", label = "Y Axis Label:",
	                  		min = 1, step = 0.1, value = 1)
										)
									)
								),
								tabPanel('Horizontal',
									fluidRow(
										column(6,
											numericInput(inputId = "gline_title_hjust", label = "Title:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline_sub_hjust", label = "Subtitle:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline_xlab_hjust", label = "X Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline_ylab_hjust", label = "Y Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1)
										)
									)
								),
								tabPanel('Vertical',
									fluidRow(
										column(6,
											numericInput(inputId = "gline_title_vjust", label = "Title:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline_sub_vjust", label = "Subtitle:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline_xlab_vjust", label = "X Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline_ylab_vjust", label = "Y Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1)
										)
									)
								)
							)
						),

						column(8,
							plotOutput('gline_plot_5', height = '600px')
						)
					),

					tabPanel('Theme',
						column(2,
							selectInput(inputId = 'gline_theme', label = 'Theme',
								choices = list("Classic Dark", "Default", "Light", "Minimal", 
									"Dark", "Classic", "Empty"), selected = "Default")
						),
						column(2),
						column(8, align = 'center',
							plotOutput('gline_plot_6', height = '600px')
						)
					),

					tabPanel('Plot',
						column(12, align = 'center',
							plotOutput('gline_plot_7', height = '600px')
						)
					)

				)
			)
		)
	)

)