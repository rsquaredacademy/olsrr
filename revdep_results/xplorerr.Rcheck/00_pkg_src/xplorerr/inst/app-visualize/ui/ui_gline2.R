tabPanel('Line Chart - II', value = 'tab_gline2',

	fluidPage(

		fluidRow(
			column(12, align = 'left',
				h4('Line Plot - II')
			)
		),

		hr(),

		fluidRow(
			column(12,
				tabsetPanel(type = 'tabs',

					tabPanel('Variables',

						fluidRow(
							column(2,
								selectInput('gline2_select_x', 'Variable: ',
                              choices = "", selected = ""),
								selectInput('gline2_group', 'Group: ',
                              choices = "", selected = ""),
								textInput(inputId = "gline2_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "gline2_xlabel", label = "X Axes Label: ",
                  value = "label")
							),

							column(2,
								selectInput('gline2_y', 'Columns: ', choices = "", selected = ""),
								textInput(inputId = "gline2_subtitle", label = "Subtitle: ",
									value = "subtitle"),
                textInput(inputId = "gline2_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8,
                plotOutput('gline2_plot_1', height = '600px')
              )
						)
					),

					tabPanel('Aesthetics',
						fluidRow(
							column(2,
								br(),
								column(12,
									actionButton('gline2_col_yes', 'Color', width = '120px')
								),
								column(12,
									br(),
									br(),
									actionButton('gline2_ltype_yes', 'Line Type', width = '120px')
								),
								column(12,
									br(),
									br(),
									actionButton('gline2_size_yes', 'Size', width = '120px')
								)
							),
							column(2, 
								column(12,
									uiOutput('gline2_col_ui')
								),
								column(12,	
									uiOutput('gline2_ltype_ui')
								),
								column(12,	
									uiOutput('gline2_size_ui')
								)
							),
							column(8,
								plotOutput('gline2_plot_2', height = '600px')
							)
						)
					),

					tabPanel('Axis Range',
            fluidRow(
              column(2,
                uiOutput('ui_gline2yrange_min'),
                selectInput('gline2_remx', 'Remove X Axis Label',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE") 
              ),
              column(2,
              	uiOutput('ui_gline2yrange_max'),
              	selectInput('gline2_remy', 'Remove Y Axis Label',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE")
              ),
              column(8,
                plotOutput('gline2_plot_3', height = '600px')
              )
            )
          ),

          tabPanel('Annotations',
						fluidRow(
							column(2,
								selectInput('gline2_text', 'Add Text',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
								numericInput(
                  inputId = "gline2_text_x_loc",
                  label = "X Intercept: ",
                  value = 1, step = 1),
                numericInput(
                  inputId = "gline2_text_y_loc",
                  label = "Y Intercept: ",
                  value = 1, step = 1
                )
              ),
              column(2,
                textInput(inputId = "gline2_plottext", label = "Text:",
                  value = ""),
                textInput(
                  inputId = "gline2_textcolor",
                  label = "Text Color: ",
                  value = "black"
                ),
                numericInput(
                  inputId = "gline2_textsize",
                  label = "Text Size: ",
                  value = 10, min = 1, step = 1
                )
              ),
              column(8,
                plotOutput('gline2_plot_4', height = '600px')
              )
						)
					),

					tabPanel('Others',
						column(4,
							tabsetPanel(type = 'tabs',
								tabPanel('Color',
									fluidRow(
										column(6,
											textInput(inputId = "gline2_title_col", label = "Title:",
	                  		value = "black"),
											textInput(inputId = "gline2_sub_col", label = "Subtitle:",
	                  		value = "black"),
											textInput(inputId = "gline2_xlab_col", label = "X Axis Label:",
	                  		value = "black"),
											textInput(inputId = "gline2_ylab_col", label = "Y Axis Label:",
	                  		value = "black")
										)
									)
								),
								tabPanel('Font Family',
									fluidRow(
										column(6,
											textInput(inputId = "gline2_title_fam", label = "Title:",
		                  		value = "Times"),
												textInput(inputId = "gline2_sub_fam", label = "Subtitle:",
		                  		value = "Times"),
												textInput(inputId = "gline2_xlab_fam", label = "X Axis Label:",
		                  		value = "Times"),
												textInput(inputId = "gline2_ylab_fam", label = "Y Axis Label:",
		                  		value = "Times")
										)
									)	
								),
								tabPanel('Font Face',
									fluidRow(
										column(6,
											selectInput('gline2_title_font', 'Title:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gline2_subtitle_font', 'Subtitle:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gline2_xlab_font', 'X Axis Label:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gline2_ylab_font', 'Y Axis Label:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain")
										)
									)	
								),
								tabPanel('Font Size',
									fluidRow(
										column(6,
											numericInput(inputId = "gline2_title_size", label = "Title:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gline2_sub_size", label = "Subtitle:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gline2_xlab_size", label = "X Axis Label:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gline2_ylab_size", label = "Y Axis Label:",
	                  		min = 1, step = 0.1, value = 1)
										)
									)
								),
								tabPanel('Horizontal',
									fluidRow(
										column(6,
											numericInput(inputId = "gline2_title_hjust", label = "Title:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline2_sub_hjust", label = "Subtitle:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline2_xlab_hjust", label = "X Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline2_ylab_hjust", label = "Y Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1)
										)
									)
								),
								tabPanel('Vertical',
									fluidRow(
										column(6,
											numericInput(inputId = "gline2_title_vjust", label = "Title:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline2_sub_vjust", label = "Subtitle:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline2_xlab_vjust", label = "X Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gline2_ylab_vjust", label = "Y Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1)
										)
									)
								)
							)
						),

						column(8,
							plotOutput('gline2_plot_5', height = '600px')
						)
					),

					tabPanel('Theme',
						column(2,
							selectInput(inputId = 'gline2_theme', label = 'Theme',
								choices = list("Classic Dark", "Default", "Light", "Minimal", 
									"Dark", "Classic", "Empty"), selected = "Default")
						),
						column(2),
						column(8, align = 'center',
							plotOutput('gline2_plot_6', height = '600px')
						)
					),

					tabPanel('Plot',
						column(12, align = 'center',
							plotOutput('gline2_plot_7', height = '600px')
						)
					)
				)
			)
		)
	)

)