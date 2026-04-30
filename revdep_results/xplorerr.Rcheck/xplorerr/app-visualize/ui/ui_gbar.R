tabPanel('Bar Plot', value = 'tab_gbar',

	fluidPage(

		fluidRow(
			column(12, align = 'left',
				h4('Bar Plot')
			)
		),

		hr(),

		fluidRow(
			column(12,
				tabsetPanel(type = 'tabs',
					
					tabPanel('Variables',

						fluidRow(
							column(2,
								selectInput('gbar_select_x', 'Variable: ',
                              choices = "", selected = ""),
								textInput(inputId = "gbar_barcol", label = "Bar Color: ",
                  value = "blue"),
								textInput(inputId = "gbar_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "gbar_xlabel", label = "X Axes Label: ",
                  value = "label")
							),

							column(2,
								selectInput('gbar_horiz', 'Horizontal',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
								textInput(inputId = "gbar_borcol", label = "Border Color: ",
                  value = "black"),
								textInput(inputId = "gbar_subtitle", label = "Subtitle: ",
									value = "subtitle"),
                textInput(inputId = "gbar_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8,
                plotOutput('gbar_plot_1', height = '600px')
              )

						)

					),

					tabPanel('Axis Range',
            fluidRow(
              column(2,
                uiOutput('ui_gbaryrange_min'),
                selectInput('gbar_remx', 'Remove X Axis Label',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE") 
              ),
              column(2,
              	uiOutput('ui_gbaryrange_max'),
              	selectInput('gbar_remy', 'Remove Y Axis Label',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE")
              ),
              column(8,
                plotOutput('gbar_plot_2', height = '600px')
              )
            )
          ),

					tabPanel('Annotations',
						fluidRow(
							column(2,
								selectInput('gbar_text', 'Add Text',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
								numericInput(
                  inputId = "gbar_text_x_loc",
                  label = "X Intercept: ",
                  value = 1, step = 1),
                numericInput(
                  inputId = "gbar_text_y_loc",
                  label = "Y Intercept: ",
                  value = 1, step = 1
                )
              ),
              column(2,
                textInput(inputId = "gbar_plottext", label = "Text:",
                  value = ""),
                textInput(
                  inputId = "gbar_textcolor",
                  label = "Text Color: ",
                  value = "black"
                ),
                numericInput(
                  inputId = "gbar_textsize",
                  label = "Text Size: ",
                  value = 10, min = 1, step = 1
                )
              ),
              column(8,
                plotOutput('gbar_plot_3', height = '600px')
              )
						)
					),

					tabPanel('Others',
						column(4,
							tabsetPanel(type = 'tabs',
								tabPanel('Color',
									fluidRow(
										column(6,
											textInput(inputId = "gbar_title_col", label = "Title:",
	                  		value = "black"),
											textInput(inputId = "gbar_sub_col", label = "Subtitle:",
	                  		value = "black"),
											textInput(inputId = "gbar_xlab_col", label = "X Axis Label:",
	                  		value = "black"),
											textInput(inputId = "gbar_ylab_col", label = "Y Axis Label:",
	                  		value = "black")
										)
									)
								),
								tabPanel('Font Family',
									fluidRow(
										column(6,
											textInput(inputId = "gbar_title_fam", label = "Title:",
		                  		value = "Times"),
												textInput(inputId = "gbar_sub_fam", label = "Subtitle:",
		                  		value = "Times"),
												textInput(inputId = "gbar_xlab_fam", label = "X Axis Label:",
		                  		value = "Times"),
												textInput(inputId = "gbar_ylab_fam", label = "Y Axis Label:",
		                  		value = "Times")
										)
									)	
								),
								tabPanel('Font Face',
									fluidRow(
										column(6,
											selectInput('gbar_title_font', 'Title:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gbar_subtitle_font', 'Subtitle:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gbar_xlab_font', 'X Axis Label:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gbar_ylab_font', 'Y Axis Label:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain")
										)
									)	
								),
								tabPanel('Font Size',
									fluidRow(
										column(6,
											numericInput(inputId = "gbar_title_size", label = "Title:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gbar_sub_size", label = "Subtitle:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gbar_xlab_size", label = "X Axis Label:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gbar_ylab_size", label = "Y Axis Label:",
	                  		min = 1, step = 0.1, value = 1)
										)
									)
								),
								tabPanel('Horizontal',
									fluidRow(
										column(6,
											numericInput(inputId = "gbar_title_hjust", label = "Title:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gbar_sub_hjust", label = "Subtitle:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gbar_xlab_hjust", label = "X Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gbar_ylab_hjust", label = "Y Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1)
										)
									)
								),
								tabPanel('Vertical',
									fluidRow(
										column(6,
											numericInput(inputId = "gbar_title_vjust", label = "Title:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gbar_sub_vjust", label = "Subtitle:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gbar_xlab_vjust", label = "X Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gbar_ylab_vjust", label = "Y Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1)
										)
									)
								)
							)
						),

						column(8,
							plotOutput('gbar_plot_4', height = '600px')
						)
					),

					tabPanel('Theme',
						column(2,
							selectInput(inputId = 'gbar_theme', label = 'Theme',
								choices = list("Classic Dark", "Default", "Light", "Minimal", 
									"Dark", "Classic", "Empty"), selected = "Default")
						),
						column(2),
						column(8, align = 'center',
							plotOutput('gbar_plot_5', height = '600px')
						)
					),

					tabPanel('Plot',
						column(12, align = 'center',
							plotOutput('gbar_plot_6', height = '600px')
						)
					)

				)
			)
		)

	)

)