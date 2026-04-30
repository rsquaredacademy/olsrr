tabPanel('Scatter Plot', value = 'tab_gscatter',

	fluidPage(
		
		fluidRow(
			column(12, align = 'left',
				h4('Scatter Plot')
			)
		),

		hr(),

		fluidRow(
			column(12,
				tabsetPanel(type = 'tabs',

					tabPanel('Variables',

						fluidRow(
							column(2,
								selectInput('gscatter_select_x', 'X Axis Variable: ',
                              choices = "", selected = ""),
								textInput(inputId = "gscatter_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "gscatter_xlabel", label = "X Axes Label: ",
                  value = "label")
							),

							column(2,
								selectInput('gscatter_select_y', 'Y Axis Variable: ',
                              choices = "", selected = ""),
								textInput(inputId = "gscatter_subtitle", label = "Subtitle: ",
									value = "subtitle"),
                textInput(inputId = "gscatter_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8,
                plotOutput('gscatter_plot_1', height = '600px')
              )

						)
					),

					tabPanel('Aesthetics',

						column(6,
							tabsetPanel(type = 'tabs',
								tabPanel('Options',
									selectInput('geas', 'Aesthetics',
										choices = c(Variables = 'Use Variables',
											Values = 'Specify Values'
										)
									)
								),
								tabPanel('Use Variables', 
									selectInput('gaes_color', 'Color: ', choices = "", selected = ""),
									selectInput('gaes_shape', 'Shape: ', choices = "", selected = ""),
									selectInput('gaes_size', 'Size: ', choices = "", selected = "")
								),
								tabPanel('Specify Values', 
									textInput('gscat_color', 'Color: ', value = "black"),
									numericInput('gscat_shape', 'Shape: ', value = 1, min = 0, max = 25),
									numericInput('gscat_size', 'Size: ', value = 1, min = 0),
									textInput('gscat_fill', 'Fill: ', value = "black")
								)
							)							
						),

						column(6,
              plotOutput('gscatter_plot_2', height = '600px')
            )

					),

					tabPanel('Axis Range',
            fluidRow(
              column(2,
                  uiOutput('ui_gxrange_min'),
                  uiOutput('ui_gxrange_max')
              ),
              column(2,
                  uiOutput('ui_gyrange_min'),
                  uiOutput('ui_gyrange_max')
              ),
              column(8,
                plotOutput('gscatter_plot_3', height = '600px')
              )
            )
          ),

					tabPanel('Fit Line',
						fluidRow(
							column(2,
								selectInput('gscat_line', 'Fit Line',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
								selectInput('greg_type', 'Regression Type',
									choices = c(Linear = 'lm', Loess = 'loess')
								),
								selectInput('greg_se', 'Standard Error',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
									selected = "FALSE"
								)
							),
							column(8,
                plotOutput('gscatter_plot_4', height = '600px')
              )
						)
					),

					tabPanel('Annotations',
						fluidRow(
							column(2,
								selectInput('gscat_text', 'Add Text',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
								numericInput(
                  inputId = "gscatter_text_x_loc",
                  label = "X Intercept: ",
                  value = 1, step = 1),
                numericInput(
                  inputId = "gscatter_text_y_loc",
                  label = "Y Intercept: ",
                  value = 1, step = 1
                )
              ),
              column(2,
                textInput(inputId = "gscatter_plottext", label = "Text:",
                  value = ""),
                textInput(
                  inputId = "gscatter_textcolor",
                  label = "Text Color: ",
                  value = "black"
                ),
                numericInput(
                  inputId = "gscatter_textsize",
                  label = "Text Size: ",
                  value = 1, min = 0.1, step = 0.1
                )
              ),
              column(8,
                plotOutput('gscatter_plot_5', height = '600px')
              )
						)
					),

					tabPanel('Others',
						column(4,
							tabsetPanel(type = 'tabs',
								tabPanel('Color',
									fluidRow(
										column(6,
											textInput(inputId = "gscat_title_col", label = "Title:",
	                  		value = "black"),
											textInput(inputId = "gscat_sub_col", label = "Subtitle:",
	                  		value = "black"),
											textInput(inputId = "gscat_xlab_col", label = "X Axis Label:",
	                  		value = "black"),
											textInput(inputId = "gscat_ylab_col", label = "Y Axis Label:",
	                  		value = "black")
										)
									)
								),
								tabPanel('Font Family',
									fluidRow(
										column(6,
											textInput(inputId = "gscat_title_fam", label = "Title:",
		                  		value = "Times"),
												textInput(inputId = "gscat_sub_fam", label = "Subtitle:",
		                  		value = "Times"),
												textInput(inputId = "gscat_xlab_fam", label = "X Axis Label:",
		                  		value = "Times"),
												textInput(inputId = "gscat_ylab_fam", label = "Y Axis Label:",
		                  		value = "Times")
										)
									)	
								),
								tabPanel('Font Face',
									fluidRow(
										column(6,
											selectInput('gscat_title_font', 'Title:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gscat_subtitle_font', 'Subtitle:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gscat_xlab_font', 'X Axis Label:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain"),
											selectInput('gscat_ylab_font', 'Y Axis Label:',
												choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
		                  	selected = "plain")
										)
									)	
								),
								tabPanel('Font Size',
									fluidRow(
										column(6,
											numericInput(inputId = "gscat_title_size", label = "Title:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gscat_sub_size", label = "Subtitle:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gscat_xlab_size", label = "X Axis Label:",
	                  		min = 1, step = 0.1, value = 1),
											numericInput(inputId = "gscat_ylab_size", label = "Y Axis Label:",
	                  		min = 1, step = 0.1, value = 1)
										)
									)
								),
								tabPanel('Horizontal',
									fluidRow(
										column(6,
											numericInput(inputId = "gscat_title_hjust", label = "Title:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gscat_sub_hjust", label = "Subtitle:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gscat_xlab_hjust", label = "X Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gscat_ylab_hjust", label = "Y Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1)
										)
									)
								),
								tabPanel('Vertical',
									fluidRow(
										column(6,
											numericInput(inputId = "gscat_title_vjust", label = "Title:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gscat_sub_vjust", label = "Subtitle:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gscat_xlab_vjust", label = "X Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1),
											numericInput(inputId = "gscat_ylab_vjust", label = "Y Axis Label:",
	                  		min = 0, step = 0.1, value = 0.5, max = 1)
										)
									)
								)
							)
						),

						column(8,
							plotOutput('gscatter_plot_6', height = '600px')
						)
					),

					tabPanel('Theme',
						column(2,
							selectInput(inputId = 'gscatter_theme', label = 'Theme',
								choices = list("Classic Dark", "Default", "Light", "Minimal", 
									"Dark", "Classic", "Empty"), selected = "Default")
						),
						column(2),
						column(8, align = 'center',
							plotOutput('gscatter_plot_7', height = '600px')
						)
					),

					tabPanel('Plot',
						column(12, align = 'center',
							plotOutput('gscatter_plot_8', height = '600px')
						)
					)
				)
			)
		)
	)
)