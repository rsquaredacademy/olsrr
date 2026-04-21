tabPanel('Pie Chart', value = 'tab_pie_prh',

	fluidPage(
		fluidRow(
			column(12, align = 'left',
				h4('Pie Chart')
			)
		),

		hr(),

		fluidRow(
			column(12,
				tabsetPanel(type = 'tabs',
					
					tabPanel('plotly',

						fluidRow(
							column(2,
								selectInput('piely_select_x', 'Variable: ',
                              choices = "", selected = ""),
								textInput(inputId = "piely_xlabel", label = "X Axes Label: ",
                  value = "label"),
								selectInput('piely_text_pos', 'Text Position: ',
                              choices = c("Inside" = "inside", "Outside" = "outside"), 
                              selected = "Inside"),
								selectInput('piely_text_dir', 'Text Direction: ',
                              choices = c("Anticlockwise" = "anticlockwise", "Clockwise" = "clockwise"), 
                              selected = "Anticlockwise"),
								numericInput(inputId = "piely_pull", "Pie Pull: ",
                	min = 0, value = 0, max = 1, step = 0.1),
								textInput(inputId = "piely_color", label = "Line Color: ",
                  value = "")
							),

							column(2,
								textInput(inputId = "piely_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "piely_ylabel", label = "Y Axes Label: ",
                  value = "label"),
                selectInput('piely_text_info', 'Text Info: ',
                              choices = c("Label" = "label", "Percent" = "percent", "Label & Percent" = "label+percent"), 
                              selected = "Label"),
                numericInput(inputId = "piely_text_rotation", "Text Rotation: ",
                	min = 0, value = 0, max = 360, step = 1),
                numericInput(inputId = "piely_hole", "Pie Hole: ",
                	min = 0, value = 0, max = 1, step = 0.1),
                numericInput(inputId = "piely_opacity", "Opacity: ",
                	min = 0, value = 1, max = 1, step = 0.1)
							),

							column(8, align = 'center',
                plotly::plotlyOutput('piely_plot_1', height = '600px')
              )
						)
					),

					tabPanel('highcharts',

						fluidRow(
							column(2,
								selectInput('hipie_select_x', 'Variable: ',
                              choices = "", selected = ""),
								textInput(inputId = "hipie_xlabel", label = "X Axes Label: ",
                  value = "label")
							),

							column(2,
								textInput(inputId = "hipie_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "hipie_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8, align = 'center',
                highcharter::highchartOutput('hipie_plot_1', height = '600px')
              )
						)
					)
				)
			)
		)
	)
)