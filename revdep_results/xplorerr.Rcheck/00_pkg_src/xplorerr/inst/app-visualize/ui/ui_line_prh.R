tabPanel('Line Chart', value = 'tab_line_prh',

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
					
					tabPanel('plotly',

						fluidRow(
							column(2,
								selectInput('linely_select_x', 'X Axis: ',
                              choices = "", selected = ""),
								textInput(inputId = "linely_xlabel", label = "X Axes Label: ",
                  value = "label"),
								textInput(inputId = "linely_title", label = "Title: ",
									value = "title"),
								textInput(inputId = "linely_type", label = "Line Type: ",
									value = "plain")
							),

							column(2,
								selectInput('linely_select_y', 'Y Axis: ',
                              choices = "", selected = ""),
                textInput(inputId = "linely_ylabel", label = "Y Axes Label: ",
                  value = "label"),
                textInput(inputId = "linely_color", label = "Color: ",
                  value = "blue"),
                numericInput(inputId = "linely_width", label = "Width: ",
                	min = 1, value = 1, step = 1)
							),

							column(8, align = 'center',
                plotly::plotlyOutput('linely_plot_1', height = '600px')
              )
						)
					),

					tabPanel('highcharts',

						fluidRow(
							column(2,
								selectInput('hiline_select_x', 'Variable 1: ',
                              choices = "", selected = ""),
								selectInput('hiline_labels', 'Labels',
                  choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE",  width = '150px'),
								textInput(inputId = "hiline_xlabel", label = "X Axes Label: ",
                  value = "label")
							),

							column(2,
								selectInput('hiline_select_y', 'Variable 1: ', selectize = TRUE, 
                              multiple = TRUE, choices = "", selected = ""),
								textInput(inputId = "hiline_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "hiline_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8, align = 'center',
                highcharter::highchartOutput('hiline_plot_1', height = '600px')
              )
						)
					)
				)
			)
		)
	)
)
