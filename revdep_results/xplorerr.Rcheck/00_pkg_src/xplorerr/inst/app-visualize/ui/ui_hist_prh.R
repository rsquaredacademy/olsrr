tabPanel('Histogram', value = 'tab_hist_prh',

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
					
					tabPanel('plotly',

						fluidRow(
							column(2,
								selectInput('histly_select_x', 'Variable 1: ',
                              choices = "", selected = ""),
								textInput(inputId = "histly_xlabel", label = "X Axes Label: ",
                  value = "label"),
								selectInput('histly_horiz', 'Horizontal',
                  choices = c("TRUE" = "h", "FALSE" = "v"),
                  selected = "FALSE",  width = '150px'),
								textInput(inputId = "histly_color", label = "Color: ",
                  value = ""),
								selectInput('histly_auto', 'Auto Bin: ',
                  choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "TRUE"),
								numericInput(inputId = "histly_binstart", "Bin Start: ",
									min = 0, step = 1, value = 1)
							),

							column(2,
								textInput(inputId = "histly_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "histly_ylabel", label = "Y Axes Label: ",
                  value = "label"),
								selectInput('histly_type', 'Type',
                  choices = c("Count" = "count", "Density" = "density"),
                  selected = "Count",  width = '150px'),
								numericInput(inputId = "histly_opacity", "Opacity: ",
									min = 0, max = 1, step = 0.1, value = 1),
								numericInput(inputId = "histly_binsize", "Bin Size: ",
									min = 0, step = 1, value = 1),
								numericInput(inputId = "histly_binend", "Bin End: ",
									min = 0, step = 1, value = 1)
							),

							column(8, align = 'center',
                plotly::plotlyOutput('histly_plot_1', height = '600px')
              )
						)
					),

					tabPanel('highcharts',

						fluidRow(
							column(2,
								selectInput('hihist_select_x', 'Variable 1: ',
                              choices = "", selected = ""),
								textInput(inputId = "hihist_xlabel", label = "X Axes Label: ",
                  value = "label")
							),

							column(2,
								textInput(inputId = "hihist_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "hihist_color", label = "Color: ",
                  value = "blue")
							),

							column(8, align = 'center',
                highcharter::highchartOutput('hihist_plot_1', height = '600px')
              )
						)
					)

				)
			)
		)
	)
)
