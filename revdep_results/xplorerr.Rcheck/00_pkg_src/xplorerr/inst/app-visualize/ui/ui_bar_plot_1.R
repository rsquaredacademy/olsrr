tabPanel('Bar Plot', value = 'tab_bar_plot_1',

	fluidPage(
		fluidRow(
			column(12, align = 'left',
				h4('Bar Plot - I')
			)
		),

		hr(),

		fluidRow(
			column(12,
				tabsetPanel(type = 'tabs',
					
					tabPanel('plotly',

						fluidRow(
							column(2,
								selectInput('barly1_select_x', 'Variable: ',
                              choices = "", selected = ""),
								textInput(inputId = "barly1_xlabel", label = "X Axes Label: ",
                  value = "label"),
								textInput(inputId = "barly1_color", label = "Color: ",
                  value = "blue")
							),

							column(2,
								textInput(inputId = "barly1_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "barly1_ylabel", label = "Y Axes Label: ",
                  value = "label"),
                textInput(inputId = "barly1_btext", label = "Text: ",
                  value = "")
							),

							column(8, align = 'center',
                plotly::plotlyOutput('barly1_plot_1', height = '600px')
              )
						)
					),

					tabPanel('highcharts',

						fluidRow(
							column(2,
								selectInput('hibar1_select_x', 'Variable: ',
                              choices = "", selected = ""),
								textInput(inputId = "hibar1_xlabel", label = "X Axes Label: ",
                  value = "label")
							),

							column(2,
								textInput(inputId = "hibar1_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "hibar1_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8, align = 'center',
                highcharter::highchartOutput('hibar1_plot_1', height = '600px')
              )
						)
					)

				)
			)
		)
	)
)
