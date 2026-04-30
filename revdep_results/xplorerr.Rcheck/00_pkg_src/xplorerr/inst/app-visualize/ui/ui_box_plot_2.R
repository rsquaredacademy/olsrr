tabPanel('2 Factor Box Plot', value = 'tab_box_plot_2',

	fluidPage(
		fluidRow(
			column(12, align = 'left',
				h4('Box Plot - II')
			)
		),

		hr(),

		fluidRow(
			column(12,
				tabsetPanel(type = 'tabs',
					
					tabPanel('plotly',

						fluidRow(
							column(2,
								selectInput('boxly2_select_x', 'Variable 1: ',
                              choices = "", selected = ""),
								textInput(inputId = "boxly2_xlabel", label = "X Axes Label: ",
                  value = "label"),
								textInput(inputId = "boxly2_title", label = "Title: ",
									value = "title")
							),

							column(2,
								selectInput('boxly2_select_y', 'Variable 2: ',
                              choices = "", selected = ""),
                textInput(inputId = "boxly2_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8, align = 'center',
                plotly::plotlyOutput('boxly2_plot_1', height = '600px')
              )
						)
					),

					tabPanel('highcharts',

						fluidRow(
							column(2,
								selectInput('hibox2_select_x', 'Variable 1: ',
                              choices = "", selected = ""),
								textInput(inputId = "hibox2_xlabel", label = "X Axes Label: ",
                  value = "label"),
								textInput(inputId = "hibox2_title", label = "Title: ",
									value = "title")
							),

							column(2,
								selectInput('hibox2_select_y', 'Variable 2: ',
                              choices = "", selected = ""),
                textInput(inputId = "hibox2_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8, align = 'center',
                highcharter::highchartOutput('hibox2_plot_1', height = '600px')
              )
						)
					)

				)
			)
		)
	)
)
