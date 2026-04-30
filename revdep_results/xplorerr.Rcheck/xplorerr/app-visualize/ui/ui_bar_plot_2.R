tabPanel('2 Factor Bar Plot', value = 'tab_bar_plot_2',

	fluidPage(
		fluidRow(
			column(12, align = 'left',
				h4('Bar Plot - II')
			)
		),

		hr(),

		fluidRow(
			column(12,
				tabsetPanel(type = 'tabs',
					
					tabPanel('plotly',

						fluidRow(
							column(2,
								selectInput('barly2_select_x', 'Variable 1: ',
                              choices = "", selected = ""),
								textInput(inputId = "barly2_xlabel", label = "X Axes Label: ",
                  value = "label"),
								textInput(inputId = "barly2_title", label = "Title: ",
									value = "title")
							),

							column(2,
								selectInput('barly2_select_y', 'Variable 2: ',
                              choices = "", selected = ""),
                textInput(inputId = "barly2_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8, align = 'center',
                plotly::plotlyOutput('barly2_plot_1', height = '600px')
              )
						)
					),

					tabPanel('highcharts',

						fluidRow(
							column(2,
								selectInput('hibar2_select_x', 'Variable 1: ',
                              choices = "", selected = ""),
								selectInput('hibar2_horiz', 'Horizontal',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
									selected = "FALSE",  width = '150px')
								# textInput(inputId = "hibar2_xlabel", label = "X Axes Label: ",
        #           value = "label"),
								# textInput(inputId = "hibar2_title", label = "Title: ",
								# 	value = "title")
							),

							column(2,
								selectInput('hibar2_select_y', 'Variable 2: ',
                              choices = "", selected = ""),
								selectInput('hibar2_stack', 'Stacked',
									choices = c("TRUE" = TRUE, "FALSE" = FALSE),
									selected = "FALSE",  width = '150px')
                # textInput(inputId = "hibar2_ylabel", label = "Y Axes Label: ",
                #   value = "label")
							),

							column(8, align = 'center',
                highcharter::highchartOutput('hibar2_plot_1', height = '600px')
              )
						)
					)
				)
			)
		)
	)
)
