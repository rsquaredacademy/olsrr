tabPanel('Pie Chart', value = 'tab_gpie',

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

					tabPanel('Variables',

						fluidRow(
							column(2,
								selectInput('gpie_select_x', 'Variable: ',
                              choices = "", selected = ""),
                textInput(inputId = "gpie_xlabel", label = "X Axes Label: ",
                  value = "label")
							),

							column(2,
								textInput(inputId = "gpie_title", label = "Title: ",
									value = "title"),
                textInput(inputId = "gpie_ylabel", label = "Y Axes Label: ",
                  value = "label")
							),

							column(8,
                plotOutput('gpie_plot_1', height = '600px')
              )

						)

					)

				)
			)
		)

	)

)