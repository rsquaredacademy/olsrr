tabPanel("Median Frequency", value = "tab_average_frequency",

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('Median Frequency by Segment')
      )
    ),

    hr(),

		fluidRow(

			br(),
			br(),
			column(2),
			column(8, align = 'center', 
				plotOutput('segment_average_frequency') %>% 
					withSpinner()
			),
			column(2)

		)

	)

)