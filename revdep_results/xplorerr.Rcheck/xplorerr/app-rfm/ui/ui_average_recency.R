tabPanel("Median Recency", value = "tab_average_recency",

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('Median Recency by Segment')
      )
    ),

    hr(),

		fluidRow(

			br(),
			br(),
			column(2),
			column(8, align = 'center', 
				plotOutput('segment_average_recency') %>% 
					withSpinner()
			),
			column(2)

		)

	)

)