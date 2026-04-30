tabPanel('Histogram', value = 'tab_rfm_histogram',

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('Histograms'),
        p("Histograms of recency, frequency and monetary value.")
      )
    ),

    hr(),

		fluidRow(

			br(),
			br(),
			column(2),
			column(8, align = 'center',
				plotOutput('plot_histogram', height = '500px')
			),
			column(2)

		)

	)

)

