tabPanel('Heat Map', value = 'tab_rfm_heatmap',

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('RFM Heatmap'),
        p("The heat map shows the average monetary value for different
					categories of recency and frequency scores. Higher scores of frequency and
					recency are characterized by higher average monetary value as indicated by
					the darker areas in the heatmap.")
      )
    ),

    hr(),

		fluidRow(

			br(),
			br(),
			column(2),
			column(8, align = 'center', 
				plotOutput('plot_heatmap', height = '500px')
			),
			column(2)

		)

	)

)
