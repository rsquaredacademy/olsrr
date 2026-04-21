tabPanel('Bar Chart', value = 'tab_rfm_barchart',

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('RFM Bar Chart'),
        p("Examine the distribution of monetary scores for the different 
        	combinations of frequency and recency scores.")
      )
    ),

    hr(),

		fluidRow(

			br(),
			br(),
			column(2),
			column(8, align = 'center', 
				plotOutput('plot_barchart', height = '500px')
			),
			column(2)

		)

	)

)

