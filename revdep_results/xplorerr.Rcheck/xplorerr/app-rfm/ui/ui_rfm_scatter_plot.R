tabPanel('Scatter Plots', value = 'tab_rfm_scatter',

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('Scatter Plots'),
        p("Examine the relationship between recency, frequency and monetary values.")
      )
    ),

    hr(),

		fluidRow(

			br(),
			br(),
			column(2),
			column(8, align = 'center', 
				plotOutput('plot_scatter_1', height = '500px')
			),
			column(2)

		),

		fluidRow(

			br(),
			br(),
			column(2),
			column(8, align = 'center', 
				plotOutput('plot_scatter_2', height = '500px')
			),
			column(2)

		),

		fluidRow(

			br(),
			br(),
			column(2),
			column(8, align = 'center', 
				plotOutput('plot_scatter_3', height = '500px')
			),
			column(2)
		)

	)

)


