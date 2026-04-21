tabPanel("Home", value = "tab_home_viz",

	fluidPage(

		fluidRow(
			column(12, align = 'center',
				h3("Please choose a library")
			)
		),

		fluidRow(column(12, hr())),

		br(),
		br(),

		fluidRow(			
			column(1),
			column(2, align = 'right',
				img(src = 'Rlogonew.png', width = '100px', height = '100px')
			),
			column(6, align = 'center',
				h5("Visualize data using the graphics pacakge from base R")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_base", 
					label = "Click Here",
					width = "120px"
				)
			),
			column(1)
		),

		br(),
		br(),

		fluidRow(	
			column(1),		
			column(2, align = 'right',
				img(src = 'ggplot2_logo.png', width = '100px', height = '100px')
			),
			column(6, align = 'center',
				h5("Visualize data using ggplot2 (based on Grammar of Graphics)")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_ggplot2", 
					label = "Click Here",
					width = "120px"
				)
			),
			column(1)
		),

		br(),
		br(),

		fluidRow(			
			column(1),
			column(2, align = 'right',
				img(src = 'highcharts_logo.png', width = '40px', height = '40px'),
				img(src = 'plotly_logo.png', width = '40px', height = '40px')
			),
			column(6, align = 'center',
				h5("Visualize data using plotly or highcharts")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_prh", 
					label = "Click Here",
					width = "120px"
				)
			),
			column(1)
		),

		fluidRow(column(12))
		
	)

)







