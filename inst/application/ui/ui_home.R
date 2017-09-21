tabPanel("Home", value = "tab_analyze_home",
	fluidPage(
		
		fluidRow(
			column(12, align = 'center',
				h3('What do you want to do?')
			)
		),

		br(),
		br(),

		fluidRow(
			column(1),
			column(2, align = 'right',
				img(src = 'summary1.png', width = '100px', height = '100px')
			),
			column(6, align = 'center',
				h4('Descriptive Statisics'),
				p('Generate descriptive/summary statistics.')
			),
			column(2, align = 'left',
				br(),
				actionButton(
					inputId = 'click_descriptive',
					label = 'Click Here',
					width = '100px'
				)
			),
			column(1)
		),

		br(),

		fluidRow(
			column(1),
			column(2, align = 'right',
				img(src = 'normal.png', width = '100px', height = '100px')
			),
			column(6, align = 'center',
				h4('Statistical Distributions'),
				p('Explore and visualize different statistical distributions.')
			),
			column(2, align = 'left',
				br(),
				actionButton(
					inputId = 'click_distributions',
					label = 'Click Here',
					width = '100px'
				)
			),
			column(1)
		),

		br(),

		fluidRow(
			column(1),
			column(2, align = 'right',
				img(src = 'ttest3.jpg', width = '100px', height = '100px')
			),
			column(6, align = 'center',
				h4('Hypothesis Testing'),
				p('Test hypothesis using parametric and non-parametric tests.')
			),
			column(2, align = 'left',
				br(),
				actionButton(
					inputId = 'click_inference',
					label = 'Click Here',
					width = '100px'
				)
			),
			column(1)
		),

		br(),

		fluidRow(
			column(1),
			column(2, align = 'right',
				img(src = 'simple_reg.png', width = '100px', height = '100px')
			),
			column(6, align = 'center',
				h4('Model Building'),
				p('Tools for building simple and multiple linear regression models.')
			),
			column(2, align = 'left',
				br(),
				actionButton(
					inputId = 'click_model',
					label = 'Click Here',
					width = '100px'
				)
			),
			column(1)
		),

		br(),

		fluidRow(
			column(1),
			column(2, align = 'right',
				img(src = 'visualize2.png', width = '100px', height = '100px')
			),
			column(6, align = 'center',
				h4('Data Visualization'),
				p('Visualize data using ggplot2, rbokeh, plotly and highcharts.')
			),
			column(2, align = 'left',
				br(),
				actionButton(
					inputId = 'click_visualize',
					label = 'Click Here',
					width = '100px'
				)
			),
			column(1)	
		)
	)
)