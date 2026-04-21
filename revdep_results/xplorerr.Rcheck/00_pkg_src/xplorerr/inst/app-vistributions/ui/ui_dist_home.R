tabPanel('Distributions', value = 'tab_dist_home',

	fluidPage(

		includeCSS("mystyle.css"),

		fluidRow(

			column(12),

			br(),

			column(12, align = 'center',
				h5('Visualize Probability Distributions')
			),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Normal Distribution')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_dist_home_1',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('t Distribution')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_dist_home_2',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Chi Square Distribution')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_dist_home_3',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Binomial Distribution')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_dist_home_4',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('F Distribution')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_dist_home_5',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3)

		)

	)
)
