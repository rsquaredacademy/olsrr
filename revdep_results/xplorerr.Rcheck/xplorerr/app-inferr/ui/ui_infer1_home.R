tabPanel('Inference - 1', value = 'tab_infer1_home',

	fluidPage(

		fluidRow(
			br(),
			column(12, align = 'center',
				h5('What do you want to do?')
			),
			br(),
			br()
		),

		fluidRow(

			column(12),

			br(),

			column(3),

			column(4, align = 'left',
				h5('One Sample t Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_1_t',
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
				h5('One Sample Variance Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_1_var',
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
				h5('One Sample Proportion Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_1_prop',
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
				h5('Chi Square Goodness of Fit')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_1_chi',
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
				h5('Runs Test for Randomness')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_1_runs',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3)

		)
	)

)