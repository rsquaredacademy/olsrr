tabPanel('Inference - 2', value = 'tab_infer2_home',

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
				h5('Independent Sample t Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_2_it',
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
				h5('Paired Sample t Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_2_pt',
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
				h5('Binomial Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_2_binom',
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
				h5('Two Sample Variance Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_2_var',
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
				h5('Two Sample Proportion Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_2_prop',
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
				h5('Chi Square Association Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_2_chi',
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
				h5('McNemar Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_2_mcnemar',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3)

		)
	)

)