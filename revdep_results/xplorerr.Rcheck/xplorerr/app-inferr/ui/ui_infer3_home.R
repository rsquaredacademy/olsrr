tabPanel('Inference - 3', value = 'tab_infer3_home',

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
				h5('One Way ANOVA')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_3_anova',
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
				h5('Levene Test')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_3_levene',
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
				h5("Cochran's Q Test")
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'inf_menu_3_cochran',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3)
		)
	)

)