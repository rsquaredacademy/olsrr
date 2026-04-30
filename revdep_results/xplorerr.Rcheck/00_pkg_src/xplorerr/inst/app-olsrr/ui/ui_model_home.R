tabPanel('Model Building', value = 'tab_model_home',

	fluidPage(

		fluidRow(
			br(),
			column(12, align = 'center',
				h3('What do you want to do?')
			),
			br(),
			br()
		),

		fluidRow(

			column(12),

			br(),

			column(3),

			column(4, align = 'left',
				h5('Regression')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_reg_click',
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
				h5('Residual Diagnostics')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_resdiag_click',
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
				h5('Heteroskedasticity Tests')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_het_click',
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
				h5('Collinearity Diagnostics')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_coldiag_click',
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
				h5('Measures of Influence')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_infl_click',
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
				h5('Model Fit Assessment')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_fit_click',
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
				h5('Variable Contribution')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_varcontrib_click',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3)

		)
	)

)