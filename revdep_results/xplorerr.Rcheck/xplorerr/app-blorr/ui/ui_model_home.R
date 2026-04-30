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
				h5('Bivariate Analysis')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_bivar_click',
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
				h5('Regression')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_regress_click',
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
				h5('Model Fit Statistics')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_fitstat_click',
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
				h5('Variable Selection')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_varsel_click',
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
				h5('Model Validation')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'model_validation_click',
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

			column(3)

		)
	)

)
