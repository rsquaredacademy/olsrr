tabPanel('EDA', value = 'tab_eda_home',

	fluidPage(

		fluidRow(
			column(12, align = 'center',

				uiOutput('eda_options')

			),

			column(12, align = 'center',

				uiOutput('infer_options')

			),

			column(12, align = 'center',

				uiOutput('viz_options')

			),

			column(12, align = 'center',

				uiOutput('model_options')

			)
		)
	)
)