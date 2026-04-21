tabPanel('Inference', value = 'tab_infer_home',

	fluidPage(

		fluidRow(

							column(12),

							br(),

							column(12, align = 'center',
								h5('What do you want to do?')
							),

							br(),
							br(),
							br(),

							column(3),

							column(4, align = 'left',
								h5('Comparison of one group to a hypothetical value')
							),

							column(2, align = 'left',
								actionButton(inputId = 'button_infer_home_1', label = 'Click Here', width = '120px')
							),

							column(3),

							br(),
							br(),
							br(),

							column(3),

							column(4, align = 'left',
								h5('Comparison of two groups')
							),

							column(2, align = 'left',
								actionButton(inputId = 'button_infer_home_2', label = 'Click Here', width = '120px')
							),

							column(3),

							br(),
							br(),
							br(),

							column(3),

							column(4, align = 'left',
								h5('Comparison of three or more groups')
							),

							column(2, align = 'left',
								actionButton(inputId = 'button_infer_home_3', label = 'Click Here', width = '120px')
							),

							column(3)

						)

						
	)

)