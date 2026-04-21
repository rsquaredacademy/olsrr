tabPanel('Libraries', value = 'tab_viz_lib',

	fluidPage(

		fluidRow(

			column(12, align = 'center',
				uiOutput('vizlib_bar')
			),

			column(12, align = 'center',
				uiOutput('vizlib_line')
			),

			column(12, align = 'center',
				uiOutput('vizlib_pie')
			),

			column(12, align = 'center',
				uiOutput('vizlib_scatter')
			),

			column(12, align = 'center',
				uiOutput('vizlib_hist')
			),

			column(12, align = 'center',
				uiOutput('vizlib_box')
			)

		)

	)

)
