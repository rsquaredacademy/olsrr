tabPanel('Data Sources', value = 'tab_datasources', 

	fluidPage(theme = shinytheme('cerulean'),

		includeCSS("mystyle.css"),

		fluidRow(
			column(12, align = 'center',
				h4('Use sample data or upload a file')
			)
		),

		fluidRow(

			column(6, align = 'right',
				actionButton(
					inputId = 'sample_data_yes',
					label = 'Sample Data',
					width = '120px'
				)
			),

			column(6, align = 'left',
				actionButton(
					inputId = 'upload_files_yes',
					label = 'Upload File',
					width = '120px'
				)
			)
			
		),

		br(),

		fluidRow(
			column(12, align = 'center',
				h6('The app takes a few seconds to load. Please wait for ~12 seconds.')
			)
		),

		br(),
		br(),

		fluidRow(
			uiOutput('upload_file_links')
		)		

	)

)