tabPanel('Sample', value = 'tab_sample', icon = icon('random'),

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('Sample Data'),
        p('Click on Yes to create a random sample of data.')
      ),
      column(6, align = 'right',
        actionButton(inputId='samplelink', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=X8b0beNJ64A#t=03m47s', '_blank')")
      )
    ),
    hr(),

		fluidRow(
			column(12, align = 'center',
				h4('Draw a random sample of the data?')
			)
		),

		fluidRow(
			
			column(6, align = 'right',
				actionButton(
					inputId = 'button_sample_yes',
					label = 'Yes',
					width = '120px'
				)
			),

			column(6, align = 'left',
				actionButton(
					inputId = 'button_sample_no',
					label = 'No',
					width = '120px'
				)
			)

		),

		br(),
		br(),

		fluidRow(

			column(12, align = 'center',

				uiOutput('samp_yes_no')

			),

			br(),
			br()

			# column(12, align = 'center',

			# 	uiOutput('samp_no_yes')

			# )

		),

		fluidRow(

			br(),
			br(),
			uiOutput('samp_per_option')

		),

		fluidRow(

			uiOutput('samp_obs_option')

		)

	)

)