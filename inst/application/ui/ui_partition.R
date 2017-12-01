tabPanel('Partition', value = 'tab_partition', icon = icon('cut'),

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('Partition Data'),
        p('Click on Yes to partition data into training and test set.')
      ),
      column(6, align = 'right',
        actionButton(inputId='partitionlink', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=X8b0beNJ64A#t=04m24s', '_blank')")
      )
    ),
    hr(),

		fluidRow(
			column(12, align = 'center',
				h4('Do you want to partition data into training set and testing set?')
			)
		),

		fluidRow(
			
			column(6, align = 'right',
				actionButton(
					inputId = 'button_split_yes',
					label = 'Yes',
					width = '120px'
				)
			),

			column(6, align = 'left',
				actionButton(
					inputId = 'button_split_no',
					label = 'No',
					width = '120px'
				)
			)

		),

		br(),
		br(),

		fluidRow(
			uiOutput('ui_partition')
		)

	)

)