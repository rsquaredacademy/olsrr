tabPanel('Select Variables', value = 'tab_selvar',

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('Select Variables'),
        p('Click on Yes to select variables.')
      ),
      column(6, align = 'right',
        actionButton(inputId='selvarlink', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('http://google.com', '_blank')")
      )
    ),
    hr(),

		fluidRow(
			column(12, align = 'center',
				h4('Do you want to select variables?')
			)
		),

		fluidRow(
			
			column(6, align = 'right',
				actionButton(
					inputId = 'button_selvar_yes',
					label = 'Yes',
					width = '120px'
				)
			),

			column(6, align = 'left',
				actionButton(
					inputId = 'button_selvar_no',
					label = 'No',
					width = '120px'
				)
			)

		),

		fluidRow(
			br(),
			br(),
			uiOutput('show_sel_button')
		),

		fluidRow(
			uiOutput('sub_sel_button')
		)

	)

)