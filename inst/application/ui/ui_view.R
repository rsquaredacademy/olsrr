tabPanel('View', value = 'tab_view',
	fluidPage(

		br(),

		fluidRow(
			column(6, align = 'left',
			  actionButton(inputId='view2getdata', label=" Get Data", icon = icon("long-arrow-left"))         
			),
			column(6, align = 'right',
			  actionButton(inputId='view2analyze', label="Analyze Data", icon = icon("long-arrow-right"))         
			)
		),

		hr(),

		fluidRow(
			dataTableOutput(outputId = "table")
			)
		)
)
