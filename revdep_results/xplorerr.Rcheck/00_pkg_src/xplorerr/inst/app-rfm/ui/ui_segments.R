tabPanel("Segmentation", value = "tab_rfm_segments",

	fluidPage(

		fluidRow(
      column(6, align = 'left',
        h4('Generate Segments'),
        p("Classify customers based on the individual recency, frequency and monetary scores. Those
          customers who do not fall into any of the below segments will be classified as 'Others'.")
      )
    ),

    hr(),

		fluidRow(

			column(6, align = "right", br(), h5("Number of Segments:")),
			column(6, align = "left",
				numericInput(
					inputId = "n_segments", 
					label = "",
					min = 1, 
					max = 10,
					step = 1, 
					value = 5 
				)
			)

		),

		hr(),
		
		fluidRow(
			column(1),
      column(2, h5('Segment')),
      column(3, h5('Recency Score')),
      column(3, h5('Frequency Score')),
      column(3, h5('Monetary Score'))
    ),

		column(12, uiOutput('segment_prep')),

    fluidRow(

      column(12, align = 'center',
        br(),
        actionButton(inputId = "button_create_segments", label = "Generate Segments", icon = icon('thumbs-up')),
        br(),
        br()
      )
    ),

    fluidRow(
    	br(),
    	dataTableOutput("segment_out")
    )

	)

)