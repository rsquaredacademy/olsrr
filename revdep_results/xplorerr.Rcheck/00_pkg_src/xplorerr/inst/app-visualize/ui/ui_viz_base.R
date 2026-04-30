tabPanel("Base", value = "tab_viz_base", 

	fluidPage(

		fluidRow(
			column(12, align = 'center',
				h3("What do you want to do?")
			)
		),

		br(),

		fluidRow(column(6, offset = 3, hr())),

		fluidRow(			
			column(3),
			column(4, align = 'left',
				h5("Compare data across categories")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_bar_base", 
					label = "Bar Chart",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr())),

		fluidRow(			
			column(3),
			column(4, align = 'left',
				h5("Compare data of sub groups across categories")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_bar2_base", 
					label = "Bar Chart 2",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr())),

		fluidRow(
			column(3),
			column(4, align = 'left',
				h5("View trends in data over time")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_line_base", 
					label = "Line Chart",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr())),

		fluidRow(
			column(3),
			column(4, align = 'left',
				h5("Display proportions")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_pie_base", 
					label = "Pie Chart",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr())),

		fluidRow(
			column(3),
			column(4, align = 'left',
				h5("Display proportions (3d)")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_pie2_base", 
					label = "Pie Chart 3d",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr())),

		fluidRow(
			column(3),
			column(4, align = 'left',
				h5("Explore relationship between continuous variables")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_scatter_base", 
					label = "Scatter Plot",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr())),

		fluidRow(
			column(3),
			column(4, align = 'left',
				h5("Understand distribution of your data")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_hist_base", 
					label = "Histogram",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr())),

		fluidRow(
			column(3),
			column(4, align = 'left',
				h5("Understand distribution and/or identify outliers")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_box_base", 
					label = "Box Plot",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr())),

		fluidRow(
			column(3),
			column(4, align = 'left',
				h5("Compare distribution across groups and/or identify outliers")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_box2_base", 
					label = "Box Plot 2",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr()))

	)

)