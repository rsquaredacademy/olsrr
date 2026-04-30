tabPanel("Others", value = "tab_viz_others",

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
				actionButton(inputId = "click_bar_others", 
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
				actionButton(inputId = "click_bar2_others", 
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
				actionButton(inputId = "click_line_others", 
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
				actionButton(inputId = "click_pie_others", 
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
				h5("Explore relationship between continuous variables")
			),
			column(2, align = 'left',
				actionButton(inputId = "click_scatter_others", 
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
				actionButton(inputId = "click_hist_others", 
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
				actionButton(inputId = "click_box_others", 
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
				actionButton(inputId = "click_box2_others", 
					label = "Box Plot 2",
					width = "120px"
				)
			),
			column(3)
		),

		fluidRow(column(6, offset = 3, hr()))

	)

)