tabPanel('Sample Data', value = 'tab_use_sample',

	fluidPage(

		includeCSS("mystyle.css"),

		fluidRow(
			column(12, align = 'center',
				h5('Click on a sample for more information')
			)
		),

		br(),

		fluidRow(

			column(4, align = 'center',
				actionButton(
					inputId = 'german_data',
					label = 'German Credit',
					width = '200px',
					onclick ="window.open('https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)', 'newwindow', 'width=800,height=600')"
				)
			),

			column(4, align = 'center',
				actionButton(
					inputId = 'iris_data',
					label = 'Iris',
					width = '200px',
					onclick ="window.open('https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html', 'newwindow', 'width=800,height=600')"
				)
			),

			column(4, align = 'center',
				actionButton(
					inputId = 'mtcars_data',
					label = 'mtcars',
					width = '200px',
					onclick ="window.open('https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html', 'newwindow', 'width=800,height=600')"
				)
			)

		),

		fluidRow(column(12, br())),

		fluidRow(

			column(4, align = 'center',
				actionButton(
					inputId = 'mpg_data',
					label = 'mpg',
					width = '200px',
					onclick ="window.open('http://ggplot2.tidyverse.org/reference/mpg.html', 'newwindow', 'width=800,height=600')"
				)
			),

			column(4, align = 'center',
				actionButton(
					inputId = 'hsb_data',
					label = 'hsb',
					width = '200px',
					onclick ="window.open('https://blorr.rsquaredacademy.com/reference/hsb2.html', 'newwindow', 'width=800,height=600')"
				)
			),

			column(4, align = 'center',
				actionButton(
					inputId = 'diamonds_data',
					label = 'diamonds',
					width = '200px',
					onclick ="window.open('http://ggplot2.tidyverse.org/reference/diamonds.html', 'newwindow', 'width=800,height=600')"
				)
			)

		),

		br(),
		br(),
		br(),

		fluidRow(
			column(12, align = 'center',
				actionButton(
					inputId = 'use_sample_data',
					label = 'Use Sample Data',
					width = '200px'
				)
			)
		)

	)
)