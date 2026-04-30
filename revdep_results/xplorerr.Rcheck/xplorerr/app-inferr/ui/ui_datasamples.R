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
					inputId = 'mtcars_data',
					label = 'mtcars',
					width = '200px',
					onclick ="window.open('https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html', 'newwindow', 'width=800,height=600')"
				)
			),

			column(4, align = 'center',
				actionButton(
					inputId = 'mpg_data',
					label = 'mpg',
					width = '200px',
					onclick ="window.open('https://ggplot2.tidyverse.org/reference/mpg.html', 'newwindow', 'width=800,height=600')"
				)
			),

			column(4, align = 'center',
				actionButton(
					inputId = 'hsb_data',
					label = 'hsb',
					width = '200px',
					onclick ="window.open('https://www.rsquaredacademy.com/descriptr/reference/hsb.html', 'newwindow', 'width=800,height=600')"
				)
			)

		),

		fluidRow(column(12, br())),

		fluidRow(

			column(4, align = 'center',
				actionButton(
					inputId = 'exam_data',
					label = 'exam',
					width = '200px',
					onclick ="window.open('https://inferr.rsquaredacademy.com/reference/exam.html', 'newwindow', 'width=800,height=600')"
				)
			),

			column(4, align = 'center',
				actionButton(
					inputId = 'treatment_data',
					label = 'treatment',
					width = '200px',
					onclick ="window.open('https://www.rsquaredacademy.com/inferr/reference/treatment.html', 'newwindow', 'width=800,height=600')"
				)
			),			

			column(4, align = 'center',
				actionButton(
					inputId = 'diamonds_data',
					label = 'diamonds',
					width = '200px',
					onclick ="window.open('https://ggplot2.tidyverse.org/reference/diamonds.html', 'newwindow', 'width=800,height=600')"
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