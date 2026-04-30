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

			column(6, align = 'center',
				actionButton(
					inputId = 'orders_data',
					label = 'Transaction Data',
					width = '200px',
					onclick ="window.open('https://rfm.rsquaredacademy.com/reference/rfm_data_orders.html', 'newwindow', 'width=800,height=600')"
				)
			),

			column(6, align = 'center',
				actionButton(
					inputId = 'customer_data',
					label = 'Customer Data',
					width = '200px',
					onclick ="window.open('https://rfm.rsquaredacademy.com/reference/rfm_data_customer.html', 'newwindow', 'width=800,height=600')"
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