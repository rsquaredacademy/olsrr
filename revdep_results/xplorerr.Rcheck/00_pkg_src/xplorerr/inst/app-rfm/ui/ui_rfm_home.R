tabPanel("Home", value = "tab_rfm_home",
	fluidPage(
		
		fluidRow(
			column(12, align = 'center',
				h3('What type of data do you have?')
			)
		),

		br(),
		br(),

		fluidRow(
			column(3),
			column(4, align = 'center',
				h4('Transaction Level Data'),
				p('Each row represents a transaction/order.')
			),
			column(2, align = 'left',
				br(),
				actionButton(
					inputId = 'click_transaction',
					label = 'Click Here',
					width = '100px'
				)
			),
			column(3)
		),

		br(),

		fluidRow(
			column(3),
			column(4, align = 'center',
				h4('Customer Level Data'),
				p('Each row represents transactions of a customer. The data includes 
					the number of days since the last transaction.')
			),
			column(2, align = 'left',
				br(),
				actionButton(
					inputId = 'click_customer_1',
					label = 'Click Here',
					width = '100px'
				)
			),
			column(3)
		),

		br(),

		fluidRow(
			column(3),
			column(4, align = 'center',
				h4('Customer Level Data'),
				p('Each row represents transactions of a customer. The data includes 
					the date of the latest transaction.')
			),
			column(2, align = 'left',
				br(),
				actionButton(
					inputId = 'click_customer_2',
					label = 'Click Here',
					width = '100px'
				)
			),
			column(3)
		)

	)
)