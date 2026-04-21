tabPanel('Transaction Data', value = 'tab_rfm_transaction_score',

	# check box for transcation or customer data
	fluidPage(

    fluidRow(

      column(6, align = 'left',
          h4('RFM Analysis'),
          p('Recency, frequency and monetary value analysis for transaction level data i.e. each row represents 
            a transaction/order.')
        ),
        column(6, align = 'right',
          actionButton(inputId='rvsp1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://rfm.rsquaredacademy.com/reference/rfm_table_order.html', '_blank')")
        )

    ),

    hr(),

		fluidRow(

      column(2, align = "right",
        br(),
        h5("Unique ID:")
      ),

      column(2, align = "left",
        selectInput("rfm_customer_id_t", label = '',
          choices = "", selected = "", width = '150px'
        ),
        bsTooltip("rfm_customer_id_t", "Select the variable representing the unique id of the customer.",
          "bottom", options = list(container = "body")
        )
      ),
      column(2, align = "right",
        br(),
        h5("Order Date:")
      ),

      column(2, align = "left",
        selectInput("rfm_order_date_t", label = '',
          choices = "", selected = "", width = '150px'
        ),
        bsTooltip("rfm_order_date_t", "Select the variable representing the date of the order/transaction.",
          "bottom", options = list(container = "body")
        )
      ),
      column(2, align = "right",
        br(),
        h5("Revenue:")
      ),

      column(2, align = "left",
        selectInput("rfm_revenue_t", label = '',
          choices = "", selected = "", width = '150px'
        ),
        bsTooltip("rfm_revenue_t", "Select the variable representing the total revenue from the transaction.",
          "bottom", options = list(container = "body")
        )
      )
    ),

    fluidRow(
    	column(2, align = "right",
        br(),
        h5("Analysis Date:")
      ),

      column(2, align = "left",
        dateInput("rfm_analysis_date_t", label = '', width = '150px'),
        bsTooltip("rfm_analysis_date_t", "Select the date of analysis.",
          "bottom", options = list(container = "body")
        )
      ),
    	column(2, align = "right",
        br(),
        h5("Recency Bins:")
      ),

      column(2, align = "left",
        numericInput("rfm_recency_bins_t", label = '',
          min = 1, step = 1, value = 5, width = '150px'
        ),
        bsTooltip("rfm_recency_bins_t", "Specify the number of bins for recency.",
          "bottom", options = list(container = "body")
        )
      ),
    	column(2, align = "right",
        br(),
        h5("Frequency Bins:")
      ),

      column(2, align = "left",
        numericInput("rfm_frequency_bins_t", label = '',
          min = 1, step = 1, value = 5, width = '150px'
        ),
        bsTooltip("rfm_recency_bins_t", "Specify the number of bins for recency.",
          "bottom", options = list(container = "body")
        )
      )
    ),

    fluidRow(
      column(2, align = "right",
        br(),
        h5("Monetary Bins:")
      ),

      column(2, align = "left",
        numericInput("rfm_monetary_bins_t", label = '',
          min = 1, step = 1, value = 5, width = '150px'
        ),
        bsTooltip("rfm_monetary_bins_t", "Specify the number of bins for monetary value.",
          "bottom", options = list(container = "body")
        )
      )
    ),

    br(),

    fluidRow(
    	column(12, align = "center",
    		actionButton(inputId = 'submit_rfm_transaction_score', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_rfm_transaction_score", "Click here to view RFM score.",
                              "bottom", options = list(container = "body"))
    	)
    ),

    fluidRow(
      br(),
      dataTableOutput('rfm_transaction_score_out')
    )

	)

)