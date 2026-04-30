tabPanel('Customer Data - I', value = 'tab_rfm_customer_score',

	# check box for transcation or customer data
	fluidPage(

    fluidRow(

      column(6, align = 'left',
          h4('RFM Analysis'),
          p('Recency, frequency and monetary value analysis for customer level data i.e. each row represents 
            transactions of a customer and the data includes the number of days since the last transaction.')
        ),
        column(6, align = 'right',
          actionButton(inputId='rvsp1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://rfm.rsquaredacademy.com/reference/rfm_table_customer.html', '_blank')")
        )

    ),

    hr(),

		fluidRow(

      column(2, align = "right",
        br(),
        h5("Unique ID:")
      ),

      column(2, align = "left",
        selectInput("rfm_customer_id_c", label = '',
          choices = "", selected = "", width = '150px'
        ),
        bsTooltip("rfm_customer_id_c", "Select the variable representing the unique id of the customer.",
          "bottom", options = list(container = "body")
        )
      ),
      column(2, align = "right",
        br(),
        h5("Orders:")
      ),

      column(2, align = "left",
        selectInput("rfm_n_transactions_c", label = '',
          choices = "", selected = "", width = '150px'
        ),
        bsTooltip("rfm_n_transactions_c", "Select the variable representing the number of orders/purchases.",
          "bottom", options = list(container = "body")
        )
      ),
      column(2, align = "right",
        br(),
        h6("Days since last transaction:")
      ),

      column(2, align = "left",
        selectInput("rfm_recency_days_c", label = '',
          choices = "", selected = "", width = '150px'
        ),
        bsTooltip("rfm_recency_days_c", "Select the variable representing the days since last transaction.",
          "bottom", options = list(container = "body")
        )
      )
    ),

    fluidRow(
    	column(2, align = "right",
        br(),
        h5("Revenue:")
      ),

      column(2, align = "left",
        selectInput("rfm_total_revenue_c", label = '',
          choices = "", selected = "", width = '150px'
        ),
        bsTooltip("rfm_total_revenue_c", "Select the variable representing the total revenue from the customer.",
          "bottom", options = list(container = "body")
        )
      ),
    	column(2, align = "right",
        br(),
        h5("Analysis Date:")
      ),

      column(2, align = "left",
        dateInput("rfm_analysis_date_c", label = '', width = '150px'),
        bsTooltip("rfm_analysis_date_c", "Select the date of analysis.",
          "bottom", options = list(container = "body")
        )
      ),
    	column(2, align = "right",
        br(),
        h5("Recency Bins:")
      ),

      column(2, align = "left",
        numericInput("rfm_recency_bins_c", label = '',
          min = 1, step = 1, value = 5, width = '150px'
        ),
        bsTooltip("rfm_recency_bins_c", "Specify the number of bins for recency.",
          "bottom", options = list(container = "body")
        )
      )
    ),

    fluidRow(
      column(2, align = "right",
        br(),
        h5("Frequency Bins:")
      ),

      column(2, align = "left",
        numericInput("rfm_frequency_bins_c", label = '',
          min = 1, step = 1, value = 5, width = '150px'
        ),
        bsTooltip("rfm_frequency_bins_c", "Specify the number of bins for frequency.",
          "bottom", options = list(container = "body")
        )
      ),
      column(2, align = "right",
        br(),
        h5("Monetary Bins:")
      ),

      column(2, align = "left",
        numericInput("rfm_monetary_bins_c", label = '',
          min = 1, step = 1, value = 5, width = '150px'
        ),
        bsTooltip("rfm_monetary_bins_c", "Specify the number of bins for monetary value",
          "bottom", options = list(container = "body")
        )
      )

    ),

    br(),

    fluidRow(
    	column(12, align = "center",
    		actionButton(inputId = 'submit_rfm_customer_score', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_rfm_customer_score", "Click here to view RFM score.",
                              "bottom", options = list(container = "body"))
    	)
    ),

    fluidRow(
      br(),
      dataTableOutput('rfm_customer_score_out') %>% 
          withSpinner()
    )

	)

)