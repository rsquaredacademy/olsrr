tabPanel("Home", value = "tab_welcome", icon = icon('home'),

	fluidPage(

		br(),

    fluidRow(
      column(2),
      column(3, align = 'left', 
        br(),

        h3('RFM Analysis'),

        p('RFM (recency, frequency, monetary) analysis is a behavior based technique 
        	used to segment customers by examining their transaction history such as'),

        tags$ul(
			    tags$li("how recently a customer has purchased (recency)"), 
			    tags$li("how often they purchase (frequency)"), 
			    tags$li("how much the customer spends (monetary)")
			  ),

				p('It is based on the marketing axiom that 80% of your business comes from 20% of 
				your customers. RFM helps to identify customers who are more likely to respond 
				to promotions by segmenting them into various categories.')

      ),

      column(2),
      column(4, align = 'center', 
      	br(),
      	br(),
      	br(),
        img(src = 'rfm_main.png', width = '360px', height = '250px')
      ),
      column(1)
    ),

    br(),
    br(),
    br(),

    fluidRow(
    	column(12, align = "center",
    		actionButton(inputId='welcomebutton', label="Get Started", icon = icon("long-arrow-right"))
    	)
    ),

    fluidRow(hr()),

    # fluidRow(
    # 	column(12, align = "center", h4("Quick Demo"))
    # ),

    # fluidRow(
    #   column(12, align = 'center',
    #     div(style = "height:550px;",
    #       br(),
    #       br(),
    #       tags$iframe(width="760", height="515", src="https://www.youtube.com/embed/aM0bjrYCvv8?rel=0&autoplay=0")
    #     )
    #   )
    # ),

    br(),
    br(),

    fluidRow(
    	column(12, align = "center", h4("Data Sources"))
    ),

    fluidRow(
    	column(12, align = "center",
    		p("There are a few data sets available on the internet which can be used for learning RFM analysis
    			and we list them below:")
    	)
    ),

    fluidRow(
    	column(12, align = "center", a("Data Source 1", href = "http://shiny.rstudio.com/", target = "_blank")),
    	br(),
    	column(12, align = "center", a("Data Source 2", href = "http://shiny.rstudio.com/", target = "_blank"))
    ),

    br(),
    br(),
    br()
   )

)