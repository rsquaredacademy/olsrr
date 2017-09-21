tabPanel("Select Data", value = "tab_seldata",

  fluidPage(

    fluidRow(
      column(6, align = 'left',
        h4('Select Data Set'),
        p('Select a data set from the drop down box and click on submit.')
      ),
      column(6, align = 'right',
        actionButton(inputId='seldatalink', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('http://google.com', '_blank')")
      )
    ),
    hr(),
  	
  	fluidRow(
  		
  		column(12, align = "center",
  			selectInput(
  				inputId = "sel_data",
  				label = "Select a data set:",
          choices = '',
  				# choices = c('csv', 'excel', 'json', 'spss', 'stata', 'sas'),
  				selected = '', 
  				width = '200px',
  			)
  		)
  	),

    fluidRow(
        column(12, align = 'center',
        br(),
        actionButton(inputId = 'submit_seldata', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_seldata", "Click here to select data.",
                      "bottom", options = list(container = "body")))
    )
  )

)