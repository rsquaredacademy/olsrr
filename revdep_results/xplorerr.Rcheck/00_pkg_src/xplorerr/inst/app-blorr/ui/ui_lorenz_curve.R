tabPanel('Lorenz Curve', value = 'tab_lorenz_curve',

	fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('Lorenz Curve & Gini Index')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='lorenzlink', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_lorenz_curve.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("lorenz_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("lorenz_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'lorenz_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("lorenz_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_lorenz', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_lorenz", "Click here to view Lorenz curve.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        column(2),
        column(8, align = 'center', plotOutput('lorenz_out')),
        column(2)
    )

  )


)