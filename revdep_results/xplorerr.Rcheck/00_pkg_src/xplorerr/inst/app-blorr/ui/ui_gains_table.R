tabPanel('Lift Chart', value = 'tab_gains_table',

	fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('Gains Table & Lift Chart')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='liftlink', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_gains_table.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("lift_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("lift_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'lift_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("lift_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),

    fluidRow(
      column(2, align = 'right', br(), h5('Use test data:')),
      column(2, align = 'left', br(),
             checkboxInput(inputId = 'lift_use_test_data', label = '',
                           value = FALSE),
             bsTooltip("conf_use_test_data", "Use the test/validation data.",
                       "left", options = list(container = "body"))
      )
    ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_lift', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_lift", "Click here to view gains table and lift chart.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        uiOutput('lift_title'),
        # column(12, align = 'center', h4('Regression Result')),
        hr(),
        column(12, align = 'center', verbatimTextOutput('gains_table_out'))
    ),

    fluidRow(
        br(),
        column(2),
        column(8, align = 'center', plotOutput('lift_out')),
        column(2)
    )

  )

)