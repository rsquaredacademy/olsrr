tabPanel('ROC Curve', value = 'tab_roc_curve',

	fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('ROC Curve')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='roclink', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_roc_curve.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("roc_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("roc_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'roc_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("roc_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),

    fluidRow(
      column(2, align = 'right', br(), h5('Use test data:')),
      column(2, align = 'left', br(),
             checkboxInput(inputId = 'roc_use_test_data', label = '',
                           value = FALSE),
             bsTooltip("roc_use_test_data", "Use the test/validation data.",
                       "left", options = list(container = "body"))
      )
    ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_roc', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_roc", "Click here to view ROC curve.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        column(2),
        column(8, align = 'center', plotOutput('roc_out')),
        column(2)
    )

  )


)