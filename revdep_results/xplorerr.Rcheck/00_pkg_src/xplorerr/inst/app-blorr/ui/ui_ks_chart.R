tabPanel('KS Chart', value = 'tab_ks_chart',

	fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('KS Chart')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='kschartlink', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_ks_chart.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("ks_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("ks_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'ks_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("ks_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),

    fluidRow(
      column(2, align = 'right', br(), h5('Use test data:')),
      column(2, align = 'left', br(),
             checkboxInput(inputId = 'ks_use_test_data', label = '',
                           value = FALSE),
             bsTooltip("ks_use_test_data", "Use the test/validation data.",
                       "left", options = list(container = "body"))
      )
    ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_ks', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_ks", "Click here to view KS chart.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        column(2),
        column(8, align = 'center', plotOutput('ks_out')),
        column(2)
    )

  )

)