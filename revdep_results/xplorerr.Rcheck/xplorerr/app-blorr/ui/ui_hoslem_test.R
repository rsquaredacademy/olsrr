tabPanel('Hosmer Lemeshow Test', value = 'tab_hoslem_test',

	fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('Hosmer Lemeshow Test')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='conf1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_hosmer_lemeshow_test.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("hoslem_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("hoslem_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'hoslem_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("hoslem_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),

    fluidRow(
      column(2, align = 'right', br(), h5('Use test data:')),
      column(2, align = 'left', br(),
             checkboxInput(inputId = 'hoslem_use_test_data', label = '',
                           value = FALSE),
             bsTooltip("hoslem_use_test_data", "Use the test/validation data.",
                       "left", options = list(container = "body"))
      )
    ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_hoslem', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_hoslem", "Click here to view hosmer lemeshow test results.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        column(12, align = 'center', verbatimTextOutput('hoslem_out'))
    )
  )

)