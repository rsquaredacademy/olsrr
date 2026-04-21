tabPanel('Confusion Matrix', value = 'tab_conf_matrix',

	fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('Confusion Matrix')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='conf1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_confusion_matrix.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("conf_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("conf_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
      column(2, align = 'right', br(), h5('Cutoff:')),
      column(10, align = 'left',
          numericInput("conf_cutoff", label = '', min = 0, max = 1, 
          	value = 0.5, step = 0.01),
          bsTooltip("conf_cutoff", "Specify cutoff",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'conf_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("conf_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),

    fluidRow(
      column(2, align = 'right', br(), h5('Use test data:')),
      column(2, align = 'left', br(),
             checkboxInput(inputId = 'conf_use_test_data', label = '',
                           value = FALSE),
             bsTooltip("conf_use_test_data", "Use the test/validation data.",
                       "left", options = list(container = "body"))
      )
    ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_conf', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_conf", "Click here to view confusion matrix.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        uiOutput('conf_title'),
        # column(12, align = 'center', h4('Regression Result')),
        hr(),
        column(12, align = 'center', verbatimTextOutput('conf_out'))
    )
  )

)