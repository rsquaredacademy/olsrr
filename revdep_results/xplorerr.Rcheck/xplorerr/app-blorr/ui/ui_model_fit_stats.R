tabPanel('Model Fit Stats - I', value = 'tab_model_fit_stats',

	fluidPage(
	  
	  br(),
	  
	  fluidRow(
    	   
	    column(6, align = 'left',
	       h4('Model Fit Statistics')
	    ),
	    column(6, align = 'right',
	       actionButton(inputId='cdiaglink1', label="Help", icon = icon("question-circle"),
           onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_model_fit_stats.html', '_blank')")
	    )
	     
	  ),
	  
	  hr(),
	  fluidRow(
	    column(2, align = 'right', br(), h5('Model Formula:')),
	    column(10, align = 'left',
	           textInput("mfs_fmla", label = '', width = '660px',
	                     value = ""),
	           bsTooltip("mfs_fmla", "Specify model formula",
	                     "left", options = list(container = "body")))
	  ),
	  
	  fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'mfs_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("mfs_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),
	  
	  fluidRow(
	    column(12, align = 'center',
	           br(),
	           br(),
	           actionButton(inputId = 'submit_mfs', label = 'Submit', width = '120px', icon = icon('check')),
	           bsTooltip("submit_mfs", "Click here to view model fit statistics.",
	                     "bottom", options = list(container = "body")))
	  ),
	  
	  fluidRow(
	    br(),
	    column(12, align = 'center', verbatimTextOutput('mfs'))
	  )

	)

)