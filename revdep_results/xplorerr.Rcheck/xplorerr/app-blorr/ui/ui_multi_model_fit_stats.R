tabPanel('Model Fit Stats - 2', value = 'tab_multi_model_fit_stats',

	fluidPage(
	  
	  br(),
	  
	  fluidRow(
    	   
	    column(6, align = 'left',
	       h4('Multiple Model Fit Statistics')
	    ),
	    column(6, align = 'right',
	       actionButton(inputId='cdiaglink1', label="Help", icon = icon("question-circle"),
           onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_multi_model_fit_stats.html', '_blank')")
	    )
	     
	  ),
	  
	  hr(),

	  fluidRow(
	    column(2, align = 'right', br(), h5('Model 1:')),
	    column(10, align = 'left',
	           textInput("mmfs_fmla_1", label = '', width = '660px',
	                     value = ""),
	           bsTooltip("mmfs_fmla_1", "Specify model formula",
	                     "left", options = list(container = "body")))
	  ),

	  fluidRow(
	    column(2, align = 'right', br(), h5('Model 2:')),
	    column(10, align = 'left',
	           textInput("mmfs_fmla_2", label = '', width = '660px',
	                     value = ""),
	           bsTooltip("mmfs_fmla_2", "Specify model formula",
	                     "left", options = list(container = "body")))
	  ),
	  
	  fluidRow(
	    column(12, align = 'center',
	           br(),
	           br(),
	           actionButton(inputId = 'submit_mmfs', label = 'Submit', width = '120px', icon = icon('check')),
	           bsTooltip("submit_mmfs", "Click here to view multiple model fit statistics.",
	                     "bottom", options = list(container = "body")))
	  ),
	  
	  fluidRow(
	    br(),
	    column(12, align = 'center', verbatimTextOutput('mmfs'))
	  )

	)

)