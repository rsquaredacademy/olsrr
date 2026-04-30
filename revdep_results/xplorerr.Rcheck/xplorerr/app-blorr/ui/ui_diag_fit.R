tabPanel('Fit Diagnostics', value = 'tab_diag_fit',

		fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('Fitted Values Diagnostics')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='fitlink', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_plot_diag_fit.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("fit_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("fit_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'fit_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("fit_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_fit', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_fit", "Click here to view fitted values diagnostics plots.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        column(12, align = 'center', plotOutput('fit_out', height = '1000px'))
    )

  )

)