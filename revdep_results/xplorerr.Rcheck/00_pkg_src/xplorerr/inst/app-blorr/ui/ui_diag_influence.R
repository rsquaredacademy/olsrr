tabPanel('Influence Diagnostics', value = 'tab_diag_influence',

	fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('Influence Diagnostics')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='infllink', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_plot_diag_influence.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("infl_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("infl_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'infl_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("infl_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_infl', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_infl", "Click here to view influence diagnostics plots.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        column(12, align = 'center', plotOutput('infl_out', height = '2000px'))
    )

  )


)