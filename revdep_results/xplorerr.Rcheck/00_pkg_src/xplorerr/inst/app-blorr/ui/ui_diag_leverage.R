tabPanel('Leverage Diagnostics', value = 'tab_diag_leverage',

		fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('Leverage Diagnostics')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='levlink', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_plot_diag_leverage.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("lev_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("lev_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'lev_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("lev_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_lev', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_lev", "Click here to view leverage diagnostics plots.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        column(12, align = 'center', plotOutput('lev_out', height = '1000px'))
    )

  )

)