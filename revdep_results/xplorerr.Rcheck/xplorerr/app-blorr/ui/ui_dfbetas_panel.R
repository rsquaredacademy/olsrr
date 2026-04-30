tabPanel('DFBETAs Panel', value = 'tab_dfbetas_panel',

		fluidPage(

    br(),
    
    fluidRow(
    
      column(6, align = 'left',
        h4('DFBETAs Panel')
      ),
    
      column(6, align = 'right',
        actionButton(inputId='dfbetaslink', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_plot_dfbetas_panel.html', '_blank')")
      )
    
    ),

    hr(),
    
    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("dfbetas_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("dfbetas_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Use previous model:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'dfbetas_use_prev', label = '',
	                         value = FALSE),
	           bsTooltip("dfbetas_use_prev", "Use model from Regression Tab.",
	                     "left", options = list(container = "body"))
	    )
	  ),
    
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_dfbetas', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_dfbetas", "Click here to view dfbetas panel plots.",
                      "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
        br(),
        column(12, align = 'center', plotOutput('dfbetas_out', height = '1000px'))
    )

  )

)