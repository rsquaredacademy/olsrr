tabPanel('Backward Elimination', value = 'tab_varsel_backward',

	fluidPage(

    br(),

    fluidRow(

      column(6, align = 'left',
        h4('Backward Elimination')
      ),

      column(6, align = 'right',
        actionButton(inputId='varsel_be', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_backward_elimination.html', '_blank')")
      )

    ),

    hr(),

    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("varsel_be_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("varsel_be_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Trace:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'trace_varsel_be', label = '',
	                         value = FALSE),
	           bsTooltip("trace_varsel_be", "Trace elimination steps.",
	                     "left", options = list(container = "body"))
	    )
	  ),

    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_varsel_be', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_varsel_be", "Click here to view backward elimination result.",
                      "bottom", options = list(container = "body")))
    ),

    fluidRow(
        br(),
        column(2),
        column(10, align = 'left', verbatimTextOutput('regress_varsel_be'))
    )

  )

)