tabPanel('Forward Selection', value = 'tab_varsel_forward',

	fluidPage(

    br(),

    fluidRow(

      column(6, align = 'left',
        h4('Forward Selection')
      ),

      column(6, align = 'right',
        actionButton(inputId='varsel_fe', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_forward_selection.html', '_blank')")
      )

    ),

    hr(),

    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("varsel_fe_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("varsel_fe_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Trace:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'trace_varsel_fe', label = '',
	                         value = FALSE),
	           bsTooltip("trace_varsel_fe", "Trace selection steps.",
	                     "left", options = list(container = "body"))
	    )
	  ),

    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_varsel_fe', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_varsel_fe", "Click here to view forward selection results.",
                      "bottom", options = list(container = "body")))
    ),

    fluidRow(
        br(),
        column(2),
        column(10, align = 'left', verbatimTextOutput('regress_varsel_fe'))
    )

  )


)