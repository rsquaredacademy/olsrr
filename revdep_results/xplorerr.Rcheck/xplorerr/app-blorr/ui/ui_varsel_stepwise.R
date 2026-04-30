tabPanel('Stepwise Selection', value = 'tab_varsel_stepwise',

		fluidPage(

    br(),

    fluidRow(

      column(6, align = 'left',
        h4('Stepwise Selection')
      ),

      column(6, align = 'right',
        actionButton(inputId='varsel_se', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_stepwise_selection.html', '_blank')")
      )

    ),

    hr(),

    fluidRow(
      column(2, align = 'right', br(), h5('Model Formula:')),
      column(10, align = 'left',
          textInput("varsel_se_fmla", label = '', width = '660px',
                          value = ""),
          bsTooltip("varsel_se_fmla", "Specify model formula",
                    "left", options = list(container = "body")))
    ),

    fluidRow(
	    column(2, align = 'right', br(), h5('Trace:')),
	    column(2, align = 'left', br(),
	           checkboxInput(inputId = 'trace_varsel_se', label = '',
	                         value = FALSE),
	           bsTooltip("trace_varsel_se", "Trace selection steps.",
	                     "left", options = list(container = "body"))
	    )
	  ),

    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_varsel_se', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_varsel_se", "Click here to view stepwise selection results.",
                      "bottom", options = list(container = "body")))
    ),

    fluidRow(
        br(),
        column(2),
        column(10, align = 'left', verbatimTextOutput('regress_varsel_se'))
    )

  )

)