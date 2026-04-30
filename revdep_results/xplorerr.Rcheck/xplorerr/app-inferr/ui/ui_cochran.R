tabPanel('Cochran Test', value = 'tab_cochran',
  fluidPage(
    fluidRow(
      column(8, align = 'left',
        h4("Cochran's Q Test"),
        p('Test if the proportions of 3 or more dichotomous variables are equal in the same population.')
      ),
      column(4, align = 'right',
        actionButton(inputId='cochranlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_cochran_qtest.html', '_blank')"),
        actionButton(inputId='cochranlink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=rLix3uAvqck', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(2, align = 'right', br(), h5('Select Variables:')),
      column(10, align = 'left',
          selectInput("var_cochran", label = '', width = '660px',
                          choices = "", selected = "",
                          multiple = TRUE, selectize = TRUE),
          bsTooltip("var_cochran", "Select variables.",
                    "left", options = list(container = "body")))
    ),
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_cochran', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_cochran", "Click here to view result.",
                      "bottom", options = list(container = "body")))
    ),
    fluidRow(
        br(),
        column(12, align = 'center',
            verbatimTextOutput('cochran_out')
        )
    )
  )
)
