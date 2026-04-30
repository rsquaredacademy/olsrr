tabPanel('Multiple 1 Way Tables', value = 'tab_mult1',
  fluidPage(
    fluidRow(
      column(8, align = 'left',
        h4('Multiple One Way Tables'),
        p('Generates multiple one way tables.')
      ),
      column(4, align = 'right',
        actionButton(inputId='multonelink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/ds_oway_tables.html', '_blank')"),
        actionButton(inputId='multonelink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=bCmKnXpGP54', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(12, align = 'center',
        verbatimTextOutput('mult1')
      )
    )
  )
)
