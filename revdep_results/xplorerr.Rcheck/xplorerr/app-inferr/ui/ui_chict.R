tabPanel('Chi Square Association', value = 'tab_chict',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('Chi Square Test of Association'),
        p('Examine if there is a relationship between two categorical variables.')
      ),
      column(6, align = 'right',
        actionButton(inputId='chiasso1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_chisq_assoc_test.html', '_blank')"),
        actionButton(inputId='chiasso3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=p7-KcyeVl8s', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(2, align = 'right', br(), h5('Variable 1:')),
      column(2, align = 'left',
          selectInput("var_chict1", label = '', width = '200px',
                          choices = "", selected = ""),
          bsTooltip("var_chict1", "Select a variable.",
                    "left", options = list(container = "body"))),
          column(2, align = 'right', br(), h5('Variable 2:')),
          column(2, align = 'left',
            selectInput("var_chict2", label = '', width = '200px',
              choices = "", selected = ""),
            bsTooltip("var_chict2", "Select a variable.",
              "left", options = list(container = "body"))),
              column(4, align = 'center',
              br(),
              actionButton(inputId = 'submit_chict', label = 'Submit', width = '120px', icon = icon('check')),
              bsTooltip("submit_chict", "Click here to view result.",
                            "bottom", options = list(container = "body")))

    ),
    fluidRow(
        br(),
        column(12, align = 'center',
            verbatimTextOutput('chict_out')
        )
    )
  )
)
