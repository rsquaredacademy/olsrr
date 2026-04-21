tabPanel('Runs Test', value = 'tab_runs',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('Runs Test for Randomness'),
        p('Tests whether the observations are serially independent i.e. whether 
          they occur in a random order, by counting how many runs there are above 
          and below a threshold. By default, the median is used as the threshold. 
          A small number of runs indicates positive serial correlation; a large 
          number indicates negative serial correlation.')
      ),
      column(6, align = 'right',
        actionButton(inputId='runslink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_runs_test.html', '_blank')"),
        actionButton(inputId='runslink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=H12KM_uHbWc', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(2, align = 'right', br(), h5('Variable:')),
      column(4, align = 'left',
          selectInput("var_runs", label = '', width = '250px',
                          choices = "", selected = ""),
          bsTooltip("var_runs", "Select a variable.",
                    "left", options = list(container = "body"))),
      column(1, align = 'right', br(), h5('Drop:')),
      column(3, align = 'left',
        selectInput('runs_drop', '', width = '250px',
          choices = c("TRUE" = TRUE, "FALSE" = FALSE),
          selected = "FALSE"),
        bsTooltip("runs_drop", "Drop values equal to threshold.",
          "bottom", options = list(container = "body")))
    ),
    fluidRow(
      column(2, align = 'right', br(), h5('Split:')),
      column(2, align = 'left',
          selectInput("runs_split", label = '',
            choices = c("TRUE" = TRUE, "FALSE" = FALSE),
            selected = "FALSE"),
          bsTooltip("runs_split", "Recode data in binary format.",
                    "left", options = list(container = "body"))),
      column(1, align = 'right', br(), h5('Mean:')),
      column(2, align = 'left',
        selectInput('runs_mean', '',
          choices = c("TRUE" = TRUE, "FALSE" = FALSE),
          selected = "FALSE"),
        bsTooltip("runs_mean", "Use mean as the threshold.",
          "bottom", options = list(container = "body"))),
      column(1, align = 'right', br(), h5('Threshold')),
      column(2, align = 'left',
        numericInput('runs_thold', label = '',
          min = 0, step = 1, value = NA),
        bsTooltip("runs_thold", "threshold to be used for counting runs, specify
         0 if data is coded as a binary.",
          "bottom", options = list(container = "body")))
    ),
    fluidRow(
        column(12, align = 'center',
        br(),
        br(),
        actionButton(inputId = 'submit_runs', label = 'Submit', width = '120px', icon = icon('check')),
        bsTooltip("submit_runs", "Click here to view result.",
                      "bottom", options = list(container = "body")))
    ),
    fluidRow(
        br(),
        column(12, align = 'center',
            verbatimTextOutput('runs_out')
        )
    )
  )
)
