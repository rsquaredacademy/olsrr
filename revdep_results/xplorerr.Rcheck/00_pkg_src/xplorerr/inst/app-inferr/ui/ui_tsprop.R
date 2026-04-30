tabPanel('Two Sample Proportion', value = 'tab_tsproptest',
  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('Two Sample Proportion Test'),
        p('Tests on the equality of proportions using large-sample statistics. 
          It tests that a sample has the same proportion within two independent 
          groups or two samples have the same proportion.')
      ),
      column(6, align = 'right',
        actionButton(inputId='tsproplink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://inferr.rsquaredacademy.com/reference/infer_ts_prop_test.html', '_blank')"),
        actionButton(inputId='tsproplink3', label="Demo", icon = icon("video-camera"),
          onclick ="window.open('https://www.youtube.com/watch?v=0HhnnSzj1JY', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Using Variables',
            fluidPage(
              fluidRow(
                column(2, align = 'right', br(), h5('Variable 1:')),
                column(2, align = 'left',
                    selectInput("var_tsproptest1", label = '', width = '200px',
                                    choices = "", selected = ""),
                    bsTooltip("var_tsproptest1", "Select a variable.",
                              "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Variable 2:')),
                column(2, align = 'left',
                selectInput("var_tsproptest2", label = '', width = '200px',
                  choices = "", selected = ""),
                bsTooltip("var_tsproptest2", "Select a variable.",
                  "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Alternative:')),
                column(2, align = 'left',
                  selectInput('tsproptest_type', '',
                    choices = c("both", "less", "greater", "all"),
                    selected = "both"),
                bsTooltip("tsproptest_type", "Alternative hypothesis",
                  "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_tsproptest', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_tsproptest", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('tsproptest_out')
                  )
              )
            )
          ),
          tabPanel('Using Groups',
            fluidPage(
              fluidRow(
                column(2, align = 'right', br(), h5('Variable:')),
                column(2, align = 'left',
                    selectInput("var_tsproptestg1", label = '', width = '200px',
                                    choices = "", selected = ""),
                    bsTooltip("var_tsproptestg1", "Select a variable.",
                              "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Grouping Variable:')),
                column(2, align = 'left',
                selectInput("var_tsproptestg2", label = '', width = '200px',
                  choices = "", selected = ""),
                bsTooltip("var_tsproptestg2", "Select a  grouping variable.",
                  "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Alternative:')),
                column(2, align = 'left',
                  selectInput('tsproptestg_type', '',
                    choices = c("both", "less", "greater", "all"),
                    selected = "both"),
                bsTooltip("tsproptestg_type", "Alternative hypothesis",
                  "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_tsproptestg', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_tsproptestg", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('tsproptestg_out')
                  )
              )
            )
          ),
          tabPanel('Calculator',
            fluidPage(
              fluidRow(
                column(2, align = 'right', br(), h5('n1:')),
                column(2, align = 'left',
                    numericInput("n1_tspropcalc", label = '', width = '120px',
                                    min = 0, step = 1, value = 1),
                    bsTooltip("n1_tspropcalc", "Number of observations",
                              "left", options = list(container = "body"))),
                column(1, align = 'right', br(), h5('n2:')),
                column(2, align = 'left',
                  numericInput("n2_tspropcalc", label = '', width = '120px',
                    min = 0, step = 1, value = 1),
                  bsTooltip("n2_tspropcalc", "Number of observations",
                    "left", options = list(container = "body"))),
                    column(2, align = 'right', br(), h5('Alternative:')),
                    column(3, align = 'left',
                      selectInput('tspropcalc_type', '', width = '120px',
                        choices = c("both", "less", "greater", "all"),
                        selected = "both"),
                    bsTooltip("tspropcalc_type", "Alternative hypothesis",
                            "bottom", options = list(container = "body")))
              ),
              fluidRow(
                column(4, align = 'right', br(), h5('Proportion 1:')),
                column(2, align = 'left',
                  numericInput('prop_tspropcalc1', label = '', width = '150px',
                    min = 0, value = 0.5, step = 0.1, max = 1),
                  bsTooltip("prop_tspropcalc1", "Proportion 1",
                      "bottom", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Proportion 2:')),
                column(4, align = 'left',
                  numericInput('prop_tspropcalc2', label = '', width = '150px',
                    min = 0, value = 0.5, step = 0.1, max = 1),
                  bsTooltip("prop_tspropcalc2", "Proportion 2",
                    "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_tspropcalc', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_tspropcalc", "Click here to view test result.",
                                "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, align = 'center',
                      verbatimTextOutput('tspropcalc_out')
                  )
              )
            )
          )
        )
      )
    )
  )
)
