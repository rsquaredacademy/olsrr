tabPanel('t Distribution', value = 'tab_t',

  fluidPage(
    fluidRow(
      column(6, align = 'left',
        h4('t Distribution'),
        p('Visualize how degrees of freedom affect the shape of t distribution. Compute/visualize quantiles out of given probability and probability 
          from a given quantile.')
      ),
      column(6, align = 'right',
        actionButton(inputId='tdistlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/dist_t_plot.html', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Distribution Shape',
            column(2,
              column(12, align = 'center',
                br(),
                br(),
                numericInput('t_df', 'Degrees of Freedom', value = 1,
                 min = 0, step = 1)
              )
            ),
            column(10,
              plotOutput('t_shape', height = '500px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Probability',
            column(2,
              column(12, align = 'center',
                br(),
                br(),
                numericInput('tprob_p', 'Percentile', value = 2, min = 0,
                  step = 1),
                numericInput('tprob_df', 'Degrees of Freedom', value = 1,
                  min = 0, step = 1),
                selectInput('tprob_tail', 'Tail',
                  choices = c('lower', 'upper', 'interval', 'both'),
                  selected = 'lower'
                )
              )
            ),
            column(10,
              plotOutput('tprob_plot', height = '500px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Percentile',
            column(2,
              column(12, align = 'center',
                br(),
                br(),
                numericInput('tperc_p', 'Probability', value = 0.3, min = 0,
                  step = 0.01, max = 1
                ),
                numericInput('tperc_df', 'Degrees of Freedom', value = 1,
                  min = 0, step = 1),
                selectInput('tperc_tail', 'Tail',
                  choices = c('lower', 'upper', 'both'),
                  selected = 'lower'
                )
              )
            ),
            column(10,
              plotOutput('tperc_plot', height = '500px') %>% 
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    )
  )

)
