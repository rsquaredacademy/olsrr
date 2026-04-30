tabPanel('Binomial Distribution', value = 'tab_binom',

  fluidPage(
    fluidRow(
      column(12,
        fluidRow(
          column(8, align = 'left',
            h4('Binomial Distribution'),
            p('Visualize how changes in number of trials and the probability of
              success affect the shape of the binomial distribution.
              Compute/visualize probability from a given quantile and quantiles
              out of given probability.')
          ),
          column(4, align = 'right',
            actionButton(inputId='binomdist1', label="Help", icon = icon("question-circle"),
              onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/dist_binom_plot.html', '_blank')")
          )
        ),
        hr(),
        tabsetPanel(type = 'tabs',
          tabPanel('Distribution Shape',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('binom_n', 'Number of trials', value = 10, min = 1,
                  step = 1
                ),
                numericInput('binom_p', 'Probability', value = 0.3, min = 0,
                  max = 1, step = 0.01
                )
              )
            ),
            column(8,
              plotOutput('binom_shape', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Probability',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('bprob_n', 'Number of trials', value = 10, min = 1,
                  step = 1
                ),
                numericInput('bprob_p', 'Probability', value = 0.3, min = 0,
                  max = 1, step = 0.01
                ),
                selectInput('bprob_tail', 'Tail',
                  choices = c('lower', 'upper', 'exact', 'interval'),
                  selected = 'lower'
                ),
                conditionalPanel(
                  condition = "input.bprob_tail != 'interval'",
                  numericInput('bprob_s', 'Success', value = 1, min = 0, step = 1)
                ),
                conditionalPanel(
                  condition = "input.bprob_tail == 'interval'",
                  numericInput('bprob_tail_1', 'Lower', value = 1, min = 0, step = 1),
                  br(),
                  numericInput('bprob_tail_2', 'Upper', value = 1, min = 0, step = 1)
                )
              )
            ),
            column(8,
              plotOutput('bprob_plot', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Percentile',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('bperc_n', 'Number of trials', value = 10, min = 1,
                  step = 1
                ),
                numericInput('bperc_p', 'Probability of Success', value = 0.3,
                  min = 0, max = 1, step = 0.01
                ),
                numericInput('bperc_tp', 'Aggregated Probability', value = 0.05,
                  min = 0, max = 0.5, step = 0.01
                ),
                selectInput('bperc_tail', 'Tail',
                  choices = c('lower', 'upper'), selected = 'lower'
                )
              )
            ),
            column(8,
              plotOutput('bperc_plot', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    )
  )

)
