tabPanel('Normal Distribution', value = 'tab_norm',

  fluidPage(
    fluidRow(
      column(8, align = 'left',
        h4('Normal Distribution'),
        p('Visualize how changes in mean and standard deviation affect the shape
         of the normal distribution. Compute/visualize quantiles out of given 
         probability and probability from a given quantile.')
      ),
      column(4, align = 'right',
        actionButton(inputId='ndistlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/dist_norm_plot.html', '_blank')")
      )
    ),
    hr(),
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Distribution Shape',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('norm_m', 'Mean', value = 0, step = 0.1),
                numericInput('norm_sd', 'Standard Deviation', value = 1,
                 min = 0, step = 0.1)
              )
            ),
            column(8,
              plotOutput('norm_shape', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Probability',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('nprob_p', 'Percentile', value = 2, min = 0,
                 step = 1),
                numericInput('nprob_m', 'Mean', value = 0, step = 0.1),
                numericInput('nprob_sd', 'Standard Deviation', value = 1,
                 min = 0, step = 0.1),
                selectInput('nprob_tail', 'Tail',
                  choices = c('lower', 'upper'), selected = 'lower'
                )
              )
            ),
            column(8,
              plotOutput('nprob_plot', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Percentile',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('nperc_p', 'Probability', value = 0.3, min = 0,
                  step = 0.01, max = 1
                ),
                numericInput('nperc_m', 'Mean', value = 0, step = 0.1),
                numericInput('nperc_sd', 'Standard Deviation', value = 1,
                 min = 0, step = 0.1),
                selectInput('nperc_tail', 'Tail',
                  choices = c('lower', 'upper'), selected = 'lower'
                )
              )
            ),
            column(8,
              plotOutput('nperc_plot', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    )
  )

)
