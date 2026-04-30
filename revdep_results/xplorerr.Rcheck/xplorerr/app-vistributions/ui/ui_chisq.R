tabPanel('Chisquare Distribution', value = 'tab_chisq',

  fluidPage(
    fluidRow(
      column(8, align = 'left',
        h4('Chi Square Distribution'),
        p('Visualize how changes in degrees of freedom affect the shape of the
         chi square distribution. Compute/visualize quantiles out of given 
         probability and probability from a given quantile.')

      ),
      column(4, align = 'right',
        actionButton(inputId='chidistlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/dist_chi_plot.html', '_blank')")
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
                numericInput('chisq_df', 'Degrees of freedom', value = 4,
                  min = 1, step = 1
                ),
                selectInput('chisq_norm', 'Normal Distribution',
                  choices = c('TRUE' = TRUE, 'FALSE' = FALSE),
                  selected = 'FALSE'
                )
              )
            ),
            column(8,
              plotOutput('chisq_shape', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Probability',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('chiprob_p', 'Percentile', value = 2,
                  min = 0, step = 1
                ),
                numericInput('chiprob_df', 'Degrees of freedom', value = 4,
                  min = 1, step = 1
                ),
                selectInput('chiprob_tail', 'Tail',
                  choices = c('lower', 'upper'), selected = 'lower'
                )
              )
            ),
            column(8,
              plotOutput('chiprob_plot', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Percentile',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('chiperc_p', 'Probability', value = 0.3, min = 0,
                  step = 0.01, max = 1
                ),
                numericInput('chiperc_df', 'Degrees of freedom', value = 4,
                  min = 1, step = 1
                ),
                selectInput('chiperc_tail', 'Tail',
                  choices = c('lower', 'upper'), selected = 'lower'
                )
              )
            ),
            column(8,
              plotOutput('chiperc_plot', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    )
  )

)
