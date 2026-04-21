tabPanel('F Distribution', value = 'tab_f',

  fluidPage(
    fluidRow(
      column(7, align = 'left',
        h4('F Distribution'),
        p('Visualize how changes in degrees of freedom affect the shape of the F 
          distribution. Compute/visualize quantiles out of given probability and 
          probability from a given quantile.')
      ),
      column(5, align = 'right',
        actionButton(inputId='fdistlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/dist_f_plot.html', '_blank')")
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
                numericInput('f_numdf', 'Numerator Degrees of freedom',
                  value = 4, min = 1, step = 1
                ),
                numericInput('f_dendf', 'Denominator Degrees of freedom',
                  value = 5, min = 1, step = 1
                ),
                selectInput('f_norm', 'Normal Distribution',
                  choices = c('TRUE' = TRUE, 'FALSE' = FALSE),
                  selected = 'FALSE'
                )
              )
            ),
            column(8,
              plotOutput('f_shape', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Probability',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('fprob_p', 'Percentile', value = 2,
                  min = 0, step = 1
                ),
                numericInput('fprob_numdf', 'Numerator Degrees of freedom',
                  value = 4, min = 1, step = 1
                ),
                numericInput('fprob_dendf', 'Denominator Degrees of freedom',
                  value = 5, min = 1, step = 1
                ),
                selectInput('fprob_tail', 'Tail',
                  choices = c('lower', 'upper'), selected = 'lower'
                )
              )
            ),
            column(8,
              plotOutput('fprob_plot', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          ),
          tabPanel('Find Percentile',
            column(4,
              column(6, align = 'center',
                br(),
                br(),
                numericInput('fperc_p', 'Probability', value = 0.3, min = 0,
                  step = 0.01, max = 1
                ),
                numericInput('fperc_numdf', 'Numerator Degrees of freedom',
                  value = 4, min = 1, step = 1
                ),
                numericInput('fperc_dendf', 'Denominator Degrees of freedom',
                  value = 5, min = 1, step = 1
                ),
                selectInput('fperc_tail', 'Tail',
                  choices = c('lower', 'upper'), selected = 'lower'
                )
              )
            ),
            column(8,
              plotOutput('fperc_plot', height = '400px') %>% 
                shinycssloaders::withSpinner()
            )
          )
        )
      )
    )
  )

)
