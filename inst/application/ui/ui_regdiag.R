tabPanel('Variable Contribution', value = 'tab_regvarcont',
  fluidPage(
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Added Variable Plot',
            fluidPage(
              br(),
              fluidRow(
                column(6, align = 'left',
                  h4('Added Variable Plot'),
                  p('Added variable plot provides information about the marginal 
                    importance of a predictor variable, given the other predictor 
                    variables already in the model. It shows the marginal importance 
                    of the variable in reducing the residual variability.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='advarlink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_added_variable.html', '_blank')")
                )
              ),
              hr(),
              fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("avplot_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("avplot_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'advar_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("advar_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_avplot', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_avplot", "Click here to view added variable plot.",
                              "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12,  align = 'center', verbatimTextOutput('avplotdata'))
              ),
              fluidRow(
                  br(),
                  column(12,  align = 'center', plotOutput('avplot'))
              )
            )
          ),
          tabPanel('Residual Plus Component Plot',
            fluidPage(
              br(),
              fluidRow(
                column(6, align = 'left',
                  h4('Residual Plus Component Plot'),
                  p('The residual plus component plot indicates whether any 
                    non-linearity is present in the relationship between 
                    response and predictor variables and can suggest possible 
                    transformations for linearizing the data.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='regcprp1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_comp_plus_resid.html', '_blank')")
                )
              ),
              hr(),
              fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("cprp_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("cprp_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'cprp_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("cprp_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_cprp_plot', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_cprp_plot", "Click here to view regression result.",
                              "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12,  align = 'center', plotOutput('cprplot'))
              )
            )
          ),
          tabPanel('Residual vs Regressor Plot',
            fluidPage(
              br(),
              fluidRow(
                column(6, align = 'left',
                  h4('Residual vs Regressor Plot'),
                  p('Graph to determine whether we should add a new predictor 
                    to the model already containing other predictors. The residuals 
                    from the model is regressed on the new predictor and if the plot 
                    shows non random pattern, you should consider adding the new 
                    predictor to the model.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='rvsrlink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_regressor.html', '_blank')")
                )
              ),
              hr(),
              fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("resreg_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("resreg_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Select Regressor:')),
                column(2, align = 'left',
                    selectInput('resreg_var', 'Select Variable',
                                                choices = "", selected = ""),
                    bsTooltip("resreg_var", "Select a variable not in the model.",
                              "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'resreg_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("resreg_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_resreg_plot', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_resreg_plot", "Click here to view regression result.",
                              "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12,  align = 'center', plotOutput('rvsrplot'))
              )
            )
          )        
        )
      )
    )
  )
)
