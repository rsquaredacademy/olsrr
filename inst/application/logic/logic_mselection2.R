output$ui_mselectlink <- renderUI({
  if (input$mselect == "All Possible") {
    fluidRow(
                column(6, align = 'left',
                  h4('All Subset Regression'),
                  p('Fits all regressions involving one regressor, two regressors,
                    three regressors, and so on. It tests all possible subsets of
                    the set of potential independent variables.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='allsub1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_step_all_possible.html', '_blank')")
                )
              )
  } else if (input$mselect == "Best Subset") {
    fluidRow(
                column(6, align = 'left',
                  h4('Best Subset Regression'),
                  p("Select the subset of predictors that do the best at meeting
                    some well-defined objective criterion, such as having the
                    largest R2 value or the smallest MSE, Mallow's Cp or AIC.")
                ),
                column(6, align = 'right',
                  actionButton(inputId='bestsub1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_step_best_subset.html', '_blank')")
                )
              )

  } else if (input$mselect == "Stepwise") {
    fluidRow(
                column(6, align = 'left',
                  h4('Stepwise Regression'),
                  p('Build regression model from a set of candidate predictor
                    variables by entering and removing predictors based on p
                    values, in a stepwise manner until there is no variable
                    left to enter or remove any more.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='stepwise1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https:/olsrr.rsquaredacademy.com/reference/ols_step_both_p.html', '_blank')")
                )
              )

  } else if (input$mselect == "Forward") {
    fluidRow(
                column(6, align = 'left',
                  h4('Stepwise Forward Regression'),
                  p('Build regression model from a set of candidate predictor
                    variables by entering predictors based on p values, in a
                    stepwise manner until there is no variable left to enter
                    any more.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='stepf1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_step_forward_p.html', '_blank')")
                )
              )

  } else if (input$mselect == "Backward") {
    fluidRow(
                column(6, align = 'left',
                  h4('Stepwise Backward Regression'),
                  p('Build regression model from a set of candidate predictor
                    variables by removing predictors based on p values, in a
                    stepwise manner until there is no variable left to remove
                    any more.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='stepb1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_step_backward_p.html', '_blank')")
                )
              )

  } else if (input$mselect == "stepAIC Forward") {
    fluidRow(
                column(6, align = 'left',
                  h4('stepAIC Forward Regression'),
                  p('Build regression model from a set of candidate predictor
                    variables by entering predictors based on Akaike Information
                    Criteria, in a stepwise manner until there is no variable
                    left to enter any more.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='stepaicf1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_step_forward_aic.html', '_blank')")
                )
              )

  } else if (input$mselect == "stepAIC Backward") {
    fluidRow(
                column(6, align = 'left',
                  h4('stepAIC Backward Regression'),
                  p('Build regression model from a set of candidate predictor
                    variables by removing predictors based on Akaike Information
                    Criteria, in a stepwise manner until there is no variable
                    left to remove any more.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='stepaicb1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_step_backward_aic.html', '_blank')")
                )
              )

  } else if (input$mselect == "stepAIC Both") {
    fluidRow(
                column(6, align = 'left',
                  h4('stepAIC(Both) Regression'),
                  p('Build regression model from a set of candidate predictor
                    variables by entering and removing predictors based on Akaike
                    Information Criteria, in a stepwise manner until there is no
                    variable left to enter or remove any more.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='stepaicbo1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('olsrr.rsquaredacademy.com/reference/ols_step_both_aic.html', '_blank')")
                )
              )
  }
})

output$ui_mselectfmla <- renderUI({
  if (input$mselect == "All Possible") {
    fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("allsub_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("allsub_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )

  } else if (input$mselect == "Best Subset") {
    fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("bestsub_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("bestsub_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )

  } else if (input$mselect == "Stepwise") {
    fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("stepwise_fmla", label = '', width = '860px',
                                    value = ""),
                    bsTooltip("stepwise_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )

  } else if (input$mselect == "Forward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("forward_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("forward_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )

  } else if (input$mselect == "Backward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("backward_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("backrward_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Forward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("aicforward_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("aicforward_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Backward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("aicbackward_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("aicbackrward_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Both") {
    fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("aicboth_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("aicboth_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )
  }
})

output$ui_mselectprev <- renderUI({
  if (input$mselect == "All Possible") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'all_use_model', label = '',
                    value = FALSE),
                  bsTooltip("all_use_model", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )

  } else if (input$mselect == "Best Subset") {

    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'best_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("best_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  } else if (input$mselect == "Stepwise") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'step_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("step_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )

  } else if (input$mselect == "Forward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'stepf_use_model', label = '',
                    value = FALSE),
                  bsTooltip("stepf_use_model", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )

  } else if (input$mselect == "Backward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'stepb_use_model', label = '',
                    value = FALSE),
                  bsTooltip("stepb_use_model", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )

  } else if (input$mselect == "stepAIC Forward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'stepaicf_use_model', label = '',
                    value = FALSE),
                  bsTooltip("stepaicf_use_model", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )

  } else if (input$mselect == "stepAIC Backward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'stepaicb_use_model', label = '',
                    value = FALSE),
                  bsTooltip("stepaicb_use_model", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )

  } else if (input$mselect == "stepAIC Both") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'saicbo_use_model', label = '',
                    value = FALSE),
                  bsTooltip("saicbo_use_model", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  }
})

output$ui_mselectrow1 <- renderUI({
  if (input$mselect == "All Possible") {

  } else if (input$mselect == "Best Subset") {

  } else if (input$mselect == "Stepwise") {
    fluidRow(
                column(2, align = 'right', br(), h5('Prob (Enter):')),
                column(2, align = 'left',
                    numericInput("stepwise_pent", label = '', width = '200px',
                                    value = 0.3, min = 0, max = 1, step = 0.01),
                    bsTooltip("stepwise_pent", "Minimum p value for adding variable to model.",
                              "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Prob (Removal):')),
                column(2, align = 'left',
                  numericInput("stepwise_prem", label = '', width = '200px',
                    value = 0.3, min = 0, max = 1, step = 0.01),
                  bsTooltip("stepwise_prem", "Maximum p value for removing variable from model.",
                    "left", options = list(container = "body"))),
                column(2, align = 'right', br(), h5('Details:')),
                column(2, align = 'left',
                  selectInput('stepwise_details', '',
                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                    selected = "FALSE"),
                  bsTooltip("stepwise_details", "Print model selection details.",
                    "left", options = list(container = "body")))
              )

  } else if (input$mselect == "Forward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Prob (Enter):')),
                column(4, align = 'left',
                    numericInput("forward_pent", label = '', width = '200px',
                                    value = 0.3, min = 0, max = 1, step = 0.01),
                    bsTooltip("forward_pent", "Minimum p value for adding variable to model.",
                              "left", options = list(container = "body")))
              )

  } else if (input$mselect == "Backward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Prob (Removal):')),
                column(4, align = 'left',
                    numericInput("backward_prem", label = '', width = '200px',
                                    value = 0.3, min = 0, max = 1, step = 0.01),
                    bsTooltip("backward_prem", "Minimum p value for removing variables from model.",
                              "left", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Forward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Details:')),
                column(4, align = 'left',
                  selectInput('aicforward_details', '', width = '200px',
                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                    selected = "FALSE"),
                  bsTooltip("aicforward_details", "Print model selection details.",
                    "left", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Backward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Details:')),
                column(4, align = 'left',
                  selectInput('aicbackward_details', '', width = '200px',
                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                    selected = "FALSE"),
                  bsTooltip("aicbackward_details", "Print model selection details.",
                    "left", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Both") {
    fluidRow(
                column(2, align = 'right', br(), h5('Details:')),
                column(4, align = 'left',
                  selectInput('aicboth_details', '', width = '200px',
                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                    selected = "FALSE"),
                  bsTooltip("aicboth_details", "Print model selection details.",
                    "left", options = list(container = "body")))
              )
  }
})

output$ui_mselectrow2 <- renderUI({
  if (input$mselect == "All Possible") {

  } else if (input$mselect == "Best Subset") {

  } else if (input$mselect == "Stepwise") {

  } else if (input$mselect == "Forward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Details:')),
                column(4, align = 'left',
                  selectInput('forward_details', '', width = '200px',
                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                    selected = "FALSE"),
                  bsTooltip("forward_details", "Print model selection details.",
                    "left", options = list(container = "body")))
              )

  } else if (input$mselect == "Backward") {
    fluidRow(
                column(2, align = 'right', br(), h5('Details:')),
                column(4, align = 'left',
                  selectInput('backward_details', '', width = '200px',
                    choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                    selected = "FALSE"),
                  bsTooltip("backward_details", "Print model selection details.",
                    "left", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Forward") {


  } else if (input$mselect == "stepAIC Backward") {

  } else if (input$mselect == "stepAIC Both") {

  }
})

output$ui_mselectsubmit <- renderUI({
  if (input$mselect == "All Possible") {
    fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_allsub', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_allsub", "Click here to view all subsets regression.",
                                "bottom", options = list(container = "body")))
              )

  } else if (input$mselect == "Best Subset") {
    fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_bestsub', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_bestsub", "Click here to view best subsets regression.",
                                "bottom", options = list(container = "body")))
              )

  } else if (input$mselect == "Stepwise") {
    fluidRow(
                    column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_stepwise', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_stepwise", "Click here to view stepwise regression.",
                                "bottom", options = list(container = "body")))
              )

  } else if (input$mselect == "Forward") {
    fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_forward', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_forward", "Click here to view best stepwise forward regression.",
                                "bottom", options = list(container = "body")))
              )

  } else if (input$mselect == "Backward") {
    fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_backward', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_backward", "Click here to view stepwise backward elimination.",
                                "bottom", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Forward") {
    fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_aicforward', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_aicforward", "Click here to view forward selection based on AIC.",
                                "bottom", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Backward") {
    fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_aicbackward', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_aicbackward", "Click here to view backward elimination based on AIC.",
                                "bottom", options = list(container = "body")))
              )

  } else if (input$mselect == "stepAIC Both") {
    fluidRow(
                  column(12, align = 'center',
                  br(),
                  br(),
                  actionButton(inputId = 'submit_aicboth', label = 'Submit', width = '120px', icon = icon('check')),
                  bsTooltip("submit_aicboth", "Click here to view stepwise regression based on AIC.",
                                "bottom", options = list(container = "body")))
              )
  }
})

output$ui_mseloutput <- renderUI({
  if (input$mselect == "All Possible") {
    fluidRow(
                  br(),
                  uiOutput('all_title1'),
                  # column(12, align = 'center', h4('All Subset Regression Result')),
                  hr(),
                  column(12, align = 'center', verbatimTextOutput('allsub_out')),
                  hr()
              )

  } else if (input$mselect == "Best Subset") {
    fluidRow(
                  br(),
                  uiOutput('best_title1'),
                  # column(12, align = 'center', h4('Best Subset Regression Result')),
                  hr(),
                  column(12, align = 'center', verbatimTextOutput('bestsub_out')),
                  hr()
              )

  } else if (input$mselect == "Stepwise") {
    fluidRow(
                  br(),
                  uiOutput('swise_title1'),
                  # column(12, align = 'center', h4('Stepwise Regression Result')),
                  hr(),
                  column(12, align = 'center', verbatimTextOutput('stepwise_out')),
                  hr()
              )

  } else if (input$mselect == "Forward") {
    fluidRow(
                  br(),
                  uiOutput('forward_title1'),
                  # column(12, align = 'center', h4('Stepwise Forward Regression Result')),
                  hr(),
                  column(12, align = 'center', verbatimTextOutput('forward_out')),
                  hr()
              )

  } else if (input$mselect == "Backward") {
    fluidRow(
                  br(),
                  uiOutput('backward_title1'),
                  # column(12, align = 'center', h4('Stepwise Backward Regression Result')),
                  hr(),
                  column(12, align = 'center', verbatimTextOutput('backward_out')),
                  hr()
              )

  } else if (input$mselect == "stepAIC Forward") {
    fluidRow(
                  br(),
                  uiOutput('sforward_title1'),
                  # column(12, align = 'center', h4('stepAIC Forward Regression Result')),
                  hr(),
                  column(12, align = 'center', verbatimTextOutput('aicforward_out')),
                  hr()
              )

  } else if (input$mselect == "stepAIC Backward") {
    fluidRow(
                  br(),
                  uiOutput('sbackward_title1'),
                  # column(12, align = 'center', h4('stepAIC Backward Regression Result')),
                  hr(),
                  column(12, align = 'center', verbatimTextOutput('aicbackward_out')),
                  hr()
              )

  } else if (input$mselect == "stepAIC Both") {
    fluidRow(
                  br(),
                  uiOutput('sboth_title1'),
                  # column(12, align = 'center', h4('stepAIC Regression Result')),
                  hr(),
                  column(12, align = 'center', verbatimTextOutput('aicboth_out')),
                  hr()
              )
  }
})

output$ui_mselplot <- renderUI({
  if (input$mselect == "All Possible") {
    fluidRow(
                  br(),
                  uiOutput('all_title2'),
                  # column(12, align = 'center', h4('All Subset Regression Plot')),
                  hr(),
                  br(),
                  column(12, plotOutput('allsub_plot', height = '1500px'))
              )

  } else if (input$mselect == "Best Subset") {
    fluidRow(
                  br(),
                  uiOutput('best_title2'),
                  # column(12, align = 'center', h4('Best Subset Regression Plot')),
                  hr(),
                  br(),
                  column(12, plotOutput('bestsub_plot', height = '1500px'))
              )

  } else if (input$mselect == "Stepwise") {
    fluidRow(
                  br(),
                  uiOutput('swise_title2'),
                  # column(12, align = 'center', h4('Stepwise Regression Plot')),
                  hr(),
                  br(),
                  column(12, plotOutput('stepwise_plot', height = '1500px'))
              )

  } else if (input$mselect == "Forward") {
    fluidRow(
                  br(),
                  uiOutput('forward_title2'),
                  # column(12, align = 'center', h4('Stepwise Forward Regression Plot')),
                  hr(),
                  br(),
                  column(12, plotOutput('forward_plot', height = '1500px'))
              )

  } else if (input$mselect == "Backward") {
    fluidRow(
                  br(),
                  uiOutput('backward_title2'),
                  # column(12, align = 'center', h4('Stepwise Backward Regression Plot')),
                  hr(),
                  br(),
                  column(12, plotOutput('backward_plot', height = '1500px'))
              )

  } else if (input$mselect == "stepAIC Forward") {
    fluidRow(
                  br(),
                  uiOutput('sforward_title2'),
                  # column(12, align = 'center', h4('stepAIC Forward Regression Plot')),
                  hr(),
                  br(),
                  column(12, plotOutput('aicforward_plot', height = '500px'))
              )

  } else if (input$mselect == "stepAIC Backward") {
    fluidRow(
                  br(),
                  uiOutput('sbackward_title2'),
                  # column(12, align = 'center', h4('stepAIC Backward Regression Plot')),
                  hr(),
                  br(),
                  column(12, plotOutput('aicbackward_plot', height = '500px'))
              )

  } else if (input$mselect == "stepAIC Both") {
    fluidRow(
                  br(),
                  uiOutput('sboth_title2'),
                  # column(12, align = 'center', h4('stepAIC Regression Plot')),
                  hr(),
                  br(),
                  column(12, plotOutput('aicboth_plot', height = '500px'))
              )
  }
})

# # main regression
# all_use_n <- reactive({
#   k <- model()
#   object <- k$model
#   formul <- formula(object)
#   data <- eval(object$call$data)
#   n <- lm(formul, data = data)
#   n
# })

# all subset
# d_allsub <- eventReactive(input$submit_allsub, {
#   # validate(need((input$allsub_fmla != ''), 'Please specify model'))
#     data <- final_split$train
# })

allsub_model <- eventReactive(input$submit_allsub, {
  if (input$all_use_model) {
    ols_step_all_possible(all_use_n())
  } else {
    model <- lm(input$allsub_fmla, data = final_split$train)
    ols_step_all_possible(model)
  }
})

a1_title <- eventReactive(input$submit_allsub, {
  column(12, align = 'center', h4('All Subset Regression'))
})

output$all_title1 <- renderUI({
  a1_title()
})

a2_title <- eventReactive(input$submit_allsub, {
  column(12, align = 'center', h4('All Subset Regression Plot'))
})

output$all_title2 <- renderUI({
  a2_title()
})


output$allsub_out <- renderPrint({
  allsub_model()
})

# output$allsub_plot <- renderPlot({
#   plot(allsub_model())
# })

bestsub_model <- eventReactive(input$submit_bestsub, {
  if (input$best_use_prev) {
    ols_step_best_subset(all_use_n())
  } else {
    data <- final_split$train
    model <- lm(input$bestsub_fmla, data = data)
    ols_step_best_subset(model)
  }
})

b1_title <- eventReactive(input$submit_bestsub, {
  column(12, align = 'center', h4('Best Subset Regression'))
})

output$best_title1 <- renderUI({
  b1_title()
})

b2_title <- eventReactive(input$submit_allsub, {
  column(12, align = 'center', h4('Best Subset Regression Plot'))
})

output$best_title2 <- renderUI({
  b2_title()
})

output$bestsub_out <- renderPrint({
  bestsub_model()
})

output$bestsub_plot <- renderPlot({
  plot(bestsub_model())
})


stepwise_model <- eventReactive(input$submit_stepwise, {
  if (input$step_use_prev) {
    ols_step_both_p(all_use_n(), input$stepwise_pent, input$stepwise_prem,
      as.logical(input$stepwise_details))
  } else {
    model <- lm(input$stepwise_fmla, data = final_split$train)
    ols_step_both_p(model, input$stepwise_pent, input$stepwise_prem,
      as.logical(input$stepwise_details))
  }
})

s1_title <- eventReactive(input$submit_stepwise, {
  column(12, align = 'center', h4('Stepwise Regression'))
})

output$swise_title1 <- renderUI({
  s1_title()
})

s2_title <- eventReactive(input$submit_stepwise, {
  column(12, align = 'center', h4('Stepwise Regression Plot'))
})

output$swise_title2 <- renderUI({
  s2_title()
})


output$stepwise_out <- renderPrint({
  print(stepwise_model())
})

output$stepwise_plot <- renderPlot({
  plot(stepwise_model())
})

forward_model <- eventReactive(input$submit_forward, {
  if (input$stepf_use_model) {
    ols_step_forward_p(all_use_n(), input$forward_pent,
      as.logical(input$forward_details))
  } else {
    model <- lm(input$forward_fmla, data = final_split$train)
    ols_step_forward_p(model, input$forward_pent,
      as.logical(input$forward_details))
  }
})

f1_title <- eventReactive(input$submit_forward, {
  column(12, align = 'center', h4('Stepwise Forward Regression'))
})

output$forward_title1 <- renderUI({
  f1_title()
})

f2_title <- eventReactive(input$submit_forward, {
  column(12, align = 'center', h4('Stepwise Forward Regression Plot'))
})

output$forward_title2 <- renderUI({
  f2_title()
})


output$forward_out <- renderPrint({
  print(forward_model())
})

output$forward_plot <- renderPlot({
  plot(forward_model())
})


backward_model <- eventReactive(input$submit_backward, {
  if (input$stepb_use_model) {
    ols_step_backward_p(all_use_n(), input$backward_prem,
      as.logical(input$backward_details))
  } else {
    model <- lm(input$backward_fmla, data = final_split$train)
    ols_step_backward_p(model, input$backward_prem,
      as.logical(input$backward_details))
  }
})

ba1_title <- eventReactive(input$submit_backward, {
  column(12, align = 'center', h4('Stepwise Backward Regression'))
})

output$backward_title1 <- renderUI({
  ba1_title()
})

ba2_title <- eventReactive(input$submit_backward, {
  column(12, align = 'center', h4('Stepwise Backward Regression Plot'))
})

output$backward_title2 <- renderUI({
  ba2_title()
})


output$backward_out <- renderPrint({
  print(backward_model())
})

output$backward_plot <- renderPlot({
  plot(backward_model())
})


aicforward_model <- eventReactive(input$submit_aicforward, {
  if (input$stepaicf_use_model) {
    ols_step_forward_aic(all_use_n(), as.logical(input$aicforward_details))
  } else {
    model <- lm(input$aicforward_fmla, data = final_split$train)
    ols_step_forward_aic(model, as.logical(input$aicforward_details))
  }
})

af1_title <- eventReactive(input$submit_aicforward, {
  column(12, align = 'center', h4('stepAIC Forward Regression'))
})

output$sforward_title1 <- renderUI({
  af1_title()
})

af2_title <- eventReactive(input$submit_aicforward, {
  column(12, align = 'center', h4('stepAIC Forward Regression Plot'))
})

output$sforward_title2 <- renderUI({
  af2_title()
})


output$aicforward_out <- renderPrint({
  print(aicforward_model())
})

output$aicforward_plot <- renderPlot({
  plot(aicforward_model())
})


aicbackward_model <- eventReactive(input$submit_aicbackward, {
  if (input$stepaicb_use_model) {
    ols_step_backward_aic(all_use_n(),
      as.logical(input$aicbackward_details))
  } else {
    model <- lm(input$aicbackward_fmla, data = final_split$train)
    ols_step_backward_aic(model,
      as.logical(input$aicbackward_details))
  }
})

ab1_title <- eventReactive(input$submit_aicbackward, {
  column(12, align = 'center', h4('stepAIC Backward Regression'))
})

output$sbackward_title1 <- renderUI({
  ab1_title()
})

ab2_title <- eventReactive(input$submit_aicbackward, {
  column(12, align = 'center', h4('stepAIC Backward Regression Plot'))
})

output$sbackward_title2 <- renderUI({
  ab2_title()
})

output$aicbackward_out <- renderPrint({
  print(aicbackward_model())
})

output$aicbackward_plot <- renderPlot({
  plot(aicbackward_model())
})


aicboth_model <- eventReactive(input$submit_aicboth, {
  if (input$saicbo_use_model) {
    ols_step_both_aic(all_use_n(),
      as.logical(input$aicboth_details))
  } else {
    model <- lm(input$aicboth_fmla, data = final_split$train)
    ols_step_both_aic(model,
      as.logical(input$aicboth_details))
  }
})

bo1_title <- eventReactive(input$submit_aicboth, {
  column(12, align = 'center', h4('stepAIC Regression'))
})

output$sboth_title1 <- renderUI({
  bo1_title()
})

bo2_title <- eventReactive(input$submit_aicboth, {
  column(12, align = 'center', h4('stepAIC Regression Plot'))
})

output$sboth_title2 <- renderUI({
  bo2_title()
})

output$aicboth_out <- renderPrint({
  print(aicboth_model())
})

output$aicboth_plot <- renderPlot({
  plot(aicboth_model())
})
