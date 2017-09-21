output$ui_mfitlink <- renderUI({
    if (input$mfit_select == "Residual Fit Spread Plot") {
      fluidRow(
        column(6, align = 'left',
          h4('Residual Fit Spread Plot'),
          p('Plot to detect non-linearity, influential observations and outliers.')
        ),
        column(6, align = 'right',
          actionButton(inputId='rfslink1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/ols_rfs_plot.html', '_blank')"),
          actionButton(inputId='rfslink3', label="Demo", icon = icon("video-camera"),
            onclick ="window.open('http://google.com', '_blank')")
        )
      )
    } else if (input$mfit_select == "Part & Partial Correlations") {
      fluidRow(
                column(6, align = 'left',
                  h4('Part & Partial Correlations'),
                  p('Zero-order, part and partial correlations.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='corlink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/ols_correlations.html', '_blank')"),
                  actionButton(inputId='corlink3', label="Demo", icon = icon("video-camera"),
                    onclick ="window.open('http://google.com', '_blank')")
                )
              )
    } else if (input$mfit_select == "Observed vs Fitted Plot") {
      fluidRow(
                column(6, align = 'left',
                  h4('Observed vs Fitted Plot'),
                  p('Plot of observed vs fitted values to assess the fit of the model.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='ovsplink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/ols_ovsp_plot.html', '_blank')"),
                  actionButton(inputId='ovsplink3', label="Demo", icon = icon("video-camera"),
                    onclick ="window.open('http://google.com', '_blank')")
                )
              )
    } else if (input$mfit_select == "Lack of Fit F Test") {
      fluidRow(
                column(6, align = 'left',
                  h4('Lack of Fit F Test'),
                  p('Assess how much of the error in prediction is due to lack of model fit.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='lfitlink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/dffits_plot.html', '_blank')"),
                  actionButton(inputId='lfitlink3', label="Demo", icon = icon("video-camera"),
                    onclick ="window.open('http://google.com', '_blank')")
                )
              )
    } else if (input$mfit_select == "Diagnostics Panel") {
      fluidRow(
        column(6, align = 'left',
          h4('Diagnostics Panel'),
          p('Panel of plots for regression diagnostics.')
        ),
        column(6, align = 'right',
          actionButton(inputId='dpanelink1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/ols_diagnostic_panel.html', '_blank')"),
          actionButton(inputId='dpanelink3', label="Demo", icon = icon("video-camera"),
            onclick ="window.open('http://google.com', '_blank')")
        )
      )
    } 
})

output$ui_mfitfmla <- renderUI({
    if (input$mfit_select == "Residual Fit Spread Plot") {
      fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("rfs_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("rfs_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )
    } else if (input$mfit_select == "Part & Partial Correlations") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("corr_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("corr_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$mfit_select == "Observed vs Fitted Plot") {
      fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("ovsp_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("ovsp_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )
    } else if (input$mfit_select == "Lack of Fit F Test") {
      fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("lfit_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("lfit_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )
    } else if (input$mfit_select == "Diagnostics Panel") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("dpanel_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("dpanel_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } 
})

output$ui_mfitsubmit <- renderUI({
    if (input$mfit_select == "Residual Fit Spread Plot") {
      fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_rfsplot', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_rfsplot", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              )
    } else if (input$mfit_select == "Part & Partial Correlations") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_corr', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_corr", "Click here to correlations.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$mfit_select == "Observed vs Fitted Plot") {
      fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_ovsplot', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_ovsplot", "Click here to view plot.",
                              "bottom", options = list(container = "body")))
              )
    } else if (input$mfit_select == "Lack of Fit F Test") {
      fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_lfit', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_lfit", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              )
    } else if (input$mfit_select == "Diagnostics Panel") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_dpanel', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_dpanel", "Click here to view panel.",
                        "bottom", options = list(container = "body")))
      )
    } 
})

output$ui_mfitprev <- renderUI({
  if (input$mfit_select == "Residual Fit Spread Plot") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'rfs_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("rfs_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  } else if (input$mfit_select == "Part & Partial Correlations") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'corr_use_prev', label = '',
          value = FALSE),
        bsTooltip("corr_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$mfit_select == "Observed vs Fitted Plot") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'ovsp_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("ovsp_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  } else if (input$mfit_select == "Lack of Fit F Test") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'lfit_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("lfit_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  } else if (input$mfit_select == "Diagnostics Panel") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'dpanel_use_prev', label = '',
          value = FALSE),
        bsTooltip("dpanel_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } 
})


output$ui_mfitout <- renderUI({
  if (input$mfit_select == "Residual Fit Spread Plot") {
    fluidRow(
      br(),
      column(12, align = 'center', plotOutput('mfitrfs', height = '500px'))
    )
  } else if (input$mfit_select == "Part & Partial Correlations") {
    fluidRow(
      br(),
      column(12, align = 'center', verbatimTextOutput('mfitcorr'))
    )
  } else if (input$mfit_select == "Observed vs Fitted Plot") {
    fluidRow(
      br(),
      column(12, align = 'center', plotOutput('mfitovfp', height = '500px'))
    )
  } else if (input$mfit_select == "Lack of Fit F Test") {
    fluidRow(
      br(),
      column(12, align = 'center', verbatimTextOutput('mfitlfit'))
    )
  } else if (input$mfit_select == "Diagnostics Panel") {
    fluidRow(
      br(),
      column(12, align = 'center', plotOutput('mfitdpanel', height = '2500px'))
    )
  } 
})

d_rfs_mod <- eventReactive(input$submit_rfsplot, {
  k <- lm(input$rfs_fmla, data = final_split$train)
  if (input$rfs_use_prev) {
    out <- ols_rfs_plot(all_use_n())
  } else {
    out <- ols_rfs_plot(k)
  }
  out
})

d_corr_mod <- eventReactive(input$submit_corr, {
  k <- lm(input$corr_fmla, data = final_split$train)
  if(input$corr_use_prev) {
    out <- ols_correlations(all_use_n())
  } else {
    out <- ols_correlations(k)
  }
  out  
})

d_ovsp_mod <- eventReactive(input$submit_ovsplot, {
  k <- lm(input$ovsp_fmla, data = final_split$train) 
  if (input$ovsp_use_prev) {
    out <- out <- ols_ovsp_plot(all_use_n())
  } else {
    out <- ols_ovsp_plot(k)
  } 
  out   
})

d_lfit_mod <- eventReactive(input$submit_lfit, {
  k <- lm(input$lfit_fmla, data = final_split$train)
  if (input$lfit_use_prev) {
    out <- ols_pure_error_anova(all_use_n())
  } else {
    out <- ols_pure_error_anova(k)
  } 
  out 
})

d_dpanel_mod <- eventReactive(input$submit_dpanel, {
  k <- lm(input$dpanel_fmla, data = final_split$train)
  if(input$dpanel_use_prev) {
    out <- ols_diagnostic_panel(all_use_n())
  } else {
    out <- ols_diagnostic_panel(k)
  } 
  out 
})


output$mfitrfs <- renderPlot({
  d_rfs_mod()
})

output$mfitcorr <- renderPrint({
  print(d_corr_mod())
})

output$mfitovfp <- renderPlot({
  d_ovsp_mod()
})

output$mfitlfit <- renderPrint({
  print(d_lfit_mod())
})

output$mfitdpanel <- renderPlot({
  d_dpanel_mod()
})



