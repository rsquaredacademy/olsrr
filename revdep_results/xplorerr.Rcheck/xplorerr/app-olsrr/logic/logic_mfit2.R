output$ui_mfitlink <- renderUI({
    if (input$mfit_select == "Residual Fit Spread Plot") {
      fluidRow(
        column(6, align = 'left',
          h4('Residual Fit Spread Plot'),
          p('Plot to detect non-linearity, influential observations and outliers.')
        ),
        column(6, align = 'right',
          actionButton(inputId='rfslink1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_fit_spread.html', '_blank')")
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
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_correlations.html', '_blank')")
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
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_obs_fit.html', '_blank')")
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
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_pure_error_anova.html', '_blank')")
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
            onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_diagnostics.html', '_blank')")
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
  if (input$rfs_use_prev) {
    ols_plot_resid_fit_spread(all_use_n())
  } else {
    k <- lm(input$rfs_fmla, data = final_split$train)
    ols_plot_resid_fit_spread(k)
  }
})

d_corr_mod <- eventReactive(input$submit_corr, {
  if(input$corr_use_prev) {
    ols_correlations(all_use_n())
  } else {
    k <- lm(input$corr_fmla, data = final_split$train)
    ols_correlations(k)
  }
})

d_ovsp_mod <- eventReactive(input$submit_ovsplot, {
  if (input$ovsp_use_prev) {
    ols_plot_obs_fit(all_use_n())
  } else {
    k <- lm(input$ovsp_fmla, data = final_split$train)
    ols_plot_obs_fit(k)
  }
})

d_lfit_mod <- eventReactive(input$submit_lfit, {
  if (input$lfit_use_prev) {
    ols_pure_error_anova(all_use_n())
  } else {
    k <- lm(input$lfit_fmla, data = final_split$train)
    ols_pure_error_anova(k)
  }
})

d_dpanel_mod <- eventReactive(input$submit_dpanel, {
  if(input$dpanel_use_prev) {
    ols_plot_diagnostics(all_use_n())
  } else {
    k <- lm(input$dpanel_fmla, data = final_split$train)
    ols_plot_diagnostics(k)
  }
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



