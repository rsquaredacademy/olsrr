output$ui_resdiaglink <- renderUI({
    if (input$restrial1 == "Residual vs Predicted Plot") {
      fluidRow(
        column(6, align = 'left',
          h4('Residual vs Predicted Plot'),
          p('Plot to detect non-linearity, unequal error variances, and outliers.')
        ),
        column(6, align = 'right',
          actionButton(inputId='rvsp1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/ols_rvsp_plot.html', '_blank')"),
          actionButton(inputId='rvsp3', label="Demo", icon = icon("video-camera"),
            onclick ="window.open('http://google.com', '_blank')")
        )
      )
    } else if (input$restrial1 == "Residual Box Plot") {
      fluidRow(
        column(6, align = 'left',
          h4('Residual Box Plot')
        ),
        column(6, align = 'right',
          actionButton(inputId='rbplot1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/ols_rsd_boxplot.html', '_blank')"),
          actionButton(inputId='rbplot3', label="Demo", icon = icon("video-camera"),
            onclick ="window.open('http://google.com', '_blank')")
        )
      )
    } else if (input$restrial1 == "Residual Histogram") {
      fluidRow(
        column(6, align = 'left',
          h4('Residual Histogram'),
          p('Histogram of residuals for detecting violation of normality assumption.')
        ),
        column(6, align = 'right',
          actionButton(inputId='rhist1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/ols_rsd_hist.html', '_blank')"),
          actionButton(inputId='rhist3', label="Demo", icon = icon("video-camera"),
            onclick ="window.open('http://google.com', '_blank')")
        )
      )
    } else if (input$restrial1 == "Residual QQ Plot") {
      fluidRow(
        column(6, align = 'left',
          h4('Residual QQ Plot'),
          p('Graph for detecting violation of normality assumption.')
        ),
        column(6, align = 'right',
          actionButton(inputId='rqq1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/ols_rsd_qqplot.html', '_blank')"),
          actionButton(inputId='rqq3', label="Demo", icon = icon("video-camera"),
            onclick ="window.open('http://google.com', '_blank')")
        )
      )
    } else if (input$restrial1 == "Normality Test") {
      fluidRow(
        column(6, align = 'left',
          h4('Normality Test'),
          p('Test for detecting violation of normality assumption.')
        ),
        column(6, align = 'right',
          actionButton(inputId='resnorm1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://rsquaredacademy.github.io/olsrr/reference/ols_norm_test.html', '_blank')"),
          actionButton(inputId='resnorm3', label="Demo", icon = icon("video-camera"),
            onclick ="window.open('http://google.com', '_blank')")
        )
      )
    }
})

output$ui_resdiagfmla <- renderUI({
    if (input$restrial1 == "Residual vs Predicted Plot") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("respred_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("respred_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$restrial1 == "Residual Box Plot") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("resbox_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("resbox_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$restrial1 == "Residual Histogram") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("reshist_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("reshist_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$restrial1 == "Residual QQ Plot") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("resqq_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("resqq_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$restrial1 == "Normality Test") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("resnorm_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("resnorm_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    }
})

output$ui_resdiagsubmit <- renderUI({
    if (input$restrial1 == "Residual vs Predicted Plot") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_respred_plot', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_respred_plot", "Click here to view regression result.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$restrial1 == "Residual Box Plot") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_resbox_plot', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_resbox_plot", "Click here to view regression result.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$restrial1 == "Residual Histogram") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_reshist_plot', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_reshist_plot", "Click here to view regression result.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$restrial1 == "Residual QQ Plot") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_resqq_plot', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_resqq_plot", "Click here to view regression result.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$restrial1 == "Normality Test") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_resnorm', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_resnorm", "Click here to view normality test result.",
                        "bottom", options = list(container = "body")))
      )
    }
})

output$ui_resdiagprev <- renderUI({
  if (input$restrial1 == "Residual vs Predicted Plot") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'respred_use_prev', label = '',
          value = FALSE),
        bsTooltip("respred_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$restrial1 == "Residual Box Plot") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'resbox_use_prev', label = '',
          value = FALSE),
        bsTooltip("resbox_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$restrial1 == "Residual Histogram") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'reshist_use_prev', label = '',
          value = FALSE),
        bsTooltip("reshist_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$restrial1 == "Residual QQ Plot") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'resqq_use_prev', label = '',
          value = FALSE),
        bsTooltip("resqq_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$restrial1 == "Normality Test") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'resnorm_use_prev', label = '',
          value = FALSE),
        bsTooltip("resnorm_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  }
})

output$ui_resdiagout <- renderUI({
  if (input$restrial1 == "Residual vs Predicted Plot") {
    fluidRow(
      br(),
      column(12, align = 'center', plotOutput('resvsplot', height = '500px'))
    )
  } else if (input$restrial1 == "Residual Box Plot") {
    fluidRow(
      br(),
      column(12, align = 'center', plotOutput('resboxplot', height = '500px'))
    )
  } else if (input$restrial1 == "Residual Histogram") {
    fluidRow(
      br(),
      column(12, align = 'center', plotOutput('reshistplot', height = '500px'))
    )
  } else if (input$restrial1 == "Residual QQ Plot") {
    fluidRow(
      br(),
      column(12, align = 'center', plotOutput('resqqplot', height = '500px'))
    )
  } else if (input$restrial1 == "Normality Test") {
    fluidRow(
      br(),
      column(12, align = 'center', verbatimTextOutput('resnormtest'))
    )
  }
})


d_respred_mod <- eventReactive(input$submit_respred_plot, {
  k <- lm(input$respred_fmla, data = final_split$train)
  if(input$respred_use_prev) {
    out <- ols_rvsp_plot(all_use_n())
  } else {
    out <- ols_rvsp_plot(k)
  }
  out
})

d_resbox_mod <- eventReactive(input$submit_resbox_plot, {
  k <- lm(input$resbox_fmla, data = final_split$train)
  if(input$resbox_use_prev) {
    out <- ols_rsd_boxplot(all_use_n())
  } else {
    out <-   ols_rsd_boxplot(k)
  }
  out
})

d_reshist_mod <- eventReactive(input$submit_reshist_plot, {
  k <- lm(input$reshist_fmla, data = final_split$train)
  if(input$reshist_use_prev) {
    out <- ols_rsd_hist(all_use_n())
  } else {
    out <- ols_rsd_hist(k)
  }
  out
})

d_resqq_mod <- eventReactive(input$submit_resqq_plot, {
  k <- lm(input$resqq_fmla, data = final_split$train)
  if(input$resqq_use_prev) {
    out <- ols_rsd_qqplot(all_use_n())
  } else {
    out <- ols_rsd_qqplot(k)
  }
  out
})

d_resnorm_mod <- eventReactive(input$submit_resnorm, {
  k <- lm(input$resnorm_fmla, data = final_split$train)
  if(input$resnorm_use_prev) {
    out <- ols_norm_test(all_use_n())
  } else {
    out <- ols_norm_test(k)
  }
  out
})


output$resvsplot <- renderPlot({
  print(d_respred_mod())
})

output$resboxplot <- renderPlot({
  print(d_resbox_mod())
})

output$reshistplot <- renderPlot({
  print(d_reshist_mod())
})

output$resqqplot <- renderPlot({
  print(d_resqq_mod())
})

output$resnormtest <- renderPrint({
  print(d_resnorm_mod())
})

