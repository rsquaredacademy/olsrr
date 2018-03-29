output$ui_inflobslink <- renderUI({
    if (input$inflobs_select == "Cook's D Bar Plot") {
      fluidRow(
                column(6, align = 'left',
                  h4("Cook's D Bar Plot"),
                  p("Bar Plot of Cook's distance to detect observations that strongly influence fitted values of the model.")
                ),
                column(6, align = 'right',
                  actionButton(inputId='cdbplink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_cooksd_bar.html', '_blank')")
                )
              )
    } else if (input$inflobs_select == "Cook's D Chart") {
      fluidRow(
                column(6, align = 'left',
                  h4("Cook's D Chart"),
                  p("Chart of Cook's distance to detect observations that strongly influence fitted values of the model.")
                ),
                column(6, align = 'right',
                  actionButton(inputId='cdclink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_cooksd_chart.html', '_blank')")
                )
              )
    } else if (input$inflobs_select == "DFBETAs Panel") {
      fluidRow(
                column(6, align = 'left',
                  h4('DFBETAS Panel'),
                  p("Panel of plots to detect influential observations using DFBETAs.")
                ),
                column(6, align = 'right',
                  actionButton(inputId='dfblink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_dfbetas.html', '_blank')")
                )
              )
    } else if (input$inflobs_select == "DFFITS Plot") {
      fluidRow(
                column(6, align = 'left',
                  h4('DFFITS Plot'),
                  p("Plot for detecting influential observations using DFFITS.")
                ),
                column(6, align = 'right',
                  actionButton(inputId='dfitslink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_dffits.html', '_blank')")
                )
              )
    } else if (input$inflobs_select == "Deleted Stud Resid vs Fitted") {
      fluidRow(
        column(6, align = 'left',
          h4('Deleted Studentized Residual vs Predicted Plot'),
          p('Plot for detecting outliers.')
        ),
        column(6, align = 'right',
          actionButton(inputId='dsrvsplink1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_stud_fit.html', '_blank')")
        )
      )
    } else if (input$inflobs_select == "Hadi Plot") {
      fluidRow(
                column(6, align = 'left',
                  h4('Hadi Plot'),
                  p("Plot for detecting outliers based on Hadi's influence measure.")
                ),
                column(6, align = 'right',
                  actionButton(inputId='hadiplink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_hadi.html', '_blank')")
                )
              )
    } else if (input$inflobs_select == "Studentized Residuals vs Leverage") {
      fluidRow(
        column(6, align = 'left',
          h4('Studentized Residual vs Leverage Plot'),
          p('Graph for detecting influential observations.')
        ),
        column(6, align = 'right',
          actionButton(inputId='srvslev1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_lev.html', '_blank')")
        )
      )
    } else if (input$inflobs_select == "Studentized Residual Plot") {
      fluidRow(
        column(6, align = 'left',
          h4('Studentized Residual Plot'),
          p('Graph for identifying outliers.')
        ),
        column(6, align = 'right',
          actionButton(inputId='srplot1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_stud.html', '_blank')")
        )
      )
    } else if (input$inflobs_select == "Studentized Residual Chart") {
      fluidRow(
        column(6, align = 'left',
          h4('Studentized Residual Chart'),
          p('Graph for identifying outliers.')
        ),
        column(6, align = 'right',
          actionButton(inputId='srchart1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_stand.html', '_blank')")
        )
      )
    } else if (input$inflobs_select == "Potential Residual Plot") {
      fluidRow(
        column(6, align = 'left',
          h4('Potential Residual Plot'),
          p('Graph for identifying outliers.')
        ),
        column(6, align = 'right',
          actionButton(inputId='potreslink1', label="Help", icon = icon("question-circle"),
            onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_plot_resid_pot.html', '_blank')")
        )
      )
    }
})

output$ui_inflobsfmla <- renderUI({
    if (input$inflobs_select == "Cook's D Bar Plot") {
      fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("cooksb_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("cooksb_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )
    } else if (input$inflobs_select == "Potential Residual Plot") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("potres_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("potres_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "Cook's D Chart") {
      fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("cooksc_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("cooksc_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )
    } else if (input$inflobs_select == "DFBETAs Panel") {
      fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("dfbetas_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("dfbetas_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )
    } else if (input$inflobs_select == "Deleted Stud Resid vs Fitted") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("dsresp_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("dsresp_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "DFFITS Plot") {
      fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("dffits_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("dffits_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )
    } else if (input$inflobs_select == "Studentized Residuals vs Leverage") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("sreslev_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("sreslev_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "Studentized Residual Plot") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("studres_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("studres_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "Studentized Residual Chart") {
      fluidRow(
        column(2, align = 'right', br(), h5('Model Formula:')),
        column(10, align = 'left',
            textInput("sreschart_fmla", label = '', width = '660px',
                            value = ""),
            bsTooltip("sreschart_fmla", "Specify model formula",
                      "left", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "Hadi Plot") {
      fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("hadiplot_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("hadiplot_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              )
    }
})

output$ui_inflobssubmit <- renderUI({
    if (input$inflobs_select == "Cook's D Bar Plot") {
      fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_cooksb', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_cooksb", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              )
    } else if (input$inflobs_select == "Potential Residual Plot") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_potres_plot', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_potres_plot", "Click here to view regression result.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "Cook's D Chart") {
      fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_cooksc', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_cooksc", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              )
    } else if (input$inflobs_select == "DFBETAs Panel") {
      fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_dfbetas', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_dfbetas", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              )
    } else if (input$inflobs_select == "Deleted Stud Resid vs Fitted") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_dsresp_plot', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_dsresp_plot", "Click here to view regression result.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "DFFITS Plot") {
      fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_dffits', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_dffits", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              )
    } else if (input$inflobs_select == "Studentized Residuals vs Leverage") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_sreslev_plot', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_sreslev_plot", "Click here to view regression result.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "Studentized Residual Plot") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_sresp_plot', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_cprp_plot", "Click here to view regression result.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "Studentized Residual Chart") {
      fluidRow(
          column(12, align = 'center',
          br(),
          br(),
          actionButton(inputId = 'submit_sreschart_plot', label = 'Submit', width = '120px', icon = icon('check')),
          bsTooltip("submit_sreschart_plot", "Click here to view regression result.",
                        "bottom", options = list(container = "body")))
      )
    } else if (input$inflobs_select == "Hadi Plot") {
      fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_hadiplot', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_hadiplot", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              )
    }
})


output$ui_inflobsprev <- renderUI({
  if (input$inflobs_select == "Cook's D Bar Plot") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'cdbp_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("cdbp_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  } else if (input$inflobs_select == "Potential Residual Plot") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'potres_use_prev', label = '',
          value = FALSE),
        bsTooltip("potres_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$inflobs_select == "Cook's D Chart") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'cooksc_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("cooksc_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  } else if (input$inflobs_select == "DFBETAs Panel") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'dfb_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("dfb_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  } else if (input$inflobs_select == "Deleted Stud Resid vs Fitted") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'dsresp_use_prev', label = '',
          value = FALSE),
        bsTooltip("dsresp_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$inflobs_select == "DFFITS Plot") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'dfits_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("dfits_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  } else if (input$inflobs_select == "Studentized Residuals vs Leverage") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'sreslev_use_prev', label = '',
          value = FALSE),
        bsTooltip("sreslev_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$inflobs_select == "Studentized Residual Plot") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'sres_use_prev', label = '',
          value = FALSE),
        bsTooltip("sres_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$inflobs_select == "Studentized Residual Chart") {
    fluidRow(
      column(2, align = 'right', br(), h5('Use previous model:')),
      column(2, align = 'left', br(),
        checkboxInput(inputId = 'sreschart_use_prev', label = '',
          value = FALSE),
        bsTooltip("sreschart_use_prev", "Use model from Regression Tab.",
                    "left", options = list(container = "body"))
      )
    )
  } else if (input$inflobs_select == "Hadi Plot") {
    fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'hadip_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("hadip_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              )
  }
})



d_potres_mod <- eventReactive(input$submit_potres_plot, {
  # validate(need((input$potres_fmla != ''), 'Please specify model'))
    data <- final_split$train
})


d_dsresp_mod <- eventReactive(input$submit_dsresp_plot, {
  # validate(need((input$dsresp_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

d_sreslev_mod <- eventReactive(input$submit_sreslev_plot, {
  # validate(need((input$sreslev_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

d_sres_mod <- eventReactive(input$submit_sresp_plot, {
  # validate(need((input$studres_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

d_sreschart_mod <- eventReactive(input$submit_sreschart_plot, {
  # validate(need((input$sreschart_fmla != ''), 'Please specify model'))
    data <- final_split$train
})



d_dffits <- eventReactive(input$submit_dffits, {
  # validate(need((input$dffits_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

d_cdbp <- eventReactive(input$submit_cooksb, {
  # validate(need((input$cooksb_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

d_cdc <- eventReactive(input$submit_cooksc, {
  # validate(need((input$cooksc_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

d_hadi <- eventReactive(input$submit_hadiplot, {
  # validate(need((input$hadiplot_fmla != ''), 'Please specify model'))
    data <- final_split$train
})


potres_mod <- reactive({
  k <- lm(input$potres_fmla, data = d_potres_mod())
  k
})

dsresp_mod <- reactive({
  k <- lm(input$dsresp_fmla, data = d_dsresp_mod())
  k
})

sreslev_mod <- reactive({
  k <- lm(input$sreslev_fmla, data = d_sreslev_mod())
  k
})

sres_mod <- reactive({
  k <- lm(input$studres_fmla, data = d_sres_mod())
  k
})

sreschart_mod <- reactive({
  k <- lm(input$sreschart_fmla, data = d_sreschart_mod())
  k
})

dfits_mod <- eventReactive(input$submit_dffits, {
  k <- lm(input$dffits_fmla, data = d_dffits())
  k
})

cdbp_mod <- eventReactive(input$submit_cooksb, {
  k <- lm(input$cooksb_fmla, data = d_cdbp())
  k
})

cdc_mod <- eventReactive(input$submit_cooksc, {
  k <- lm(input$cooksc_fmla, data = d_cdc())
  k
})

hadi_mod <- eventReactive(input$submit_hadiplot, {
  k <- lm(input$hadiplot_fmla, data = d_hadi())
  k
})

d_dfbetas <- eventReactive(input$submit_dfbetas, {
  # validate(need((input$dfbetas_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

dfbetas_mod <- eventReactive(input$submit_dfbetas, {
  k <- lm(input$dfbetas_fmla, data = d_dfbetas())
  k
})

plot_n <- reactive({
  if (input$dfb_use_prev) {
    (length(all_use_n()$coefficients) * 500) / 2
  } else {
    (length(dfbetas_mod()$coefficients) * 500) / 2
  }
})

output$ui_inflobsplot <- renderUI({
  if (input$inflobs_select == "Cook's D Bar Plot") {
    column(12, align = 'center', plotOutput('inflobsplot', height = '500px'))
  } else if (input$inflobs_select == "Potential Residual Plot") {
    column(12, align = 'center', plotOutput('inflobsplot', height = '500px'))
  } else if (input$inflobs_select == "Cook's D Chart") {
    column(12, align = 'center', plotOutput('inflobsplot', height = '500px'))
  } else if (input$inflobs_select == "DFBETAs Panel") {
    column(12,  align = 'center', plotOutput('inflobsplot', height = paste0(plot_n(), 'px')))
  } else if (input$inflobs_select == "Deleted Stud Resid vs Fitted") {
    column(12, align = 'center', plotOutput('inflobsplot', height = '500px'))
  } else if (input$inflobs_select == "DFFITS Plot") {
    column(12, align = 'center', plotOutput('inflobsplot', height = '500px'))
  } else if (input$inflobs_select == "Studentized Residuals vs Leverage") {
    column(12, align = 'center', plotOutput('inflobsplot', height = '500px'))
  } else if (input$inflobs_select == "Studentized Residual Plot") {
   column(12, align = 'center', plotOutput('inflobsplot', height = '500px'))
  } else if (input$inflobs_select == "Studentized Residual Chart") {
    column(12, align = 'center', plotOutput('inflobsplot', height = '500px'))
  } else if (input$inflobs_select == "Hadi Plot") {
    column(12, align = 'center', plotOutput('inflobsplot', height = '500px'))
  }
})

output$ui_inflobsprint <- renderUI({
  if (input$inflobs_select == "Cook's D Bar Plot") {
    column(12, align = 'center', verbatimTextOutput('inflobs'))
  } else if (input$inflobs_select == "Potential Residual Plot") {
    column(12, align = 'center', verbatimTextOutput('inflobs'))
  } else if (input$inflobs_select == "Cook's D Chart") {
    column(12, align = 'center', verbatimTextOutput('inflobs'))
  } else if (input$inflobs_select == "DFBETAs Panel") {
    column(12, align = 'center', verbatimTextOutput('inflobs'))
  } else if (input$inflobs_select == "Deleted Stud Resid vs Fitted") {
    column(12, align = 'center', verbatimTextOutput('inflobs'))
  } else if (input$inflobs_select == "DFFITS Plot") {
    column(12, align = 'center', verbatimTextOutput('inflobs'))
  } else if (input$inflobs_select == "Studentized Residuals vs Leverage") {
    column(12, align = 'center', verbatimTextOutput('inflobs'))
  } else if (input$inflobs_select == "Studentized Residual Plot") {
   column(12, align = 'center', verbatimTextOutput('inflobs'))
  } else if (input$inflobs_select == "Studentized Residual Chart") {
    column(12, align = 'center', verbatimTextOutput('inflobs'))
  } else if (input$inflobs_select == "Hadi Plot") {
    column(12, align = 'center', verbatimTextOutput('inflobs'))
  }
})

result_cdbp <- eventReactive(input$submit_cooksb, {
  if (input$cdbp_use_prev) {
    ols_plot_cooksd_bar(all_use_n())
  } else {
    ols_plot_cooksd_bar(cdbp_mod())
  }
})

result_cdc <- eventReactive(input$submit_cooksc, {
  if (input$cooksc_use_prev) {
    ols_plot_cooksd_chart(all_use_n())
  } else {
    ols_plot_cooksd_chart(cdc_mod())
  }
})

result_potres <- eventReactive(input$submit_potres_plot, {
  if(input$potres_use_prev) {
    ols_plot_resid_pot(all_use_n())
  } else {
    ols_plot_resid_pot(potres_mod())
  }
})

result_dfbetas <- eventReactive(input$submit_dfbetas, {
  if (input$dfb_use_prev) {
    ols_plot_dfbetas(all_use_n())
  } else {
    ols_plot_dfbetas(dfbetas_mod())
  }
})

result_dsrvsp <- eventReactive(input$submit_dsresp_plot, {
  if(input$dsresp_use_prev) {
    ols_plot_resid_stud_fit(all_use_n())
  } else {
    ols_plot_resid_stud_fit(dsresp_mod())
  }
})

result_dffits <- eventReactive(input$submit_dffits, {
  if (input$dfits_use_prev) {
    ols_plot_dffits(all_use_n())
  } else {
    ols_plot_dffits(dfits_mod())
  }
})

result_srvslev <- eventReactive(input$submit_sreslev_plot, {
  if(input$sreslev_use_prev) {
    ols_plot_resid_lev(all_use_n())
  } else {
    ols_plot_resid_lev(sreslev_mod())
  }
})

result_srplot <- eventReactive(input$submit_sresp_plot, {
  if(input$sres_use_prev) {
    ols_plot_resid_stud(all_use_n())
  } else {
    ols_plot_resid_stud(sres_mod())
  }
})

result_srchart <- eventReactive(input$submit_sreschart_plot, {
  if(input$sreschart_use_prev) {
    ols_plot_resid_stand(all_use_n())
  } else {
    ols_plot_resid_stand(sreschart_mod())
  }
})

result_hadi <- eventReactive(input$submit_hadiplot, {
  if (input$hadip_use_prev) {
    ols_plot_hadi(all_use_n())
  } else {
    ols_plot_hadi(hadi_mod())
  }
})

output$inflobsplot <- renderPlot({
  if (input$inflobs_select == "Cook's D Bar Plot") {
    result_cdbp()
  } else if (input$inflobs_select == "Potential Residual Plot") {
    result_potres()
  } else if (input$inflobs_select == "Cook's D Chart") {
    result_cdc()
  } else if (input$inflobs_select == "DFBETAs Panel") {
    result_dfbetas()
  } else if (input$inflobs_select == "Deleted Stud Resid vs Fitted") {
    result_dsrvsp()
  } else if (input$inflobs_select == "DFFITS Plot") {
    result_dffits()
  } else if (input$inflobs_select == "Studentized Residuals vs Leverage") {
    result_srvslev()
  } else if (input$inflobs_select == "Studentized Residual Plot") {
    result_srplot()
  } else if (input$inflobs_select == "Standardized Residual Chart") {
    result_srchart()
  } else if (input$inflobs_select == "Hadi Plot") {
    result_hadi()
  }
})

output$inflobs <- renderPrint({
  if (input$inflobs_select == "Cook's D Bar Plot") {
    k <- result_cdbp()
    k
  } else if (input$inflobs_select == "Cook's D Chart") {
    k <- result_cdc()
    k
  } else if (input$inflobs_select == "DFBETAs Panel") {
    k <- result_dfbetas()
    k
  } else if (input$inflobs_select == "Deleted Stud Resid vs Fitted") {
    k <- result_dsrvsp()
    k
  } else if (input$inflobs_select == "DFFITS Plot") {
    k <- result_dffits()
    k
  } else if (input$inflobs_select == "Studentized Residuals vs Leverage") {
    k <- result_srvslev()
    k
  } else if (input$inflobs_select == "Studentized Residual Plot") {
    k <- result_srplot()
    k
  } else if (input$inflobs_select == "Studentized Residual Chart") {
    k <- result_srchart()
    k
  }
})
