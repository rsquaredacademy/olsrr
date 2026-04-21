tabPanel('Heteroskedasticity', value = 'tab_hetero',
  fluidPage(
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Breusch Pagan Test',
            fluidPage(
              br(),
              fluidRow(
                column(6, align = 'left',
                  h4('Breusch Pagan Test'),
                  p('Test for constant variance. It assumes that the error terms are normally distributed.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='bplink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_test_breusch_pagan.html', '_blank')")
                )
              ),
              hr(),
              fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("het_bp_fmla", label = '', width = '870px',
                                    value = ""),
                    bsTooltip("het_bp_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'bp_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("bp_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Fitted Values:')),
                column(4, align = 'left',
                  selectInput('het_bp_fv', '', choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                      selected = "FALSE"),
                  bsTooltip("het_bp_fv", "Use fitted values of regression model.",
                                "left", options = list(container = "body"))
                ),
                column(3, align = 'right', br(), h5('p Value Adjustment:')),
                column(3, align = 'left',
                  selectInput('het_bp_padj', '',
                    choices = c("none", "bonferroni", "sidak", "holm"),
                    selected = "none"),
                bsTooltip("het_bp_padj", "Options for p value adjustment.",
                  "bottom", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('RHS:')),
                column(4, align = 'left',
                  selectInput('het_bp_rhs', '', choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                      selected = "FALSE"),
                  bsTooltip("het_bp_rhs", "Use explanatory variables of model.",
                                "left", options = list(container = "body"))
                ),
                column(3, align = 'right', br(), h5('Variables:')),
                column(3, align = 'left',
                  selectInput("het_bp_vars", label = '',
                                      choices = "", selected = "", multiple = TRUE,
                                      selectize = TRUE),
                  bsTooltip("het_bp_vars", "Select variables for heteroskedasticity test.",
                                "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Multiple:')),
                column(4, align = 'left',
                  selectInput('het_bp_mult', '', choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                      selected = "FALSE"),
                  bsTooltip("het_bp_mult", "Perform multiple tests.",
                                "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(4, offset = 4, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_het_bp', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_het_bp", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, verbatimTextOutput('het_bp_out'))
              )
            )
          ),
          tabPanel('Bartlett Test',
            fluidPage(
              br(),
              fluidRow(
                column(6, align = 'left',
                  h4('Bartlett Test'),
                  p('Test if k samples have equal variances.')
                ),
                column(6, align = 'right',
                  actionButton(inputId='bartlink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_test_bartlett.html', '_blank')")
                )
              ),
              fluidRow(
                column(12,
                  tabsetPanel(type = 'tabs',
                    tabPanel('Using Variables',
                      fluidPage(
                        fluidRow(
                          column(2, align = 'right', br(), h5('Variables:')),
                          column(10, align = 'left',
                              selectInput("var_bartest", label = '', width = '660px',
                                              choices = "", selected = "", multiple = TRUE,
                                              selectize = TRUE),
                              bsTooltip("var_bartest", "Select variables.",
                                        "left", options = list(container = "body")))
                        ),
                        fluidRow(
                            column(12, align = 'center',
                            br(),
                            br(),
                            actionButton(inputId = 'submit_bartest', label = 'Submit', width = '120px', icon = icon('check')),
                            bsTooltip("submit_levtest", "Click here to view test result.",
                                          "bottom", options = list(container = "body")))
                        ),
                        fluidRow(
                            br(),
                            column(12, align = 'center',
                                verbatimTextOutput('bartest_out')
                            )
                        )
                      )
                    ),
                    tabPanel('Using Groups',
                      fluidPage(
                        fluidRow(
                          column(2, align = 'right', br(), h5('Variable:')),
                          column(2, align = 'left',
                              selectInput("var_bartestg1", label = '', width = '200px',
                                              choices = "", selected = ""),
                              bsTooltip("var_bartestg1", "Select a variable.",
                                        "left", options = list(container = "body"))),
                          column(2, align = 'right', br(), h5('Grouping Variable:')),
                          column(2, align = 'left',
                          selectInput("var_bartestg2", label = '', width = '200px',
                            choices = "", selected = ""),
                          bsTooltip("var_bartestg2", "Select a  grouping variable.",
                            "left", options = list(container = "body")))
                        ),
                        fluidRow(
                            column(12, align = 'center',
                            br(),
                            br(),
                            actionButton(inputId = 'submit_bartestg', label = 'Submit', width = '120px', icon = icon('check')),
                            bsTooltip("submit_bartestg", "Click here to view test result.",
                                          "bottom", options = list(container = "body")))
                        ),
                        fluidRow(
                            br(),
                            column(12, align = 'center',
                                verbatimTextOutput('bartestg_out')
                            )
                        )
                      )
                    ),
                    tabPanel('Using Formula',
                      fluidPage(
                        fluidRow(
                          column(2, align = 'right', br(), h5('Model Formula:')),
                          column(10, align = 'left',
                              textInput("bartest_fmla", label = '', width = '660px',
                                              value = ""),
                              bsTooltip("bartest_fmla", "Specify a formula",
                                        "left", options = list(container = "body")))
                        ),
                        fluidRow(
                            column(12, align = 'center',
                            br(),
                            br(),
                            actionButton(inputId = 'submit_bartestf', label = 'Submit', width = '120px', icon = icon('check')),
                            bsTooltip("submit_bartestf", "Click here to view test result.",
                                          "bottom", options = list(container = "body")))
                        ),
                        fluidRow(
                            br(),
                            column(12, align = 'center',
                                verbatimTextOutput('bartestf_out')
                            )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          tabPanel('F Test',
            fluidPage(
              br(),
              fluidRow(
                column(6, align = 'left',
                  h4('F Test'),
                  p('Test for heteroskedasticity under the assumption that the errors are independent and identically distributed (i.i.d.).')
                ),
                column(6, align = 'right',
                  actionButton(inputId='freglink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_test_f.html', '_blank')")
                )
              ),
              hr(),
              fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("het_f_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("het_f_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'f_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("f_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Variables:')),
                column(10, align = 'left',
                  selectInput("het_f_vars", label = '', width = '660px',
                                      choices = "", selected = "", multiple = TRUE,
                                      selectize = TRUE),
                  bsTooltip("het_f_vars", "Select variables for heteroskedasticity test.",
                                "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Fitted Values:')),
                column(4, align = 'left',
                  selectInput('het_f_fv', '',  width = '200px', choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                      selected = "FALSE"),
                  bsTooltip("het_f_fv", "Use fitted values of regression model.",
                                "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('RHS:')),
                column(4, align = 'left',
                  selectInput('het_f_rhs', '', width = '200px', choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                      selected = "FALSE"),
                  bsTooltip("het_f_rhs", "Use explanatory variables of model.",
                                "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_het_f', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_het_f", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, verbatimTextOutput('het_f_out'))
              )
            )
          ),
          tabPanel('Score Test',
            fluidPage(
              br(),
              fluidRow(
                column(6, align = 'left',
                  h4('Score Test'),
                  p('Test for heteroskedasticity under the assumption that the errors are independent and identically distributed (i.i.d.).')
                ),
                column(6, align = 'right',
                  actionButton(inputId='scorelink1', label="Help", icon = icon("question-circle"),
                    onclick ="window.open('https://olsrr.rsquaredacademy.com/reference/ols_test_score.html', '_blank')")
                )
              ),
              hr(),
              fluidRow(
                column(2, align = 'right', br(), h5('Model Formula:')),
                column(10, align = 'left',
                    textInput("het_score_fmla", label = '', width = '660px',
                                    value = ""),
                    bsTooltip("het_score_fmla", "Specify model formula",
                              "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Use previous model:')),
                column(2, align = 'left', br(),
                  checkboxInput(inputId = 'score_use_prev', label = '',
                    value = FALSE),
                  bsTooltip("score_use_prev", "Use model from Regression Tab.",
                              "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Variables:')),
                column(10, align = 'left',
                  selectInput("het_score_vars", label = '', width = '660px',
                                      choices = "", selected = "", multiple = TRUE,
                                      selectize = TRUE),
                  bsTooltip("het_score_vars", "Select variables for heteroskedasticity test.",
                                "left", options = list(container = "body")))
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('Fitted Values:')),
                column(4, align = 'left',
                  selectInput('het_score_fv', '',  width = '200px', choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                      selected = "FALSE"),
                  bsTooltip("het_score_fv", "Use fitted values of regression model.",
                                "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(2, align = 'right', br(), h5('RHS:')),
                column(4, align = 'left',
                  selectInput('het_score_rhs', '', width = '200px', choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                      selected = "FALSE"),
                  bsTooltip("het_score_rhs", "Use explanatory variables of model.",
                                "left", options = list(container = "body"))
                )
              ),
              fluidRow(
                column(12, align = 'center',
                br(),
                br(),
                actionButton(inputId = 'submit_het_score', label = 'Submit', width = '120px', icon = icon('check')),
                bsTooltip("submit_het_score", "Click here to view test results.",
                              "bottom", options = list(container = "body")))
              ),
              fluidRow(
                  br(),
                  column(12, verbatimTextOutput('het_score_out'))
              )
            )
          )
        )
      )
    )
  )
)
