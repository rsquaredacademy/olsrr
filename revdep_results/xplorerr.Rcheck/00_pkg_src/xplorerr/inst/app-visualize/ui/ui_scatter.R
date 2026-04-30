tabPanel('Scatter Plot', value = 'tab_scatter',
  fluidPage(
    fluidRow(
                 column(12, align = 'left',
                   h4('Scatter Plot')
                 )
               ),
               hr(),
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          tabPanel('Variables',
            fluidRow(
              column(2,
                  selectInput('scatter_select_x', 'X Axis Variable: ',
                              choices = "", selected = ""),
                  numericInput("scatter_shape", label = "Shape: ",
                               min = 1, max = 25, step = 1, value = 1),
                  textInput("scatter_colors", label = "Color: ", value = "black"),
                  textInput(inputId = "scatter_title", label = "Title: ", value = "title"),
                  textInput(inputId = "scatter_xlabel", label = "X Axes Label: ", value = "label")
              ),
              column(2,
                  selectInput("scatter_select_y", label = "Y Axis Variable:",
                              choices = "", selected = ""),
                  numericInput("scatter_size1", label = "Size: ",
                               min = 0, max = NA, step = 0.1, value = 1),
                  textInput("scatter_fill", label = "Fill: ", value = "black"),
                  textInput(inputId = "scatter_subtitle", label = "Subtitle: ", value = "subtitle"),
                  textInput(inputId = "scatter_ylabel", label = "Y Axes Label: ", value = "label")
              ),
              column(8,
                plotOutput('scatter_plot_1', height = '600px')
              )
            )
          ),
          tabPanel('Axis Range',
            fluidRow(
              column(2,
                  uiOutput('ui_xrange_min'),
                  uiOutput('ui_xrange_max')
              ),
              column(2,
                  uiOutput('ui_yrange_min'),
                  uiOutput('ui_yrange_max')
              ),
              column(8,
                plotOutput('scatter_plot_2', height = '600px')
              )
            )
          ),
          tabPanel('Fit Line',
            fluidRow(
              column(2,
                  selectInput('fitline_y', 'Fit Line',
                      choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                      selected = "FALSE"),
                  textInput('col_fitline', 'Color', 'black')
              ),
              column(2,
                  numericInput('lty_fitline', 'Line Type', value = 1, min = 1, max = 5, step = 1),
                  numericInput('lwd_fitline', 'Line Width', value = 1, min = 0.1, step = 0.1)
              ),
              column(8,
                plotOutput('scatter_plot_3', height = '600px')
              )
            )
          ),
          tabPanel('Others',
            column(4,
              tabsetPanel(type = 'tabs',
                tabPanel('Color',
                  fluidRow(
                    column(6,
                      textInput('scatter_colaxis', 'Axis Color: ', 'black'),
                      textInput('scatter_coltitle', 'Title Color: ', 'black')
                    ),
                    column(6,
                      textInput('scatter_collabel', 'Label Color: ', 'black'),
                      textInput('scatter_colsub', 'Subtitle Color: ', 'black')
                    )
                  )
                ),
                tabPanel('Size',
                  fluidRow(
                    column(6,
                      numericInput('scatter_cexmain', 'Title Size: ',
                                   value = 1, min = 0.1, step = 0.1),
                      numericInput('scatter_cexsub', 'Subtitle Size: ',
                                   value = 1, min = 0.1, step = 0.1)
                    ),
                    column(6,
                      numericInput('scatter_cexaxis', 'Axis Size: ',
                                   value = 1, min = 0.1, step = 0.1),
                      numericInput('scatter_cexlab', 'Label Size: ',
                                   value = 1, min = 0.1, step = 0.1)
                    )
                  )
                ),
                tabPanel('Font',
                  fluidRow(
                    column(6,
                      numericInput('scatter_fontmain', 'Title Font',
                                   value = 1, min = 1, max = 5, step = 1),
                      numericInput('scatter_fontsub', 'Subtitle Font',
                                   value = 1, min = 1, max = 5, step = 1)
                    ),
                    column(6,
                      numericInput('scatter_fontaxis', 'Axis Font',
                                   value = 1, min = 1, max = 5, step = 1),
                      numericInput('scatter_fontlab', 'Label Font',
                                   value = 1, min = 1, max = 5, step = 1)
                    )
                  )
                )
              )
            ),
            column(8,
              plotOutput('scatter_plot_4')
            )
          ),
          tabPanel('Text',
            column(4,
              tabsetPanel(type = 'tabs',
                tabPanel('Text (Inside)',
                  fluidRow(
                    column(6,
                      numericInput(
                          inputId = "scatter_text_x_loc",
                          label = "X Intercept: ",
                          value = 1, step = 1),
                      numericInput(
                          inputId = "scatter_text_y_loc",
                          label = "Y Intercept: ",
                          value = 1, step = 1
                      ),
                      textInput(inputId = "scatter_plottext", label = "Text:",
                        value = "")
                    ),
                    column(6,
                      numericInput(
                          inputId = "scatter_textfont",
                          label = "Text Font: ",
                          value = 1, min = 1, max = 5, step = 1
                      ),
                      numericInput(
                          inputId = "scatter_textsize",
                          label = "Text Size: ",
                          value = 1, min = 0.1, step = 0.1
                      ),
                      textInput(
                          inputId = "scatter_textcolor",
                          label = "Text Color: ",
                          value = "black"
                      )
                    )
                  )
                ),
                tabPanel('Marginal Text',
                  fluidRow(
                    column(6,
                      numericInput(
                          inputId = "scatter_mtext_side",
                          label = "Side: ",
                          value = 1, min = 1, max = 4, step = 1
                      ),
                      numericInput(
                          inputId = "scatter_mtext_line",
                          label = "Line: ",
                          value = 1, step = 1
                      ),
                      textInput(inputId = "scatter_mtextplot", label = "Text:", value = ""),
                      numericInput(
                          inputId = "scatter_mtextsize",
                          label = "Text Size: ",
                          value = 1, min = 0.1, step = 0.1
                      )
                    ),
                    column(6,
                      numericInput(
                          inputId = "scatter_mtextadj",
                          label = "Adj: ",
                          value = 0.5, min = 0, max = 1, step = 0.1
                      ),
                      numericInput(
                          inputId = "scatter_mtextfont",
                          label = "Text Font: ",
                          value = 1, min = 1, max = 5, step = 1
                      ),
                      textInput(
                          inputId = "scatter_mtextcolor",
                          label = "Text Color: ",
                          value = "black"
                      )
                    )
                  )
                )
              )
            ),
            column(8,
              plotOutput('scatter_plot_5', height = '600px')
            )
          ),
          tabPanel('Plot',
            fluidRow(
              column(8, offset = 2,
                plotOutput('scatter_plot_final')
              )
            )
          )
        )
      )
    )
  )
)
