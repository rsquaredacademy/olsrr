tabPanel('2 Factor Box Plot', value = 'tab_gbox2',

  fluidPage(

    fluidRow(
      column(12, align = 'left',
             h4('Box Plot')
      )
    ),
    
    hr(),
    
    fluidRow(
      column(12,
        tabsetPanel(type = 'tabs',
          
          tabPanel('Variables',
                             
            fluidRow(
              column(2,
                selectInput('gbox2_select_x', 'Variable 1: ',
                  choices = "", selected = ""),
                selectInput('gbox2_notch', 'Notch',
                  choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
                textInput(inputId = "gbox2_fill", label = "Bar Color: ",
                  value = "blue"),
                textInput(inputId = "gbox2_title", label = "Title: ",
                  value = "title"),
                textInput(inputId = "gbox2_xlabel", label = "X Axes Label: ",
                  value = "label")
              ),
                               
              column(2,
                selectInput('gbox2_select_y', 'Variable 2: ',
                  choices = "", selected = ""),
                selectInput('gbox2_horiz', 'Horizontal',
                  choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
                textInput(inputId = "gbox2_col", label = "Border Color: ",
                  value = "black"),
                textInput(inputId = "gbox2_subtitle", label = "Subtitle: ",
                  value = "subtitle"),
                textInput(inputId = "gbox2_ylabel", label = "Y Axes Label: ",
                  value = "label")
                ),
                               
              column(8,
                plotOutput('gbox2_plot_1', height = '600px')
              )                               
            )                             
          ),

          tabPanel('Outliers',
            column(4, 
              fluidRow(
                h3('Outliers'),
                column(6,
                  textInput(inputId = "gbox2_ofill", label = "Fill: ",
                  value = "blue"),
                  numericInput(inputId = 'gbox2_oshape', label = 'Shape',
                    value = 22, min = 0, step = 1, max = 25
                  ),
                  numericInput(inputId = 'gbox2_oalpha', label = 'Alpha',
                    value = 0.8, min = 0, step = 0.1, max = 1
                  )
                ),
                column(6,
                  textInput(inputId = "gbox2_ocol", label = "Color: ",
                  value = "black"),
                  numericInput(inputId = 'gbox2_osize', label = 'Size',
                    value = 2, min = 0, step = 0.1
                  )
                )
              )
            ),
            column(8,
              plotOutput('gbox2_plot_2', height = '600px')
            )
          ),

          tabPanel('Jitter',
            column(4, 
              fluidRow(
                h3('Jitter'),
                column(6,
                  selectInput('gbox2_jitter', 'Jitter',
                  choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
                  numericInput(inputId = 'gbox2_jwidth', label = 'Width',
                    value = 0.1, min = 0, step = 0.1),
                  textInput(inputId = "gbox2_jfill", label = "Fill: ",
                  value = "blue"),
                  numericInput(inputId = 'gbox2_jshape', label = 'Shape',
                    value = 22, min = 0, step = 1, max = 25
                  )
                ),
                column(6,
                  numericInput(inputId = 'gbox2_jalpha', label = 'Alpha',
                    value = 0.8, min = 0, step = 0.1, max = 1
                  ),
                  numericInput(inputId = 'gbox2_jheight', label = 'Height',
                    value = 0.1, min = 0, step = 0.1
                  ),
                  textInput(inputId = "gbox2_jcol", label = "Color: ",
                  value = "black"),
                  numericInput(inputId = 'gbox2_jsize', label = 'Size',
                    value = 2, min = 0, step = 0.1
                  )
                )
              )
            ),
            column(8,
              plotOutput('gbox2_plot_3', height = '600px')
            )
          ),

          tabPanel('Axis Range',
            fluidRow(
              column(2,
                uiOutput('ui_gbox2yrange_min'),
                selectInput('gbox2_remx', 'Remove X Axis Label',
                  choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE") 
              ),
              column(2,
                uiOutput('ui_gbox2yrange_max'),
                selectInput('gbox2_remy', 'Remove Y Axis Label',
                  choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE")
              ),
              column(8,
                plotOutput('gbox2_plot_4', height = '600px')
              )
            )
          ),

          tabPanel('Annotations',
            fluidRow(
              column(2,
                selectInput('gbox2_text', 'Add Text',
                  choices = c("TRUE" = TRUE, "FALSE" = FALSE),
                  selected = "FALSE"),
                numericInput(
                  inputId = "gbox2_text_x_loc",
                  label = "X Intercept: ",
                  value = 1, step = 1),
                numericInput(
                  inputId = "gbox2_text_y_loc",
                  label = "Y Intercept: ",
                  value = 1, step = 1
                )
              ),
              column(2,
                textInput(inputId = "gbox2_plottext", label = "Text:",
                  value = ""),
                textInput(
                  inputId = "gbox2_textcolor",
                  label = "Text Color: ",
                  value = "black"
                ),
                numericInput(
                  inputId = "gbox2_textsize",
                  label = "Text Size: ",
                  value = 10, min = 1, step = 1
                )
              ),
              column(8,
                plotOutput('gbox2_plot_5', height = '600px')
              )
            )
          ),

          tabPanel('Others',
            column(4,
              tabsetPanel(type = 'tabs',
                tabPanel('Color',
                  fluidRow(
                    column(6,
                      textInput(inputId = "gbox2_title_col", label = "Title:",
                        value = "black"),
                      textInput(inputId = "gbox2_sub_col", label = "Subtitle:",
                        value = "black"),
                      textInput(inputId = "gbox2_xlab_col", label = "X Axis Label:",
                        value = "black"),
                      textInput(inputId = "gbox2_ylab_col", label = "Y Axis Label:",
                        value = "black")
                    )
                  )
                ),
                tabPanel('Font Family',
                  fluidRow(
                    column(6,
                      textInput(inputId = "gbox2_title_fam", label = "Title:",
                          value = "Times"),
                        textInput(inputId = "gbox2_sub_fam", label = "Subtitle:",
                          value = "Times"),
                        textInput(inputId = "gbox2_xlab_fam", label = "X Axis Label:",
                          value = "Times"),
                        textInput(inputId = "gbox2_ylab_fam", label = "Y Axis Label:",
                          value = "Times")
                    )
                  ) 
                ),
                tabPanel('Font Face',
                  fluidRow(
                    column(6,
                      selectInput('gbox2_title_font', 'Title:',
                        choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
                        selected = "plain"),
                      selectInput('gbox2_subtitle_font', 'Subtitle:',
                        choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
                        selected = "plain"),
                      selectInput('gbox2_xlab_font', 'X Axis Label:',
                        choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
                        selected = "plain"),
                      selectInput('gbox2_ylab_font', 'Y Axis Label:',
                        choices = c(plain = 'plain', bold = 'bold', italic = 'italic', bold_italic = 'bold.italic'),
                        selected = "plain")
                    )
                  ) 
                ),
                tabPanel('Font Size',
                  fluidRow(
                    column(6,
                      numericInput(inputId = "gbox2_title_size", label = "Title:",
                        min = 1, step = 0.1, value = 1),
                      numericInput(inputId = "gbox2_sub_size", label = "Subtitle:",
                        min = 1, step = 0.1, value = 1),
                      numericInput(inputId = "gbox2_xlab_size", label = "X Axis Label:",
                        min = 1, step = 0.1, value = 1),
                      numericInput(inputId = "gbox2_ylab_size", label = "Y Axis Label:",
                        min = 1, step = 0.1, value = 1)
                    )
                  )
                ),
                tabPanel('Horizontal',
                  fluidRow(
                    column(6,
                      numericInput(inputId = "gbox2_title_hjust", label = "Title:",
                        min = 0, step = 0.1, value = 0.5, max = 1),
                      numericInput(inputId = "gbox2_sub_hjust", label = "Subtitle:",
                        min = 0, step = 0.1, value = 0.5, max = 1),
                      numericInput(inputId = "gbox2_xlab_hjust", label = "X Axis Label:",
                        min = 0, step = 0.1, value = 0.5, max = 1),
                      numericInput(inputId = "gbox2_ylab_hjust", label = "Y Axis Label:",
                        min = 0, step = 0.1, value = 0.5, max = 1)
                    )
                  )
                ),
                tabPanel('Vertical',
                  fluidRow(
                    column(6,
                      numericInput(inputId = "gbox2_title_vjust", label = "Title:",
                        min = 0, step = 0.1, value = 0.5, max = 1),
                      numericInput(inputId = "gbox2_sub_vjust", label = "Subtitle:",
                        min = 0, step = 0.1, value = 0.5, max = 1),
                      numericInput(inputId = "gbox2_xlab_vjust", label = "X Axis Label:",
                        min = 0, step = 0.1, value = 0.5, max = 1),
                      numericInput(inputId = "gbox2_ylab_vjust", label = "Y Axis Label:",
                        min = 0, step = 0.1, value = 0.5, max = 1)
                    )
                  )
                )
              )
            ),

            column(8,
              plotOutput('gbox2_plot_6', height = '600px')
            )
          ),

          tabPanel('Theme',
            column(2,
              selectInput(inputId = 'gbox2_theme', label = 'Theme',
                choices = list("Classic Dark", "Default", "Light", "Minimal", 
                  "Dark", "Classic", "Empty"), selected = "Default")
            ),
            column(2),
            column(8, align = 'center',
              plotOutput('gbox2_plot_7', height = '600px')
            )
          ),

          tabPanel('Plot',
            column(12, align = 'center',
              plotOutput('gbox2_plot_8', height = '600px')
            )
          )         
                              
        )
      )
    )

  )

)