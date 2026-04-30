tabPanel('Bivariate Analysis', value = 'tab_bivar_analysis',

  fluidPage(
    
    fluidRow(
      
      column(6, align = 'left',
        h4('Bivariate Analysis')
      ),
      column(6, align = 'right',
        actionButton(inputId='bivarlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_bivariate_analysis.html', '_blank')")
      )
      
    ),
    
    hr(),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Response Variable:')),
      column(4, align = 'left',
             selectInput("resp_bivar", label = '', 
                         choices = '', selected = '')),
      bsTooltip("resp_bivar", "Select response variable.",
                "left", options = list(container = "body")))
      
    ),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Variables:')),
      column(10, align = 'left',
             selectInput("var_bivar", label = '', width = '660px',
                         choices = "", selected = "", multiple = TRUE,
                         selectize = TRUE),
             bsTooltip("var_bivar", "Select variables.",
                       "left", options = list(container = "body")))
      
    ),
    
    fluidRow(
      column(12, align = 'center',
             br(),
             br(),
             actionButton(inputId = 'submit_bivar', label = 'Submit', width = '120px', icon = icon('check')),
             bsTooltip("submit_bivar", "Click here to view bivariate analysis.",
                       "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
      br(),
      column(12, align = 'center',
             verbatimTextOutput('bivar_out')
      )
    )
    
  )  

