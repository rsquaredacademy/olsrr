tabPanel('WoE & IV', value = 'tab_woe_iv',

	fluidPage(
    
    fluidRow(
      
      column(6, align = 'left',
        h4('Weight of Evidence & Information Value')
      ),
      column(6, align = 'right',
        actionButton(inputId='woelink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_woe_iv.html', '_blank')")
      )
      
    ),
    
    hr(),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Response Variable:')),
      column(4, align = 'left',
             selectInput("resp_woe", label = '', 
                         choices = '', selected = '')),
      bsTooltip("resp_woe", "Select response variable.",
                "left", options = list(container = "body"))
      
    ),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Predictor Variable:')),
      column(10, align = 'left',
             selectInput("var_woe", label = '', 
                         choices = "", selected = ""),
             bsTooltip("var_woe", "Select predictor.",
                       "left", options = list(container = "body")))
      
    ),
    
    fluidRow(
      column(12, align = 'center',
             br(),
             br(),
             actionButton(inputId = 'submit_woe', label = 'Submit', width = '120px', icon = icon('check')),
             bsTooltip("submit_bivar", "Click here to view WoE & IV.",
                       "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
      br(),
      column(12, align = 'center',
             verbatimTextOutput('woe_out')
      )
    )

  )  

)