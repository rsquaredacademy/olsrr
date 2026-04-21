tabPanel('WoE & IV Stats', value = 'tab_woe_iv_stats',

	fluidPage(
    
    fluidRow(
      
      column(6, align = 'left',
        h4('WoE & IV Stats')
      ),
      column(6, align = 'right',
        actionButton(inputId='woe2link1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_woe_iv_stats.html', '_blank')")
      )
      
    ),
    
    hr(),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Response Variable:')),
      column(4, align = 'left',
             selectInput("resp_woe2", label = '', 
                         choices = '', selected = '')),
      bsTooltip("resp_woe2", "Select response variable.",
                "left", options = list(container = "body")))
      
    ),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Predictor Variables:')),
      column(10, align = 'left',
             selectInput("var_woe2", label = '', width = '660px',
                         choices = "", selected = "", multiple = TRUE,
                         selectize = TRUE),
             bsTooltip("var_woe2", "Select variables.",
                       "left", options = list(container = "body")))
      
    ),
    
    fluidRow(
      column(12, align = 'center',
             br(),
             br(),
             actionButton(inputId = 'submit_woe2', label = 'Submit', width = '120px', icon = icon('check')),
             bsTooltip("submit_woe2", "Click here to view WoE & IV.",
                       "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
      br(),
      column(12, align = 'center',
             verbatimTextOutput('woe2_out')
      )
    )


)