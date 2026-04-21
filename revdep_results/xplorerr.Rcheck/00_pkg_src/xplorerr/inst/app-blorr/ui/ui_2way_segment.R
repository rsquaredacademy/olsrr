tabPanel('Two Way Segmentation', value = 'tab_2way_segment',

	fluidPage(
    
    fluidRow(
      
      column(6, align = 'left',
        h4('2 Way Segmentation')
      ),
      column(6, align = 'right',
        actionButton(inputId='2wayseglink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_twoway_segment.html', '_blank')")
      )
      
    ),
    
    hr(),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Response Variable:')),
      column(4, align = 'left',
             selectInput("resp_2wayseg", label = '', 
                         choices = '', selected = '')),
      bsTooltip("resp_2wayseg", "Select response variable.",
                "left", options = list(container = "body")))
      
    ),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Variable 1:')),
      column(10, align = 'left',
             selectInput("var1_2wayseg", label = '', width = '660px',
                         choices = "", selected = ""),
             bsTooltip("var1_2wayseg", "Select variable",
                       "left", options = list(container = "body")))
    ),

    fluidRow(
      
      column(2, align = 'right', br(), h5('Variable 2:')),
      column(10, align = 'left',
             selectInput("var2_2wayseg", label = '', width = '660px',
                         choices = "", selected = ""),
             bsTooltip("var2_2wayseg", "Select variable",
                       "left", options = list(container = "body")))
      
    ),
    
    fluidRow(
      column(12, align = 'center',
             br(),
             br(),
             actionButton(inputId = 'submit_2wayseg', label = 'Submit', width = '120px', icon = icon('check')),
             bsTooltip("submit_2wayseg", "Click here to view two way segmentation.",
                       "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
      br(),
      column(12, align = 'center',
             verbatimTextOutput('twowayseg_out')
      )
    )


)