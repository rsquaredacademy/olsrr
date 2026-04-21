tabPanel('Segment Distribution', value = 'tab_segment_dist',

	fluidPage(
    
    fluidRow(
      
      column(6, align = 'left',
        h4('Segment Distribution')
      ),
      column(6, align = 'right',
        actionButton(inputId='segdistlink1', label="Help", icon = icon("question-circle"),
          onclick ="window.open('https://blorr.rsquaredacademy.com/reference/blr_segment_dist.html', '_blank')")
      )
      
    ),
    
    hr(),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Response Variable:')),
      column(4, align = 'left',
             selectInput("resp_segdist", label = '', 
                         choices = '', selected = '')),
      bsTooltip("resp_segdist", "Select response variable.",
                "left", options = list(container = "body"))
      
    ),
    
    fluidRow(
      
      column(2, align = 'right', br(), h5('Predictor Variable:')),
      column(10, align = 'left',
             selectInput("var_segdist", label = '', 
                         choices = "", selected = ""),
             bsTooltip("var_segdist", "Select predictor.",
                       "left", options = list(container = "body")))
      
    ),
    
    fluidRow(
      column(12, align = 'center',
             br(),
             br(),
             actionButton(inputId = 'submit_segdist', label = 'Submit', width = '120px', icon = icon('check')),
             bsTooltip("submit_segdist", "Click here to view segment distribution.",
                       "bottom", options = list(container = "body")))
    ),
    
    fluidRow(
      br(),
      column(12, align = 'center',
             verbatimTextOutput('segdist_out')
      )
    ),

    fluidRow(
      br(),
      column(12, align = 'center',
             plotOutput('segdist_plot', height = "500px", width = "75%")
      )
    )    

  )

)