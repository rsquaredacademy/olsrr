# Exit -----------------------------------------------------------
tabPanel("", value = "exit",  icon = icon("power-off"),
         br(),
         br(),
         br(),
         br(),
         br(),
         br(),
         # In case window does not close, one should see this message
         fluidRow(column(3),
                  column(6, h2("Thank you for using", strong("xplorerr"), "!"))),
         fluidRow(column(3),
                  column(6, h4("Now you should close this window.")))
)

