tabPanel('Transform', value = 'tab_transform',

             fluidPage(

               fluidRow(
                 column(6, align = 'left',
                   h4('Data Transformation'),
                   p('Rename variables and modify data types.')
                 ),
                 column(6, align = 'right',
                   actionButton(inputId='translink3', label="Demo", icon = icon("video-camera"),
                     onclick ="window.open('http://google.com', '_blank')")
                 )
               ),
               hr(),
               fluidRow(
                 column(3, tags$h5('Variable')),
                 column(3, tags$h5('Rename Variable')),
                 column(3, tags$h5('Modify Data Type'))
               ),

                 column(12, uiOutput('trans_try')),

                 fluidRow(
                     tags$br()
                 ),

                 fluidRow(

                     column(12, align = 'center',
                            br(),
                            actionButton(inputId="apply_changes", label="Apply Changes", icon = icon('thumbs-up')),
                            bsTooltip("apply_changes", "Click here to apply changes to data.",
                              "top", options = list(container = "body")),
                            br(),
                            br()
                     )
                 )

    )

)
