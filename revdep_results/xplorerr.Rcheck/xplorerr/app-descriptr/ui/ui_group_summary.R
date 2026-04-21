tabPanel('Group Summary', value = 'tab_gsummary',

         fluidPage(

           fluidRow(
             column(8, align = 'left',
               h4('Group Summary Statistics'),
               p('Generates descriptive statistics of a continuous variable for
                the different levels of a categorical variable.')
             ),
             column(4, align = 'right',
               actionButton(inputId='gstatlink1', label="Help", icon = icon("question-circle"),
                 onclick ="window.open('https://descriptr.rsquaredacademy.com/reference/ds_group_summary.html', '_blank')"),
               actionButton(inputId='gstatlink3', label="Demo", icon = icon("video-camera"),
                 onclick ="window.open('https://www.youtube.com/watch?v=7kvasytLmmk', '_blank')")
             )
           ),
           hr(),

             fluidRow(

                column(2, align = 'right', br(), h5('Grouping Variable: ')),

                column(2, align = 'left',

                    selectInput("var_group", label = '', width = '150px',
                                choices = "", selected = ""
                    ),
                    bsTooltip("var_group", "Click here to select a grouping variable.",
                              "left", options = list(container = "body"))

                ),

                column(2, align = 'right', br(), h5('Summary Variable: ')),

                column(2, align = 'left',

                    selectInput("var_grp_summary", label = '', width = '150px',
                                choices = "", selected = ""
                    ),
                    bsTooltip("var_grp_summary", "Click here to select a continuous variable.",
                              "top", options = list(container = "body"))

                ),

                column(4, align = 'center',

                br(),

                actionButton(inputId = 'submit_gsummary', label = 'Submit', width = '150px'),
                bsTooltip("submit_gsummary", "Click here to view group summary.",
                              "top", options = list(container = "body"))

                )

            ),

            fluidRow(

                column(12, align = 'center',

                    verbatimTextOutput('group_summary')

                )

             ),

            fluidRow(

                column(12, align = 'center',

                    # h3('Box Plot'),
                    uiOutput('group1_title'),
                    plotOutput('box_group_summary', height = "500px", width = "75%")

                )

            )

         )

)
