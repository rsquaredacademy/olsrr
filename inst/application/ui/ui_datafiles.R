tabPanel('Upload File', value = 'tab_uploadfile',

	fluidPage(

		includeCSS("mystyle.css"),

		fluidRow(

			column(12,

				tabsetPanel(type = 'tabs', id = 'tabset_upload',

					tabPanel('CSV', value = 'tab_upload_csv',

						fluidPage(

							br(),

			        fluidRow(
			          column(8, align = 'left',
			            h4('Upload Data'),
			            p('Upload data from a comma or tab separated file.')
			          ),
			          column(4, align = 'right',
			            actionButton(inputId='uploadlink2', label="Demo", icon = icon("video-camera"),
			              onclick ="window.open('http://google.com', '_blank')")
			          )
			        ),
			        hr(),

			            fluidRow(
			                column(12, align = 'center',
			                    fileInput('file1', 'Data Set:',
			                        accept = c('text/csv', '.csv',
			                            'text/comma-separated-values,text/plain')
			                    )
			                )
			            ),

			            fluidRow(
			                column(12, align = 'center',  checkboxInput('header', 'Header', TRUE))
			            ),

			            fluidRow(
			                column(12, align = 'center',
			                    selectInput('sep', 'Separator',
			                        choices = c('Comma' = ',', 'Semicolon' = ';', 'Tab' = '\t'), selected = ',')
			                )
			            ),

			            fluidRow(
			                column(12, align = 'center',
			                    selectInput('quote', 'Quote',
			                        choices = c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'"), selected = '')
			                )
			            ),

			            br(),
			            br(),
			            br(),

			            fluidRow(
			            	
			            	column(6, align = 'left',
			            		actionButton(inputId='csv2datasrc', label="Data Sources", icon = icon("long-arrow-left"))
			            	),

			            	column(6, align = 'right',
			            		actionButton(inputId='csv2datatrans', label="Data Selection", icon = icon("long-arrow-right"))
			            	)

			            )

			        )

					),
					tabPanel('Excel', value = 'tab_upload_excel',

						fluidPage(

							br(),

			        fluidRow(
			          column(8, align = 'left',
			            h4('Upload Data'),
			            p('Upload data from a .xls or .xlsx file.')
			          ),
			          column(4, align = 'right',
			            actionButton(inputId='uploadexcel1', label="Help", icon = icon("question-circle")),
			            actionButton(inputId='uploadexcel2', label="Demo", icon = icon("video-camera"),
			              onclick ="window.open('http://google.com', '_blank')")
			          )
			        ),
			        hr(),

			            fluidRow(
			                column(12, align = 'center',
			                    fileInput(
			                      inputId = 'file2',
			                      label = 'Choose file:',
			                      accept = c('.xls', '.xlsx')
			                    )
			                )
			            ),

			            fluidRow(
			                column(12, align = 'center',  
			                  numericInput(
			                    inputId = 'sheet_n',
			                    label = 'Sheet',
			                    value = 1,
			                    min = 1,
			                    step = 1,
			                    width = '120px'
			                  )  
			                )
			            ),

			            br(),
			            br(),

			            br(),
			            br(),

			            br(),
			            br(),
			            br(),
			            br(),
			            br(),

			            fluidRow(
			            	
			            	column(6, align = 'left',
			            		actionButton(inputId='excel2datasrc', label="Data Sources", icon = icon("long-arrow-left"))
			            	),

			            	column(6, align = 'right',
			            		actionButton(inputId='excel2datatrans', label="Data Selection", icon = icon("long-arrow-right"))
			            	)

			            )

			        )

					),
					tabPanel('JSON', value = 'tab_upload_json',

						br(),

						fluidPage(

			        fluidRow(
			          column(8, align = 'left',
			            h4('Upload Data'),
			            p('Upload data from a .json file.')
			          ),
			          column(4, align = 'right',
			            actionButton(inputId='uploadjson1', label="Help", icon = icon("question-circle")),
			            actionButton(inputId='uploadjson2', label="Demo", icon = icon("video-camera"),
			              onclick ="window.open('http://google.com', '_blank')")
			          )
			        ),
			        hr(),

			            fluidRow(
			                column(12, align = 'center',
			                    fileInput(
			                      inputId = 'file3',
			                      label = 'Choose file:',
			                      accept = '.json'
			                    )
			                )
			            ),

			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),

			            fluidRow(
			            	
			            	column(6, align = 'left',
			            		actionButton(inputId='json2datasrc', label="Data Sources", icon = icon("long-arrow-left"))
			            	),

			            	column(6, align = 'right',
			            		actionButton(inputId='json2datatrans', label="Data Selection", icon = icon("long-arrow-right"))
			            	)

			            )

			        )

					),
					tabPanel('STATA', value = 'tab_upload_stata',

						br(),

						fluidPage(

			        fluidRow(
			          column(8, align = 'left',
			            h4('Upload Data'),
			            p('Upload data from a .json file.')
			          ),
			          column(4, align = 'right',
			            actionButton(inputId='uploadstata1', label="Help", icon = icon("question-circle")),
			            actionButton(inputId='uploadstata2', label="Demo", icon = icon("video-camera"),
			              onclick ="window.open('http://google.com', '_blank')")
			          )
			        ),
			        hr(),

			            fluidRow(
			                column(12, align = 'center',
			                    fileInput(
			                      inputId = 'file6',
			                      label = 'Choose file:',
			                      accept = '.dta'
			                    )
			                )
			            ),

			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),

			            fluidRow(
			            	
			            	column(6, align = 'left',
			            		actionButton(inputId='stata2datasrc', label="Data Sources", icon = icon("long-arrow-left"))
			            	),

			            	column(6, align = 'right',
			            		actionButton(inputId='stata2datatrans', label="Data Selection", icon = icon("long-arrow-right"))
			            	)

			            )

			        )

					),
					tabPanel('SPSS', value = 'tab_upload_spss',

						br(),

						fluidPage(

			        fluidRow(
			          column(8, align = 'left',
			            h4('Upload Data'),
			            p('Upload data from a .sav file.')
			          ),
			          column(4, align = 'right',
			            actionButton(inputId='uploadspss1', label="Help", icon = icon("question-circle")),
			            actionButton(inputId='uploadspss2', label="Demo", icon = icon("video-camera"),
			              onclick ="window.open('http://google.com', '_blank')")
			          )
			        ),
			        hr(),

			            fluidRow(
			                column(12, align = 'center',
			                    fileInput(
			                      inputId = 'file5',
			                      label = 'Choose file:',
			                      accept = '.sav'
			                    )
			                )
			            ),

			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),

			            fluidRow(
			            	
			            	column(6, align = 'left',
			            		actionButton(inputId='spss2datasrc', label="Data Sources", icon = icon("long-arrow-left"))
			            	),

			            	column(6, align = 'right',
			            		actionButton(inputId='spss2datatrans', label="Data Selection", icon = icon("long-arrow-right"))
			            	)

			            )

			        )

					),
					tabPanel('SAS', value = 'tab_upload_sas',

						br(),

						fluidPage(

			        fluidRow(
			          column(8, align = 'left',
			            h4('Upload Data'),
			            p('Upload data from a .sas7bdat file.')
			          ),
			          column(4, align = 'right',
			            actionButton(inputId='uploadsas1', label="Help", icon = icon("question-circle")),
			            actionButton(inputId='uploadsas2', label="Demo", icon = icon("video-camera"),
			              onclick ="window.open('http://google.com', '_blank')")
			          )
			        ),
			        hr(),

			            fluidRow(
			                column(12, align = 'center',
			                    fileInput(
			                      inputId = 'file4',
			                      label = 'Choose file:',
			                      accept = '.sas7bdat'
			                    )
			                )
			            ),

			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),
			            br(),

			            fluidRow(
			            	
			            	column(6, align = 'left',
			            		actionButton(inputId='sas2datasrc', label="Data Sources", icon = icon("long-arrow-left"))
			            	),

			            	column(6, align = 'right',
			            		actionButton(inputId='sas2datatrans', label="Data Selection", icon = icon("long-arrow-right"))
			            	)

			            )

			        )
						
					)

				)

			)

		)

	)

)