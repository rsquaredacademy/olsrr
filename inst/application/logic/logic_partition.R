partition_ui <- eventReactive(input$button_split_yes, {

	fluidRow(

		column(12, align = 'center',
			numericInput(
				inputId = 'part_train_per',
				label = 'Training Set',
				min = 0,
				max = 1, 
				value = 0.5,
				step = 0.01,
				width = '120px'
			)
		),

		column(12, align = 'center',
			br(),
			actionButton(inputId = 'submit_part_train_per', label = 'Partition', width = '120px', icon = icon('check')),
        bsTooltip("submit_part_train_per", "Click here to partition data.",
                      "bottom", options = list(container = "body"))
		)

	)

})

output$ui_partition <- renderUI({
	partition_ui()
})

final_split <- reactiveValues(train = NULL, test = NULL)

trainpart <- eventReactive(input$button_split_yes, {

	out <- createDataPartition(
	  y = final_sample$d[[1]],
	  p = input$part_train_per,
	  list = FALSE
	)

	as.vector(out)

})

# train_data <- eventReactive(input$button_split_yes, {
# 	training <- final_sample$d[trainpart(), ]
# })

# test_data <- eventReactive(input$button_split_yes, {
# 	testing <- final_sample$d[-trainpart(), ]
# })

observeEvent(input$submit_part_train_per, {
	final_split$train <- final_sample$d[trainpart(), ]
	final_split$test <- final_sample$d[-trainpart(), ]
})

# observeEvent(input$button_split_yes, {
# 	final_split$test <- test_data()	
# })

observeEvent(input$button_split_no, {
	final_split$train <- final_sample$d	
})

observeEvent(input$submit_part_train_per, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_analyze_home')
})

observeEvent(input$button_split_no, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_analyze_home')
})