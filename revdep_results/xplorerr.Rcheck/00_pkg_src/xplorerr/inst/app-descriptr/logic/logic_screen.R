# output
output$screen <- renderPrint({
    ds_screener(filt_data$p)
})

observeEvent(input$finalok, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_eda')
	# updateNavlistPanel(session, 'navlist_eda', 'tab_eda')
})

final_split <- reactiveValues(train = NULL)

observeEvent(input$finalok, {
	final_split$train <- filt_data$p
})