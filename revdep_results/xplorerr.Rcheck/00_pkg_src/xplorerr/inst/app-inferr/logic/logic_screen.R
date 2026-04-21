# output
output$screen <- renderPrint({
    ds_screener(filt_data$p)
})

observeEvent(input$finalok, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_infer_home')
})

final_split <- reactiveValues(train = NULL)

observeEvent(input$finalok, {
	final_split$train <- filt_data$p
})